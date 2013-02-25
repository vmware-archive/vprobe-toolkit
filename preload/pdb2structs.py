#!/usr/bin/python

# pdb2structs.py
#
# A utility that extracts structures from a .pdb file (Windows debugging
# information). Requires that you have "PDBParse" (available at
# http://code.google.com/p/pdbparse)
#
# pdb2structs.py accepts a .pdb file (Windows debugging information) and a list
# of structure fields, and outputs an Emmett file that defines requested
# structures and fields within the structures. A user can request all fields in
# a structure, or select just a handful of them. The generated file will use
# the proper offsets to make an Emmett sparse structure.
#
# Usage: pdb2structs.py debuginfo.pdb < fields-needed.txt
#
# Examples:
# - Extract all members of the UNICODE_STRING structure:
#   echo '_UNICODE_STRING.*' | pdb2structs.py ntkrnlmp.pdb
#
# - Extract the CurrentThread pointer offset from the KPRCB structure:
#   echo '_KPRCB.CurrentThread' | pdb2structs.py ntkrnlmp.pdb
#
# - Extract the Tcb, Pcb, and ImageFileName from the ETHREAD and EPROCESS
#   printf '_ETHREAD.Tcb\n_EPROCESS.Pcb\n_EPROCESS.ImageFileName' \
#     | pdb2structs.py ntkrnlmp.pdb
#
# (To extract fields from multiple structures and/or multiple fields from one
# structure, just put one entry per line.)

# TODO:
# * Add support for typedefs, enums, unions, and bitfields
# * Add support for dumping all types in a .pdb file, and loading them back as
#   an Emmett type database
# * Correctly handle nested structs --- struct-in-union-in-struct gets
#   gets flattened to a struct, and Emmett won't handle it because it has non-
#   monotonic offsets
#
# examples/print_ctypes.py in the pdbparse distribution may be helpful, as a
# starting ground for a replacement for this script, or for guidance on how to
# use pdbparse.
#
# Re todo 3: It appears that print_ctypes.py interprets the existence of
# structures and unions under certain circumstances (see
# member_list_from_offset_map). We could probably just have multiple members
# with the safe offset (given an Emmett change to eliminate the "Non-monotonic
# offset for field" error), or also imagine structs and unions not in the .pdb
# file.
#
# Modifying print_ctypes.py to print Emmett type definitions also looks pretty
# doable. It seems somewhat plausible that the upstream maintainer would accept
# a patch to making hooking in an Emmett theme easy (or even ship with the
# Emmett theme), if we wanted to pursue that. (He's accepted other third-party
# patches without much hassling.)

import collections
import sys

import pdbparse

primitiveTypes = {
   "VOID":    "void",
   "CHAR":    "char",
   "SHORT":   "short",
   "LONG":    "long",
   "QUAD":    "long long",
}

def canonicalizeType(typeRef):
   """Convert a type from pdbparse into an Emmett type

      @param typeRef: a string or pdbparse structure representing the type
      @return: a (typeName, prefix, suffix) describing the type for Emmett
   """
   prefix = ""
   suffix = ""
   if type(typeRef) == str:
      assert typeRef.startswith("T_"), \
         "Primitive types should start with T_; got '%s'" % (typeRef, )
      typeRef = typeRef[2:]
      if typeRef.startswith("64P"):
         prefix = "*"
         typeRef = typeRef[3:]
      if typeRef.startswith("U"):
         unsigned = True
         typeRef = typeRef[1:]
      else:
         unsigned = False
      assert typeRef in primitiveTypes, "Unknown primitive type %s" % (typeRef, )
      typeName = primitiveTypes[typeRef]
      if unsigned: typeName = "unsigned %s" % (typeName, )
   else:
      if typeRef.leaf_type == 'LF_POINTER':
         typeName, prefix, suffix = canonicalizeType(typeRef.utype)
         prefix = "*" + prefix
      elif typeRef.leaf_type == 'LF_ARRAY':
         typeName, prefix, suffix = canonicalizeType(typeRef.element_type)
         if suffix != "":
            raise NotImplementedError, \
               "Multi-dimensional arrays are not currently supported."
         suffix = "[%d]" % (typeRef.size, )
      elif typeRef.leaf_type == 'LF_STRUCTURE':
         typeName = "struct %s" % (typeRef.name, )
      elif typeRef.leaf_type == 'LF_MODIFIER':
         typeName, prefix, suffix = canonicalizeType(typeRef.modified_type)
         if typeRef.modifier['const']: typeName = "const %s" % (typeName, )
      else:
         raise NotImplementedError, "Unknown leaf type %s" % (typeRef.leaf_type, )

   return typeName, prefix, suffix

def dictifySubstructs(substructs):
   """Convert a sequence of structures to a dictionary

      @param substructs: a sequence of structures
      @return: a dictionary with the same values, keyed by name
   """
   ret = {}
   for struct in substructs:
      ret[struct.name] = struct
   return ret

def formatFieldspec(fieldspec):
   """Format a single field of a structure

      @param fieldspec: pdbparse's description of the field to be formatted
      @return: string representation of the desired field
   """
   fieldOffset = fieldspec.offset
   fieldType, fieldPrefix, fieldSuffix = canonicalizeType(fieldspec.index)
   fieldName = fieldspec.name
   return "   @0x%03x %-19s %1s%s%s;" % (
      fieldOffset, fieldType, fieldPrefix, fieldName, fieldSuffix,
   )

def formatStruct(structInfo, fields):
   """Format the structure provided with the given fields.

      @param structInfo: pdbparse's representation of a single structure
      @param fields: requested fields (as strings with just the fieldname)
      @return: string representation of the desired structure
   """
   name = structInfo.name
   substructs = dictifySubstructs(structInfo.fieldlist.substructs)
   if '*' in fields:
      fields = [struct.name for struct in structInfo.fieldlist.substructs]
   fieldSpecs = [formatFieldspec(substructs[field]) for field in fields]
   return "struct %s {\n%s\n};\n" % (name, "\n".join(fieldSpecs), )

def formatStructs(pdbfile, fields):
   """Format the requested structures from a given .pdb file.

      @param pdbfile: name of .pdb file
      @param fields: requested fields (as struct.field strings)
      @return: string representation of the desired structures
   """
   pdb = pdbparse.parse(pdbfile)

   structs = collections.defaultdict(list)
   for fieldspec in fields:
      struct, dot, field = fieldspec.strip().partition('.')
      if field == "":
         if struct not in structs:
            structs[struct] = list()
      else:
         structs[struct].append(field)

   pdbStructs = pdb.STREAM_TPI.structures
   structSpecs = [
         formatStruct(pdbStructs[structName], structFields)
         for structName, structFields in structs.items()
   ]
   return "\n\n".join(structSpecs)

if __name__ == '__main__':
   if len(sys.argv) != 2:
      print "Usage: %s debuginfo.pdb < fields-needed.txt" % (sys.argv[0], )
      sys.exit(1)
   print formatStructs(sys.argv[1], sys.stdin.readlines())
