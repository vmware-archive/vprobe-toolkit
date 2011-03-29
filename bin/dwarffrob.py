#!/usr/bin/python

# Copyright 2008, VMware, Inc. All rights reserved.

import re, os, sys

# A "Type" is a type as dwarf and emmett conceive them. It's subclassed for
# structs, arrays, etc.
class Type:
    # Nothin'. Maybe. I dunno...
    def __init__ (self, size):
        self.size = size
    def toString (self):
        return "uint" + str(8 * self.size)
    def toStringFlat(self):
        return self.toString()

# Struct contain type/id pairs at offsets. We reuse this with offsets of
# zero to represent unions.
class Member:
    def __init__ (self, type, offset):
        self.type = type
        self.offset = offset
        self.size = 0
    def toString(self):
        s = self.type.toStringFlat()
        s += "@ " + str(self.offset)
        return s

def indent(str):
    return "".join(map ((lambda x: "    " + x + "\n"), str.split("\n")))

class Struct(Type):
    # Member: a field in this struct
    def __init__ (self):
        self.members = { } # an association of names -> members
    def addMember(self, name, type, offset):
        self.members[name] = Member(type, offset)
    def toString (self):
        s = "struct { \n"
        sub = ""
        for mem in self.members.keys():
            sub += self.members[mem].toString()
            sub += " " + mem
            sub += ";\n"
        s += indent(sub)
        s += "}"
        return s

class Enum(Type):
    def __init__ (self):
        self.members = { }
    def addEnum (self, name, value):
        self.members[name] = value
    def toString(self):
        s = "enum {\n"
        sub = ""
        for k in self.members.keys():
            sub += k + " = " + str(self.members[k]) + ",\n"
        s += indent(sub) + "}\n"
        return s

# Arrays are all one dimensional. Multidimensional arrays are just piles of
# recursively defined arrays.
class Array(Type):
    def __init__ (self):
        self.dim = None
        self.type = None
    def printDim(self):
        if self.dim:
            return str(self.dim)
        else:
            return []
    def toString(self):
        return self.type.toStringFlat() + "[" + self.printDim() + "]"

class Typedef(Type):
    def __init__ (self):
        self.name = None
        self.type = None
    def toStringFlat(self):
        return self.name
    def toString(self):
        str = "typedef " + self.type.toString() + " " + self.name + ";\n"
        return str

class Pointer(Type):
    def __init__ (self):
        self.type = None
    def toString(self):
        try:
            return self.type.toStringFlat() + "*"
        except AttributeError:
            return "void*"

def readDwarf(filename):
    if 1 == 1:
        f = os.popen("readelf --debug-dump=info " + filename)
        return f.read()
    else:
        return sys.stdin.read()

class DwarfTag:
    def __init__ (self, type, id):
        self.type = type
        self.id = id
        self.attrs = { }
    def show(self):
        print " --- type: " + self.type + " id: " + self.id
        for k in self.attrs.keys():
            print "     " + k + ": " + str(self.attrs[k])

# typemap/typeIntToNames: type names to structure objects; type
# dwarf IDs to type names.

# Turn ascii dwarf tags into a set of DwarfTag objects, preserving
# order.
def dwarfLinesToTags(lines):
    dwarfTags=[ ]
    # tagRe matches output like this:
    #     <1><3b>: Abbrev Number: 2 (DW_TAG_base_type)
    bracketed = lambda x: "<" + x + ">"
    parened = lambda x: "(" + x + ")"
    hexNum = "[0-9a-f]*"
    brackhex = bracketed(hexNum)
    groupedHex = parened(bracketed(hexNum))
    pat = r"\s*" + groupedHex + groupedHex + ": Abbrev Number: " + \
          r"([0-9]*) \(DW_TAG_([a-z_0-9]*)\)"
    tagRe = re.compile(pat)
    # atRe matches attributes
    atRe = re.compile(r"\s*DW_AT_([\S]*)\s*: (.*\S*)\s*$")
    # uConstRe is for the specially handled attribute
    # "data_member_location"
    myUconstRe=re.compile(r"\(DW_OP_plus_uconst: ([0-9]*)")
    # indStringRe strips away the line noise for long names
    indStringRe = re.compile(r"\(indirect string, offset: 0x[0-9a-f]*\): (.*)")
    curTag = None

    for line in lines:
        # print "!!!" + line
        # If we see a struct, 
        m = tagRe.match(line)
        if m:
            [hex1, curTagId, abbr, curTagClass] = [m.group(1), m.group(2), \
                                                   m.group(3), m.group(4)]
            curTag = DwarfTag(curTagClass, curTagId)
            dwarfTags = dwarfTags + [curTag]
        elif curTag:
            m = atRe.search(line)
            if m:
                [attr, val] = [m.group(1), m.group(2)]
                # handle some attributes specially; decode the
                # data_member_location, and get rid of trailing whitespace
                # for all.
                m = indStringRe.search(val)
                if m:
                    val = m.group(1)
                else:
                    val = val.split()[0]
                if attr == "data_member_location":
                    m=myUconstRe.search(line)
                    if m:
                        val = int(m.group(1))
                curTag.attrs[attr] = val
    return dwarfTags

# Given an ordered set of dwarftags, return a mapping from dwarf IDs
# to Type objects.
def tagsToTypes(tags):
    dwarfIdsToTypes = { }
    def dwarfTypeNmToType(tag):
        typeNm = tag.type
        if typeNm == "base_type":
            return Type(int(tag.attrs["byte_size"]))
        if typeNm == "structure_type" or typeNm == "union_type":
            return Struct()
        if typeNm == "array_type":
            return Array()
        if typeNm == "pointer_type":
            return Pointer()
        if typeNm == "typedef":
            return Typedef()
        if typeNm == "enumeration_type":
            return Enum()
    print "  .a"

    # First pass: put all the "base types" into the type map. (We need to
    # do two passes, since forward references are possible.)
    for tag in tags:
        dwarfIdsToTypes[tag.id] = dwarfTypeNmToType(tag)

    print "  .b"
    # Second pass: all types exist. Flesh out the recursive types.
    # Some tags are spatially encoded, in that their appearance right
    # after a previous compound type indicates which type they're part
    # of (struct and union members, array boundaries). curCompound
    # remembers which type we're currently consuming.
    curCompound = None
    for tag in tags:
        def memToOff(tag):
            if tag.attrs.has_key("data_member_location"):
                return tag.attrs["data_member_location"]
            # union member fields don't have offsets; they all effectively
            # have offset 0
            return 0
        try:
            if tag.type == "structure_type" or tag.type == "union_type" or \
               tag.type == "array_type" or tag.type == "enumeration_type":
                curCompound = dwarfIdsToTypes[tag.id]
            if tag.type == "member":
                curCompound.addMember(tag.attrs["name"],
                                      dwarfIdsToTypes[tag.attrs["type"]],
                                      memToOff(tag))
            if tag.type == "enumerator":
                curCompound.addEnum(tag.attrs["name"],
                                    int(tag.attrs["const_value"]))
            if (tag.type == "array_type" or tag.type == "typedef" or \
                tag.type == "pointer") and tag.attrs.has_key("type"):
                tp = dwarfIdsToTypes[tag.id]
                tp.type = dwarfIdsToTypes[tag.attrs["type"]]
                if tag.type == "typedef":
                    tp.name = tag.attrs["name"]
            if tag.type == "subrange_type" and \
                 tag.attrs.has_key("upper_bound"):
                curCompound.dim = 1 + int(tag.attrs["upper_bound"])
        except AttributeError:
            print "attempt to construct recursive type failed!"
            tag.show()
            sys.exit(1)
    print "  .c"
    return dwarfIdsToTypes

# Given an ordered set of dwarfTags, return a mapping from human-readable
# type names to Type objects.
def tagsToTypeNames(tags, dwarfIdsToTypes):
    namesToTypes = { }
    for tag in tags:
        # Don't bother explicitly inserting base types. If they're used,
        # emit them explicitly. Only typedefs? And, like, variables?
        try:
            if tag.type == "typedef":
                namesToTypes[tag.attrs["name"]] = dwarfIdsToTypes[tag.attrs["type"]]
        except KeyError:
            foo = 0
            # just keep going ...
            #tag.show()

    return namesToTypes

def main(argv=None):
    dwarfLines=readDwarf(argv[1])
    print 1
    tags = dwarfLinesToTags(dwarfLines.split("\n"))
    print 2
    typeMap = tagsToTypes(tags)
    print 3
    namesToTypes = tagsToTypeNames(tags, typeMap)
    print 4
    namesToTypes = tagsToTypeNames(tags, typeMap)
    print namesToTypes
    for k in namesToTypes.keys():
        try:
            print "typedef " + namesToTypes[k].toString() + " " + k + ";"
        except AttributeError:
            print "attrError" + k
main(sys.argv)
