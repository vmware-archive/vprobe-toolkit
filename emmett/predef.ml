(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Predef module: pre-defined types, functions, and variables.
 *)

open Ast
open Symtab

(*
 * Predefined types, functions, and variables.
 *)

let predefTypes = [
  (* The memmodel is irrelevant as long as we match the desired int size. *)
  ("int8",    TypeInt("vmw64", "char"));
  ("int16",   TypeInt("vmw64", "short"));
  ("int32",   TypeInt("vmw64", "int"));
  ("int64",   TypeInt("vmw64", "long"));
  ("uint8",   TypeInt("vmw64", "char"));
  ("uint16",  TypeInt("vmw64", "short"));
  ("uint32",  TypeInt("vmw64", "int"));
  ("uint64",  TypeInt("vmw64", "long"));
]

let typeInt = TypeInt("vmw64", "long")

let predefFuncs = [
  (* Supported builtins *)
  ("printf",        TypeVoid,   [TypeString; TypeVarArgs]);
  ("sprintf",       TypeVoid,   [TypeString; TypeString; TypeVarArgs]);
  ("exit",          TypeVoid,   []);
  ("logaggr",       TypeVoid,   [TypeVarArgs]);
  ("clearaggr",     TypeVoid,   [TypeVarArgs]);
  ("gueststack",    TypeVoid,   [TypeString; TypeVarArgs]);
  ("getguest",      typeInt,    [typeInt]);
  ("getguestphys",  typeInt,    [typeInt]);
  ("getgueststr",   TypeVoid,   [TypeString; typeInt; typeInt]);
  ("getsystemtime", TypeVoid,   [typeInt]);

  (* Unsupported builtins *)
  ("vmwstack",      TypeVoid,   [TypeString; TypeVarArgs]);
  ("getvmw",        typeInt,    [typeInt]);
  ("getvmwstr",     TypeVoid,   [TypeString; typeInt; typeInt]);
  ("readpmc",       typeInt,    [typeInt]);
  ("readmsr",       typeInt,    [typeInt]);
  ("getmeminfo",    typeInt,    []);
  ("offatret",      typeInt,    [typeInt]);
  ("offatstrcpy",   typeInt,    [typeInt; typeInt]);
  ("offatseg",      typeInt,    [typeInt]);
]

let predefIntVars = [
  (* Globals from vprobeVMMVMX.h *)
  "ISVMM32";
  "ISVMM64";
  "CR0";
  "CR3";
  "CR4";
  "KERNELGSBASE";
  "CS";
  "CSAR";
  "CSLIMIT";
  "CSBASE";
  "SS";
  "SSAR";
  "SSLIMIT";
  "SSBASE";
  "DS";
  "DSAR";
  "DSLIMIT";
  "DSBASE";
  "ES";
  "ESAR";
  "ESLIMIT";
  "ESBASE";
  "FS";
  "FSAR";
  "FSLIMIT";
  "FSBASE";
  "GS";
  "GSAR";
  "GSLIMIT";
  "GSBASE";
  "TR";
  "TRAR";
  "TRLIMIT";
  "TRBASE";
  "LDTR";
  "LDTRAR";
  "LDTRLIMIT";
  "LDTRBASE";
  "GDTRLIMIT";
  "GDTRBASE";
  "IDTRLIMIT";
  "IDTRBASE";
  "VMCS_EXIT_REASON";
  "VMCS_EXIT_INTR_INFO";
  "VMCS_EXIT_INTR_ERR";
  "VMCS_INSTRLEN";
  "VMCS_EXIT_QUAL";
  "VMCS_IDTVEC_INFO";
  "VMCS_IDTVEC_ERR";
  "VMCS_VMENTRY_INTR_INFO";
  "VMCS_VMENTRY_XCP_ERR";
  "VMCB_EXITCODE";
  "VMCB_EXITINFO1";
  "VMCB_EXITINFO2";
  "VMCB_EXITINTINFO";
  "VMCB_EVENTINJ";
  "VMCB_TLBCTL";
  "VMCB_VAPIC";
  "RAX";
  "RBX";
  "RCX";
  "RDX";
  "RSI";
  "RDI";
  "RSP";
  "RBP";
  "R8";
  "R9";
  "R10";
  "R11";
  "R12";
  "R13";
  "R14";
  "R15";
  "TSC";
  "TSC_HZ";
  "PCPUID";
  "NUMVCPUS";
  "THREADID";
  "BRCNT";
  "VCPUID";
  "RIP";
  "CR2";
  "CR8";
  "DR6";
  "DR7";
  "SMM";
  "EFER";
  "SELSTATE";
  "EMULATION_MODE";
  "RFLAGS";
  "APIC_BASEPA";
  "APICREGS";
  "RDTSC";
  "MEMSCHED_ADDR";

  (* Globals from vmkProbes.h *)
  "WORLDID";
  "PCPU";
  (* "TSC";    *)
  (* "TSC_HZ"; *)
  (* "RDTSC";  *)
]

let predefStrVars = [
  "PROBENAME"
]

let insertOneType(id, t) = symtabTypeDecl id t
let insertOneIntVar(id)  = symtabVarDecl  id typeInt  false
let insertOneStrVar(id)  = symtabVarDecl  id TypeString false
let insertOneFunc (id, t, tl) = symtabFuncInsert id (TypeFunc(t, tl)) []

let insertPredefs() =
  tab.predef <- true;
  List.iter insertOneFunc   predefFuncs;
  List.iter insertOneType   predefTypes;
  List.iter insertOneIntVar predefIntVars;
  List.iter insertOneStrVar predefStrVars;
  tab.predef <- false

let reservedNames = [
  (* VP keywords. *)
  "version";
  "vprobe";
  "defun";
  "cond";
  "aggr";
  "defaggr";
  "defbag";
  "definteger";
  "defstring";
  "do";
  "setint";

  (* VP unexposed builtins *)
  "logint";
  "logstr";
  "setstr";
  "strcmp";
  "baginsert";
  "bagremove";

  (* VP unexposed globals *)
  "ARG0";
  "ARG1";
  "ARG2";
  "ARG3";
  "ARG4";
  "ARG5";
  "ARG6";
  "ARG7";
  "ARG8";
  "ARG9";
]

let isReservedName(name) =
  List.exists (fun n -> n = name) reservedNames
