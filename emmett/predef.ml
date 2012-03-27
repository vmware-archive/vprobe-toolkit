(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
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
  ("printf",          TypeVoid,   [TypeString; TypeVarArgs]);
  ("sprintf",         TypeVoid,   [TypeString; TypeString; TypeVarArgs]);
  ("exit",            TypeVoid,   []);
  ("logaggr",         TypeVoid,   [TypeVarArgs]);
  ("clearaggr",       TypeVoid,   [TypeVarArgs]);
  ("gueststack",      TypeVoid,   [TypeString; TypeVarArgs]);
  ("getguest",        typeInt,    [typeInt]);
  ("getguestphys",    typeInt,    [typeInt]);
  ("getgueststr",     TypeVoid,   [TypeString; typeInt; typeInt]);
  ("getguestphysstr", TypeVoid,   [TypeString; typeInt; typeInt]);
  ("getsystemtime",   typeInt,    []);
  ("worldtovmid",     typeInt,    [typeInt]);
  ("getvmcs",         typeInt,    [typeInt]);
  ("getgpr",          typeInt,    [typeInt]);
  ("assert",          TypeVoid,   [typeInt; TypeString; TypeVarArgs]);
  ("schedtrace",      TypeVoid,   [typeInt; typeInt; typeInt; typeInt; typeInt]);

  (* Unsupported builtins *)
  ("vmwstack",        TypeVoid,   [TypeString; TypeVarArgs]);
  ("getvmw",          typeInt,    [typeInt]);
  ("getvmwstr",       TypeVoid,   [TypeString; typeInt; typeInt]);
  ("readpmc",         typeInt,    [typeInt]);
  ("readmsr",         typeInt,    [typeInt]);
  ("getmeminfo",      typeInt,    []);
  ("offatret",        typeInt,    [typeInt]);
  ("offatstrcpy",     typeInt,    [typeInt; typeInt]);
  ("offatseg",        typeInt,    [typeInt]);
  ("suspendvm",       TypeVoid,   [TypeVarArgs]);
  ("delayus",         TypeVoid,   [typeInt]);
  ("livecore",        TypeVoid,   [TypeVarArgs]);
  ("fatalPSOD",       TypeVoid,   []);
]

let predefIntVars = [
  (* Globals from vprobeVMMVMX.h *)
  "ISVMM";
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
  "VMCS_INSTR_INFO";
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
  "HVSTATE";
  "EMULATION_MODE";
  "RFLAGS";
  "APIC_BASEPA";
  "APICREGS";
  "RDTSC";
  "MEMSCHED_ADDR";

  (* Globals from vmkProbes.h *)
  "WORLDID";
  "PCPU";
  "PRDA_ADDR";
  (* "TSC";    *)
  (* "TSC_HZ"; *)
  (* "RDTSC";  *)
]

let predefStrVars = [
  "PROBENAME"
]

let predefConstants = [
  (* Common NULL pointer value *)
  ("NULL",                 0);

  (*
   * General Purpose Register state, See Intel manual 2A, section about
   * Instruction Format, the table about "32-Bit Addressing Form with the
   * ModR/M Byte" explains about the unsual register ordering.
   *)
  ("REG_RAX",              0);
  ("REG_RCX",              1);
  ("REG_RDX",              2);
  ("REG_RBX",              3);
  ("REG_RSP",              4);
  ("REG_RBP",              5);
  ("REG_RSI",              6);
  ("REG_RDI",              7);
  ("REG_R8",               8);
  ("REG_R9",               9);
  ("REG_R10",              10);
  ("REG_R11",              11);
  ("REG_R12",              12);
  ("REG_R13",              13);
  ("REG_R14",              14);
  ("REG_R15",              15);

  (* Read-only VMCS fields. See Intel manual volume 3B Appendix H. *)
  ("VT_PHYSICAL_ADDR",     0x00002400);
  ("VT_VMINSTR_ERR",       0x00004400);
  ("VT_EXIT_REASON",       0x00004402);
  ("VT_EXIT_INT_INFO",     0x00004404);
  ("VT_EXIT_INT_ERR",      0x00004406);
  ("VT_IDTVEC_INFO",       0x00004408);
  ("VT_IDTVEC_ERR",        0x0000440a);
  ("VT_INSTR_LEN",         0x0000440c);
  ("VT_INSTR_INFO",        0x0000440e);
  ("VT_EXIT_QUAL",         0x00006400);
  ("VT_IO_ECX",            0x00006402);
  ("VT_IO_ESI",            0x00006404);
  ("VT_IO_EDI",            0x00006406);
  ("VT_IO_EIP",            0x00006408);
  ("VT_LINEAR_ADDR",       0x0000640a);

  (* A few other VMCS fields. *)
  ("VT_VMENTRY_INT_INFO",  0x00004016);
  ("VT_VMENTRY_INT_ERR",   0x00004018);
]  

let insertOneType(id, t) = symtabTypeDecl id t
let insertOneIntVar(id)  = symtabVarDecl  id typeInt  false
let insertOneStrVar(id)  = symtabVarDecl  id TypeString false
let insertOneFunc (id, t, tl) = symtabFuncInsert id (TypeFunc(t, tl)) []
let insertOneConst(id, c) = symtabEnumConstDecl id (Some (exprIntConst c))

let insertPredefs() =
  tab.predef <- true;
  List.iter insertOneFunc   predefFuncs;
  List.iter insertOneType   predefTypes;
  List.iter insertOneIntVar predefIntVars;
  List.iter insertOneStrVar predefStrVars;
  List.iter insertOneConst  predefConstants;
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
