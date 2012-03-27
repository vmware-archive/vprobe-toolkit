(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * **********************************************************)

(*
 * Symtab module: the global symbol table that keeps information about
 * all identifiers in a program: variables, functions, probes, type
 * names, and structure names. For each identifier, the table
 * maintains an entry with relevant information for that identifier.
 * The symbol table also maintains a mapping of guest symbols to guest
 * addresses, a stack of memory models, and a stack of lexical
 * contexts (global, function, probe, or struct), with the innermost
 * element describing the current memory model and the current lexical
 * context. The memory model stack is used only during parsing, while
 * the context stack is used throughout the compilation process.
 *)
 
open Globals
open Ast
open Memmodel
open Defaults
open Int64
open Printf
open Scanf

type ctx =
  | CtxGlobal
  | CtxFunc   of ident
  | CtxProbe  of ident
  | CtxStruct of ident * skind
and skind =
  | KSTRUCT
  | KUNION
  | KENUM

type varEntry = {
    vstorage: storage;
    vtyp:     typ;
    vpredef:  bool;
    init:     expr option
}

type funcEntry = {
    ftyp: typ;
    fpredef: bool; 
    (* domains from which this function is reachable: *)
    mutable fdoms: string list;
    mutable fparms: (ident * typ) list; 
    mutable flocals: (ident * varEntry) list;
    mutable fbody: stat
}

type probeEntry = {
    mutable pdom: string; (* domain id of the probe *)
    mutable pparms: (ident * typ) list; 
    mutable plocals: (ident * varEntry) list;
    mutable pbody: stat
}

type typedefEntry = {
    ttyp: typ;
    tpredef: bool
}

type structEntry = {
    kind: skind;
    sdefined: bool;
    mutable flds: (ident * typ * int * int64) list
}

type symtab = {
    mutable typedefs: (ident * typedefEntry) list;
    mutable econsts:  (ident * int64) list;
    mutable structs:  (ident * structEntry) list;
    mutable vars:     (ident * varEntry) list;
    mutable probes:   (ident * probeEntry) list;
    mutable funcs:    (ident * funcEntry) list;
    mutable symmap:   (ident, int64) Hashtbl.t;
    (* default domain for probes *)
    mutable defdom:   string;
    (* targs: generate VP scripts for these target domains *)
    mutable targs:     (ident * string) list;
    (* envsseen: list of probe environments present in emmett script *)
    mutable envsseen: string list;
    (* curenv: current environment during vp code emit phase *)
    mutable curenv:   string;
    mutable memmodel: ident;
    mutable ctxstack: ctx list;
    mutable ecounter: int64;
    mutable predef:   bool
}

let tab: symtab = { 
  vars=[]; econsts=[]; typedefs=[]; structs=[]; probes=[]; funcs=[];
  symmap=Hashtbl.create 1021;
  defdom = ""; targs=[]; envsseen=[]; curenv="";
  memmodel="vmw64"; ctxstack=[CtxGlobal]; ecounter=Int64.zero; predef=false
}

let symtabInitMemModel() : unit =
  let mm = !defaultMemModel in
  if not (mmValidMemModel mm) then
    failwith ("Invalid memory model: " ^ mm);
  tab.memmodel <- mm

let skindToString(k: skind) : string =
  match k with
  | KSTRUCT -> "struct"
  | KUNION  -> "union"
  | KENUM   -> "enum"

(*
 * symtabIsTypedef -- Return true iff the given symbol is a type name.
 *)
let symtabIsTypedef(id) : bool = List.mem_assoc id tab.typedefs

(*
 * symtabIdentDefinedInCtx --
 * Return true iff the given symbol is already defined as a variable,
 * function, type, or enum constant in the current scope.
 *)
let symtabIdentDefinedInCtx(id) (ctx) : bool =
  match ctx with
  | CtxGlobal ->
      List.mem_assoc id tab.vars ||
      List.mem_assoc id tab.funcs ||
      List.mem_assoc id tab.typedefs ||
      List.mem_assoc id tab.econsts
  | CtxFunc fid ->
      let fe = List.assoc fid tab.funcs in
      List.mem_assoc id fe.flocals ||
      List.mem_assoc id fe.fparms
  | CtxProbe pid -> 
      let pe = List.assoc pid tab.probes in
      List.mem_assoc id pe.plocals ||
      List.mem_assoc id pe.pparms
  | _ -> failwith "Not reached"

(*
 * symtabIdentDefined --
 * Similar to symtabIdentDefinedInCtx, but checks both the current
 * scope and all enclosing scopes.
 *)
let symtabIdentDefined(id) : bool =
  List.exists (symtabIdentDefinedInCtx id) tab.ctxstack 

(*
 *  symtabCheckIdentVFT -- 
 *  symtabCheckGlobalIdentVFT -- 
 *  Trigger a compilation error iff the given identifier conflicts
 *  with other variables/functions/types/enum constants within the
 *  current scope or the global scope. The latter is used for
 *  enumeration constants, which are automatically promoted to the
 *  global scope.
 *)
let symtabCheckIdentVFT (id: ident) : unit =
  if symtabIdentDefinedInCtx id (List.hd tab.ctxstack)
  then failwith ("Symbol already declared: " ^ id)

let symtabCheckGlobalIdentVFT (id: ident) : unit =
  if symtabIdentDefinedInCtx id (CtxGlobal)
  then failwith ("Symbol already declared: " ^ id)

let rec typeDowngrade(t: typ): typ =
  match t with
  | TypeString
  | TypeBag _
  | TypeAggr _
  | TypeInt  _
  | TypePtr _                              -> t
  | TypeArray(TypeBag _, n) when n > 0     -> TypeBag(n)
  | TypeArray(TypeAggr _, n)               -> TypeAggr(n, 0)
  | TypeArray(TypeArray(TypeAggr _, n), m) -> TypeAggr(m, n)
  | TypeName(id)   -> let te = List.assoc id tab.typedefs
                      in typeDowngrade(te.ttyp)
  | _ -> failwith ("Not an internal type: " ^ (typeToString t))

let symtabProcessInitializer(e: expr option) (t: typ) : expr option =
  match e, t with
  | None, _ -> None
  | Some(ExprIntConst _) as e', TypeInt _  -> e'
  | Some(ExprIntConst _) as e', TypePtr _  -> e'
  | Some(ExprStrConst _) as e', TypeString -> e'
  | _ -> failwith "Invalid initializer"


(*
 * Check validity of storage class for variable 'id'. If no storage class
 * is given, determine default depending on variable type.
 * Returns the storage class for the variable.
 *)
let symtabGetStorageClass (id: ident) (scl: storage list) (t: typ): storage =
  match scl with
  | [sc; _]
     -> failwith (sprintf "more than one storage class specified for variable %s" id)
  | [sc] ->
    (match t, sc with
    | _ , ClassPerHost
    | _ , ClassPerVMK
    | _ , ClassPerVM when not !multidomainSupported
      -> failwith (sprintf "storage class '%s' unsupported" (sclassToString sc))
    | TypeInt _,   ClassPerDomain
    | TypeInt _,   ClassPerVM
    | TypeInt _,   ClassPerVMK
    | TypeInt _,   ClassPerHost
    | TypeString,  ClassPerDomain
    | TypeString,  ClassPerVM
    | TypeString,  ClassPerVMK
    | TypeString,  ClassPerHost
    | TypeBag _,   ClassPerThread
    | TypeAggr _,  ClassPerThread 
      -> failwith (sprintf "unsupported storage class '%s' for variable %s"
                           (sclassToString sc) id)
    | _ -> sc)
  | _ -> (* Determine default storage class *)
    (match t with
    | TypeAggr _
    | TypeBag _ -> ClassPerDomain
    | _ -> ClassPerThread)


let symtabVarDeclInit (id: ident) (scl: storage list) (t: typ) (e: expr option)
                      (auto: bool) : unit =
  symtabCheckIdentVFT(id);
  let t = try typeDowngrade t
          with _ -> failwith
              (match t with 
              | TypeFunc _ -> "Function declaration not allowed: " ^ id
              | _ -> "Invalid type for variable " ^ id) in
  let sc = symtabGetStorageClass id scl t in
  let i  = symtabProcessInitializer e t in
  let ve = { vstorage=sc; vtyp=t; vpredef=tab.predef; init=i } in
  if List.hd(tab.ctxstack) = CtxGlobal || auto then
    tab.vars <- (id, ve) :: tab.vars
  else
    match List.hd(tab.ctxstack) with
    | CtxFunc(fid) ->
        let fe = List.assoc fid tab.funcs in
        fe.flocals <- (id, ve) :: fe.flocals
    | CtxProbe(pid) ->
        let pe = List.assoc pid tab.probes in
        pe.plocals <- (id, ve) :: pe.plocals
    | _ -> failwith "Not reached"

let symtabVarDecl (id: ident) (t: typ) (auto: bool) : unit =
  symtabVarDeclInit id [] t None auto

let symtabEnumConstDecl (id: ident) (o: expr option) : unit =
  symtabCheckGlobalIdentVFT(id);
  let n = match o with
          | None -> tab.ecounter
          | Some(ExprIntConst m) -> m
          | Some(e) -> failwith ("Invalid enum value: " ^ (exprToString e)) in
  tab.econsts <- (id, n) :: tab.econsts;
  tab.ecounter <- Int64.succ(n)

let rec symtabCheckParms(l: (ident * typ) list) : unit =
  match l with
  | [] -> ()
  | (id, _) :: ps ->
      if List.mem_assoc id ps
      then failwith ("Duplicate function parameter: " ^ id)
      else symtabCheckParms(ps)

(*
 * To avoid name collisions for distinct probes with the same name, we
 * prefix probe names with a unique id. The id is eventually removed 
 * before emitting vp code.
 *)
let symtabProbeInsert (id: ident) (p: (ident * typ) list) (b: stat): ident =
  symtabCheckParms(p);
  List.iter (fun (par, _) -> symtabCheckIdentVFT par) p;
  let id' = sprintf "%d:%s" (List.length tab.probes) id in
  tab.probes <- (id', { pdom=""; pparms=p; plocals=[]; pbody=b }) :: tab.probes;
  id'

let symtabProbeId(id: ident) : int=
  sscanf id "%d:%_s" (fun n -> n)

let symtabProbeName(id: ident) : string =
  sscanf id "%_d:%s" (fun s -> s)

let symtabFuncInsert (id: ident) (t: typ) (p: (ident * typ) list) : unit =
  symtabCheckIdentVFT(id);
  symtabCheckParms(p);
  List.iter (fun (par, _) -> symtabCheckIdentVFT par) p;
  tab.funcs <- (id, { ftyp=t; fpredef=tab.predef;
                      fdoms=[]; fparms=p; flocals=[]; 
                      fbody=StatEmpty; }) :: tab.funcs

let symtabTypeDecl (id: ident) (t: typ) : unit =
  symtabCheckIdentVFT(id);
  tab.typedefs <- (id, { ttyp=t; tpredef=tab.predef }) :: tab.typedefs

let checkStructDecl (d: bool) (id: ident) (k: skind) : bool =
  try 
    let {kind=k'; sdefined=d'; flds=_} = List.assoc id tab.structs in
    if k <> k' then
      failwith (sprintf "%s %s already declared as %s %s"
                  (skindToString k) id (skindToString k') id)
    else if d && d' then
      failwith (sprintf "%s %s already defined" (skindToString k) id)
    else
      d
  with Not_found -> true

let symtabStructDecl (id: ident) (k: skind) : unit =
  if checkStructDecl false id k then
    tab.structs <- (id, { kind=k; sdefined=false; flds=[] }) :: tab.structs

let symtabStructDef (id: ident) (k: skind) : unit =
  if checkStructDecl true id k then 
    (tab.structs <- List.remove_assoc id tab.structs;
     tab.structs <- (id, { kind=k; sdefined=true;flds=[] }) :: tab.structs)

let symtabFieldDecl (id: ident) (t: typ) (w: int) (o: int64) : unit =
  match List.hd tab.ctxstack with
  | CtxStruct(structid, _) -> 
      let se: structEntry = List.assoc structid tab.structs in
      if List.exists (fun (id', _, _, _) -> id = id') se.flds 
      then failwith ("Field " ^ id ^ " re-declared")
      else se.flds <- (id, t, w, o) :: se.flds
  | _ -> failwith "Invalid context for field declaration."

let symtabSortFields () : unit =
  match List.hd tab.ctxstack with
  | CtxStruct(structid, _) -> 
      let offs (_, _, _, o) = o in
      let sortFunc l1 l2 = compare (offs (List.hd l2)) (offs (List.hd l1)) in
      let blockFunc l ((_, _, _, o) as e) =
        match l with
        | [] -> [e] :: []
        | (((_, _, _, o1)::_) as li) :: lo ->
            if o1 = minus_one 
            then (e :: li) :: lo
            else [e] :: (li :: lo)
        | _ -> failwith "Not reached" in
      let formBlocks = List.fold_left blockFunc [] in
      let sortBlocks = List.sort sortFunc in
      let revBlocks  = List.map List.rev in
      let se: structEntry = List.assoc structid tab.structs in
      se.flds <- List.flatten (revBlocks (sortBlocks (formBlocks se.flds)))
  | _ -> failwith "Invalid context."
  
(*
 * Push and pop the context stack.
 *)
let symtabPushCtx (c: ctx) : unit =
  tab.ctxstack <- c :: tab.ctxstack

let symtabPopCtx () : unit =
  tab.ctxstack <- (List.tl tab.ctxstack)

(*
 * Set and get the current memmodel.
 *)
let symtabSetMemModel (id: ident) : unit =
  if not (mmValidMemModel id)
  then failwith ("Invalid memory model name: " ^ id)
  else tab.memmodel <- id

let symtabGetMemModel(): ident = tab.memmodel


(*
 * Setting and getting of "current environment", ie. the
 * environment for which code is being generated. 
 *)
let symtabSetCurEnv(env: string): unit =
  tab.curenv <- env

let symtabCurEnv(): string =
  tab.curenv

(*
 * Function that lookup information in the symbol table.
 *)
let symtabGetEnumConst(id: ident) : int64      = List.assoc id tab.econsts
let symtabGetVar(id: ident) : varEntry         = List.assoc id tab.vars
let symtabGetFunc(id: ident) : funcEntry       = List.assoc id tab.funcs
let symtabGetProbe(id: ident) : probeEntry     = List.assoc id tab.probes
let symtabGetStruct(id: ident) : structEntry   = List.assoc id tab.structs
let symtabGetTypedef(id: ident) : typedefEntry = List.assoc id tab.typedefs

let symtabLookupField(s: ident) (f: ident) =
  let se = try symtabGetStruct(s)
           with Not_found -> failwith ("Struct " ^ s ^ " not found")
  in
  try List.find (fun (f', _, _, _) -> f = f') se.flds
  with Not_found -> failwith("Field " ^ f ^ " not found")


let symtabLookupVar(id: ident) : (storage * typ) =
  let varinfo id varEntries = 
    let ve = List.assoc id varEntries in (ve.vstorage, ve.vtyp)
  in
  match List.hd tab.ctxstack with
  | CtxGlobal -> varinfo id tab.vars
  | CtxFunc fid -> 
      (let fe = symtabGetFunc(fid) in
      try varinfo id fe.flocals
      with Not_found -> 
        try (ClassPerThread, List.assoc id fe.fparms)
        with Not_found -> varinfo id tab.vars)
  | CtxProbe pid -> 
      (let pe = symtabGetProbe(pid) in
      try varinfo id pe.plocals
      with Not_found ->
        try (ClassPerThread, List.assoc id pe.pparms)
        with Not_found -> varinfo id tab.vars)
  | _ -> failwith "Not reached"

let symtabLookupVarType(id: ident): typ =
  snd (symtabLookupVar id)

(* 
 * Return true if there are any "perhost" variables in the script.
 *)
let symtabHasSharedVars() : bool =
  List.exists (fun(v,ve) -> ve.vstorage == ClassPerHost) tab.vars  

(*
 * A function that looks up a local variable or a parameter in a
 * function or a probe. Returns a triple consisting of: "probe" or
 * "func"; "local" or "parm"; and the parameter index or
 * function/probe index for locals.
 *)
let symtabLookupLocal(id: ident) : (string * string * int) option =
  let rec index(n)(x) = function
    | [] -> -1
    | (y, _) :: tl -> if x = y then n else index (n + 1) x tl in 
  match List.hd tab.ctxstack with
  | CtxGlobal -> None
  | CtxFunc fid -> 
      let fe = symtabGetFunc(fid) in
      if List.mem_assoc id fe.flocals then
        (* Ignore builtins when computing the function index. *)
        let list = List.filter (fun (_, e) -> not e.fpredef) tab.funcs in
        Some("func", "local", index 0 fid (List.rev list))
      else if List.mem_assoc id fe.fparms then
        Some("func", "parm", index 0 id fe.fparms)
      else
        None
  | CtxProbe pid -> 
      let pe = symtabGetProbe(pid) in
      if List.mem_assoc id pe.plocals then
        Some("probe", "local", index 0 pid (List.rev tab.probes))
      else if List.mem_assoc id pe.pparms then
        Some("probe", "parm", index 0 id pe.pparms)
      else
        None

  | _ -> failwith "Not reached"
  

(*
 *  Functions for pretty printing the symbol table contents.
 *)
let rec symtabPrintTypedefs(predef: bool) : unit =
  let  printTypedef(id, te) =
    if predef = te.tpredef 
    then printf "   TYPEDEF %s = %s\n" id (typeToString te.ttyp) in
  List.iter printTypedef (List.rev tab.typedefs)

let rec symtabPrintEconsts() : unit =
  let  printEconst(id, n) = printf "   ECONST %s = %s\n" id 
                                   (Int64.to_string n) in
  List.iter printEconst (List.rev tab.econsts)

let rec symtabPrintStructs() : unit =
  let printField (id, t, w, o) =
    printf "      %s: %s%s%s;\n" id (typeToString t)
      (if w >= 0 then sprintf " : %d" w else "")
      (if o <> minus_one then " @ " ^ (to_string o) else "") in
  let  printStruct(id, ts) =
    printf "   %s %s" (String.uppercase(skindToString ts.kind)) id;
    if ts.kind <> KENUM 
    then (printf " {\n";
          List.iter printField (List.rev ts.flds);
          printf "   }\n")
    else printf "\n" in
  List.iter printStruct (List.rev tab.structs)

let symtabVarEntry(id: ident) (ve: varEntry) : string =
  sprintf " %s %s: %s%s" (sclassToString ve.vstorage) id (typeToString ve.vtyp)
    (match ve.init with
    | None -> ""
    | Some e -> " = " ^ (exprToString e))

let rec symtabPrintVars(predef: bool) : unit =
  let printVar(id, ve) = 
    if predef = ve.vpredef
    then printf "   VAR %s\n" (symtabVarEntry id ve) in
  List.iter printVar (List.rev tab.vars)

let rec symtabPrintFuncs(predef: bool) : unit =
  let printFunc(id, fe) =
    if predef = fe.fpredef then
      let parm(id, t) = id ^  " : " ^ (typeToString t) in
      let local(id, ve) = "   " ^ (symtabVarEntry id ve) in
      let parms  = String.concat ", " (List.map parm fe.fparms) in
      let locals = String.concat "\n" (List.map local fe.flocals) in
      let body   = statToString 2 fe.fbody in
      printf "   FUNC %s (%s):\n%s%s" id parms locals body
  in
  List.iter printFunc (List.rev tab.funcs)

let rec symtabPrintProbes() : unit =
  let printProbe(id, pe) = 
    let parm(id, t) = id ^  " : " ^ (typeToString t) in
    let local(id, ve) = "\n      " ^ (symtabVarEntry id ve) in
    let parms  = String.concat ", " (List.map parm pe.pparms) in
    let locals = String.concat ""   (List.map local pe.plocals) in
    let body   = statToString 2 pe.pbody in
    let id'    = symtabProbeName(id) in
    printf "   VPROBE %s (%s):%s\n%s" id' parms locals body in
  List.iter printProbe (List.rev tab.probes)

let rec symtabPrintASTs() : unit =
  if !verbose then
    (printf "/*\n";
     symtabPrintTypedefs(false);
     symtabPrintEconsts();
     symtabPrintStructs();
     symtabPrintVars(false);
     symtabPrintFuncs(false);
     symtabPrintProbes();
     printf "*/\n")

(*
 * Compiler pass: iterate through all the functions and probes.
 *)
let compilerPass (f: ident * funcEntry -> unit) 
                 (p: ident * probeEntry -> unit) : unit =
  let processFunc(id, fe)  = symtabPushCtx(CtxFunc id);
                             f(id, fe);
                             symtabPopCtx() in
  let processProbe(id, pe) = symtabPushCtx(CtxProbe id);
                             p(id, pe);
                             symtabPopCtx() in
  List.iter processFunc (List.rev tab.funcs);
  List.iter processProbe(List.rev tab.probes)

(*
 * parseSymbolFile: read the guest symbol file, if any, and load
 * the "tab.symmap" hashtable. We support the 32-bit and 64-bit
 * formats produced by winDbg via the 'x *!*' command, and the linux 
 * format from /proc/kallsyms.
 *)
let parseSymbolFile() =
  if !symbolFile <> "" then
    let of_uint32(n) = logand (of_int32 n) (of_string "0xffffffff") in
    let file = try open_in (!symbolFile) 
               with Sys_error(s) -> failwith s in
    if !verbose then printf "# Parsing symbols %s...\n" (!symbolFile);
    try 
      while true do
        let line = input_line file in
        try (* Linux symbol format *)
          sscanf line "%Lx %s %s"
            (fun a -> fun c -> fun s ->
              if String.length c <> 1  then
                raise (Scan_failure "")
              else
                Hashtbl.add tab.symmap s a)
        with _ -> 
        try (* Windows 64-bit symbol format *)
          sscanf line "%lx`%lx %s"
            (fun h -> fun l -> fun s ->
              Hashtbl.add tab.symmap s
                (logor(shift_left(of_uint32 h) 32) (of_uint32 l)))
        with _ ->
        try (* Linux 32-bit symbol format *)
          sscanf line "%lx %s"
            (fun a -> fun s ->
              Hashtbl.add tab.symmap s (of_uint32 a))
        with _ -> 
          ()  (* ignore non-matching lines *)
      done
    with End_of_file -> close_in file;
    if !verbose then
      printf "# Loaded %d symbols\n" (Hashtbl.length tab.symmap)
