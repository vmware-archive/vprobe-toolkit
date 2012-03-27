(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * **********************************************************)

(*
 * Actions module: contains helper functions for the semantic actions
 * performed during parsing. All complex manipluations from parser.mly
 * are moved out in this module, leaving parser.mly simple and easy to
 * follow.
 *)

open Predef
open Ast
open Symtab
open Domain
open Type
open Int64
open Printf

type decl =
  | DeclNull
  | DeclId      of ident
  | DeclPointer of decl * (qual list)
  | DeclArray   of decl * (int option)
  | DeclFunc    of decl * ((ident option * typ) list)

type initdecl = decl * (expr option)

let rec getDeclId : decl -> ident = function
  | DeclNull  -> raise Not_found
  | DeclId id -> id
  | DeclPointer(d, _)
  | DeclArray(d, _)
  | DeclFunc(d, _) -> getDeclId(d)

let specToType(_, _, tl) : typ =
  let mm = symtabGetMemModel() in
  match tl with
  | [SpecInt "char"]                        -> TypeInt(mm, "char")
  | [SpecSign "unsigned"; SpecInt "char"]   -> TypeInt(mm, "char")

  | [SpecInt "int"]                         -> TypeInt(mm, "int")
  | [SpecSign "unsigned"; SpecInt "int"]    -> TypeInt(mm, "int")
  | [SpecSign "unsigned"]                   -> TypeInt(mm, "int")

  | [SpecInt "short"]
  | [SpecInt "short"; SpecInt "int"]
  | [SpecSign "unsigned"; SpecInt "short"]
  | [SpecInt "short"; SpecSign "unsigned"; SpecInt "int"]
    -> TypeInt(mm, "short")

    (*
     * Per PR 428978, the memory model should be extended to 
     * describe the width of long long. Until then, let's
     * interpret 'long long' as 'long'. That is correct for 
     * 64-bit domains.
     *)
  | [SpecInt "long"; SpecInt "long"]
  | [SpecInt "long"; SpecInt "long"; SpecInt "int"]
  | [SpecSign "unsigned"; SpecInt "long"; SpecInt "long"]
  | [SpecInt "long"; SpecInt "long"; SpecSign "unsigned"; SpecInt "int"]
    -> TypeInt(mm, "long")

  | [SpecInt "long"]
  | [SpecInt "long"; SpecInt "int"]
  | [SpecSign "unsigned"; SpecInt "long"]
  | [SpecInt "long"; SpecSign "unsigned"; SpecInt "int"]
    -> TypeInt(mm, "long")

  | [SpecStruct id]                         -> TypeStruct(id)
  | [SpecUnion id]                          -> TypeUnion (id)
  | [SpecEnum id]                           -> TypeEnum(id)
  | [SpecTypeName id]                       -> TypeName(id)
  | [SpecBag]                               -> TypeBag(256)
  | [SpecAggr]                              -> TypeAggr(0,0)
  | [SpecString]                            -> TypeString
  | [SpecVoid]                              -> TypeVoid
  | _ -> failwith ("Invalid specifier(s) : " ^
                   (String.concat ", " (List.map specToString tl)))

let rec typeDeclToPair (t: typ) (d: decl) : ident option * typ =
  let mm = symtabGetMemModel() in
  match d with
  | DeclNull              -> (None, t)
  | DeclId id             -> (Some id, t)
  | DeclPointer (d', _)   -> typeDeclToPair (TypePtr(mm, t)) d'
  | DeclArray(d', None)   -> typeDeclToPair (TypePtr(mm, t)) d'
  | DeclArray(d', Some n) -> typeDeclToPair (TypeArray(t, n)) d'
  | DeclFunc(d', l)       -> typeDeclToPair (TypeFunc(t, List.map snd l)) d'

let specDeclToPair (s: specs) (d: decl) : ident option * typ =
  typeDeclToPair (specToType s) d

let specDeclToType (s: specs) (d: decl) : typ =
  snd (specDeclToPair s d)


let rec actionsDecl (s: specs) (i: initdecl) : unit =
  (match snd(i) with
  | None
  | Some(ExprStrConst _)
  | Some(ExprIntConst _) -> ()
  | Some(e) -> failwith ("Initializer not constant: " ^ (exprToString e)));
  match specDeclToPair s (fst i) with
  | None, _ -> ()
  | Some id, t ->
      let _, slist, _ = s in
      match List.exists (fun x -> x = ClassTypedef) slist, t with
      | true, _       -> symtabTypeDecl id t
      | _, TypeFunc _ -> failwith ("Function declaration not allowed: " ^ id)
      | _, _          -> symtabVarDeclInit id slist t (snd i) false

let actionsDecls (s: specs) (l: initdecl list) : unit =
  List.iter (actionsDecl s) l

let actionsDeclField (o: int64) (s: specs) (d, w: decl * int) : unit =
  match specDeclToPair s d with
  | None, _ -> failwith "Invalid structure field declaration (no field name)" 
  | Some id, t ->
      let _, slist, _ = s in
      let typedef = List.exists (fun x -> x = ClassTypedef) slist in
      if typedef
      then failwith "Invalid structure field declaration (typedef)" 
      else symtabFieldDecl id t w o

let actionsDeclFields(s:specs) (l: (decl * int) list) : unit = 
  List.iter (actionsDeclField (minus_one) s) l

let actionsScanParms = function
  | None, _ -> failwith "Invalid parameter declaration"
  | Some id, t -> (id, typeDowngrade t)

let actionsFuncDefBegin (s: specs) (d: decl) : ident =
  let rec getParams = function
    | DeclFunc(_, [None, TypeVoid]) -> []
    | DeclFunc(_, l) -> List.map actionsScanParms l
    | DeclPointer(d', _) -> getParams(d')
    | _ -> failwith ("Invalid function declaration: " ^ (getDeclId d)) in
  let params = getParams(d) in
  let typeDowngradeVarArgs(id, t) = if t = TypeVarArgs then t
                                    else typeDowngrade(t)
  in
  match specDeclToPair s d with
  | Some id, TypeFunc(r, _) ->
      let ret = 
        if r = TypeVoid then r else
        try typeDowngrade(r) 
        with _ -> failwith ("Function " ^ id ^ " has invalid return type: " ^
                            (typeToString r)) in
      let ty = TypeFunc(ret, List.map typeDowngradeVarArgs params) in
      symtabFuncInsert id ty params;
      symtabPushCtx(CtxFunc id);
      id
  | _ -> failwith ("Invalid function declaration: " ^ (getDeclId d))

let actionsFuncDefFinish (id: ident) (body: stat) : unit =
  (symtabGetFunc id).fbody <- body;
  symtabPopCtx()

let actionsLookupSym(id: ident) : int64 =
  try Hashtbl.find tab.symmap id
  with Not_found -> failwith ("Invalid guest symbol: " ^ id)

let actionsGetProbeLoc(id: ident) : int64 =
  try Int64.of_string id with _ -> actionsLookupSym(id)

let actionsExprAddr(e: expr) : expr = 
  match e with
  | ExprIdent(id) -> ExprIntConst(actionsLookupSym id)
  | _ -> ExprAddr(symtabGetMemModel(), e)

let actionsSplitName(id) = 
  if String.contains id ':' then
    let i = 1 + String.index id ':' in
    (String.sub id 0 (i - 1), String.sub id i (String.length(id) - i))
  else
    (id, "")

(* 
 * Break string s into a list of substrings, splitting
 * at each ":" in s.
 *)
let rec actionsSplit(s: string) : string list =
  match actionsSplitName s with
  | s, "" -> [s]
  | s, rest -> s :: actionsSplit rest

let actionsTranslateGuestProbe (id: ident) : ident =
  match actionsSplit id with
  | ["GUEST"; "OFFSET"; sym; off]
      -> sprintf "GUEST:OFFSET:0x%Lx:%s" (actionsGetProbeLoc sym) off
  | ["GUEST"; "EXIT"  as op; sym]
  | ["GUEST"; "ENTER" as op; sym]
  | ["GUEST"; "READ"  as op; sym]
  | ["GUEST"; "WRITE" as op; sym]
      -> sprintf "GUEST:%s:0x%Lx" op (actionsGetProbeLoc sym)
  | _ -> id

let actionsCheckProbeArgs(id: ident) (parms) : unit =
  let numArgs = List.length parms in
  if numArgs > 10 then
    failwith ("Probe has too many arguments: " ^ id)
  else if numArgs > 1 then
    match actionsSplit id with
    | "EXIT" :: _
    | _ :: "EXIT" :: _ -> failwith("EXIT probe has more than one argument: " ^ id)
    | _ -> ()

let actionsProbeDefBegin (id: ident) (parms) : ident =
  let _ = actionsCheckProbeArgs id parms in
  let parms' = List.map actionsScanParms parms in
  let id1 = actionsTranslateGuestProbe(id) in
  let id2 = symtabProbeInsert id1 parms' StatEmpty in
  symtabPushCtx(CtxProbe id2);
  id2

let actionsProbeDefFinish (id: ident) (body: stat) : unit =
  (symtabGetProbe id).pbody <- body;
  symtabPopCtx()

let rec actionsDeclPtrMerge (d: decl) : decl -> decl = function
  | DeclPointer(d', s) -> DeclPointer(actionsDeclPtrMerge d d', s)
  | DeclNull -> d
  | _ -> failwith "Not reached"

let actionsMergeQ (q: qual)    = fun (ql, sl, tl) -> (q::ql, sl, tl)
let actionsMergeS (s: storage) = fun (ql, sl, tl) -> (ql, s::sl, tl)
let actionsMergeT (t: tyspec)  = fun (ql, sl, tl) -> (ql, sl, t::tl)

let kindToSpec (id: ident): skind -> tyspec = function
  | KSTRUCT -> SpecStruct id
  | KUNION  -> SpecUnion id
  | KENUM   -> SpecEnum id

let actionsStructDecl(id: ident) (k: skind) : tyspec =
  symtabStructDecl id k;
  kindToSpec id k

let actionsStructPush(id: ident) (k: skind) : tyspec =
  symtabStructDef id k;
  symtabPushCtx(CtxStruct(id, k));
  kindToSpec id k

let actionsStructPop(ts: tyspec) : tyspec =
  symtabSortFields();
  symtabPopCtx();
  ts

let actionsEnum (id: ident) : tyspec =
  tab.ecounter <- Int64.zero;
  symtabStructDef id KENUM;
  kindToSpec id KENUM

let actionsExprIdent(id: ident) : expr =
  try ExprIntConst(symtabGetEnumConst id)
  with Not_found -> ExprIdent(id)

let actionsAssert(line: int) (args: expr list) : expr =
  let cond = List.hd(args) in
  match List.tl(args) with
  | [] ->
      let lineExpr = ExprStrConst(sprintf "\"line %d\"" line) in
      ExprCall("assert", [cond; lineExpr])
  | (ExprStrConst _ as fmt)::params -> 
      let lineExpr = ExprStrConst(sprintf "\"line %d: \"" line) in
      let fmtWithLine = exprBinary("+", lineExpr, fmt) in
      ExprCall("assert", cond::fmtWithLine::params)
  | _ -> failwith ("non-constant string format in assert")

let actionsExprComma(l: expr list) : expr =
  match l with
  | []  -> failwith "Not reached"
  | [e] -> e
  | _   -> ExprComma(l)

let actionsExprIndexed(e: expr) (l: expr list) : expr =
  let indices = String.concat ", " (List.map exprToString l) in
  let ascii = (exprToString e) ^ "[" ^ indices ^ "]" in
  let partArgs(l) = typePartitionAggrList l in
  try match e, getExprType(e) with 
  | _, TypeArray _
  | _, TypePtr _ -> 
      if List.length(l) <> 1 
      then failwith ("Array expression with multiple indices: " ^ ascii)
      else ExprArray(e, List.hd l)
  | ExprIdent(id), TypeBag _ ->
      if List.length(l) <> 1 
      then failwith ("Bag expression with multiple indices: " ^ ascii)
      else ExprBag(id, List.hd l)
  | ExprIdent(id), TypeAggr _ ->
      let l1, l2 = partArgs l in
      ExprAggr(id, l1, l2)
  | _, t -> failwith ("Invalid index expression: " ^ ascii ^
                      " (" ^ (exprToString e) ^ " has type " ^ 
                      (typeToString t) ^ ")")
  with 
  | _ as exc ->
      match e with
      | ExprIdent(id) ->
          if List.length(l) <= 1
          then
            (* 
             * There is type ambiguity: id[i] can refer to an array access,
             * aggregate, or bag operation. We give up and raise the exception.
             *)
            raise exc
          else
            (* 
             * A multi-index expression id[i1, .., aN ] can only refer to
             * an aggregate. Let's auto-declare the type of id since this is
             * the only possibility.
             *)
            let l1, l2 = partArgs l in
            let t = TypeAggr(List.length l1, List.length l2) in
            symtabVarDecl id t true; 
            ExprAggr(id, l1, l2)
      | _ -> raise exc

let actionsAssignBagOrAggr(e1: expr) (e2: expr): expr =
  match e1 with
  | ExprIdent(id) -> ExprAssignAggr(id, [], [], e2)
  | ExprAggr(id, l1, l2) -> ExprAssignAggr(id, l1, l2, e2)
  | ExprBag(id, e1') -> ExprAssignBag(id, e1', e2)
  | _ -> failwith ("Invalid expression in bag/aggr assignment: " ^
                   (exprToString e1))

let actionsAggrInc(e: expr) : expr =
  match e with
  | ExprBag _ -> failwith ("The ++ operator is not allowed for bags: " ^
                   (exprToString e))
  | _ -> actionsAssignBagOrAggr e (exprIntConst 1)
  
let actionsAssign(op: string) (id: ident) (e: expr) : expr =
  (* 
   * For an assignment id = expr, or id += expr, etc. if id is undeclared
   * but we are able to type expr, then we auto-declare id with that type.
   *)
  if not (symtabIdentDefined id)
  then symtabVarDecl id (getExprType e) true;
  if op = ""
  then ExprAssign(id, e)
  else ExprAssign(id, ExprBinary(op, ExprIdent id, e))

let actionsSizeOfE(e: expr) : expr =
  try ExprIntConst(getTypeSize(getExprType e))
  with _ -> failwith ("Cannot determine the size of: " ^ (exprToString e))

let actionsSizeOfT(t: typ) : expr =
  try ExprIntConst(getTypeSize t)
  with _ -> failwith ("Cannot determine the size of: " ^ (typeToString t))

let actionsPositiveInt31 (e: expr) : int =
  let se = exprToString e in
  match e with
  | ExprIntConst(n) ->
      if Int64.of_int(Int64.to_int n) <> n || Int64.to_int(n) < 0
      then failwith ("Constant is not a positive 31-bit integer: " ^ se)
      else Int64.to_int(n)
  | _ -> failwith ("Expression is not a constant: " ^ se)

let actionsPositiveInt64 (e: expr) : int64 =
  let se = exprToString e in
  match e with
  | ExprIntConst(n) ->
      if Int64.compare n zero < 0
      then failwith ("Constant is not a positive 64-bit integer: " ^ se)
      else n
  | _ -> failwith ("Expression is not a constant: " ^ se)


(*
 * Trim whitespaces and quotes from a string.
 *)
let rec actionsTrim s =
  let isSpaceOrQuote c = c = ' ' || c = '\t' || c = '\"' || c = '\'' in
  let l = String.length s in
  if l > 0 && isSpaceOrQuote s.[0] then
    actionsTrim (String.sub s 1 (l-1))
  else if l > 0 && isSpaceOrQuote s.[l-1] then
    actionsTrim (String.sub s 0 (l-1))
  else
    s

(*
 * Add the target environment to the list of targets for which we
 * issue a VP script. We allow entries with the same VM environment,
 * so that the same script can be generated for different target VMs.
 * However, targets must all be different.
 *)
let actionsEnvSpec ((env,tgt) : string * string) : unit =
  let tgt' = actionsTrim tgt in
  if env <> "VMK" && tgt' = "" then
    failwith ("Empty target for domain: " ^ env);
  if List.mem tgt' (List.map snd tab.targs) then
    (match env with
    | "VMK" -> failwith ("Duplicate VMK target")
    | _     -> failwith ("Duplicate VM target: " ^ tgt));
  tab.targs <- tab.targs @ [ (env, tgt') ]

(*
 * Perform initilization tasks after we're done parsing the
 * specification list.
 *)
let actionsSpecListDone() : unit =
  symtabInitMemModel();
  domainInitDefaultDom();
  parseSymbolFile()
