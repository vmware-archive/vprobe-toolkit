(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
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
  | [SpecInt "int"]                         -> TypeInt(mm, "int")
  | [SpecSign "unsigned"; SpecInt "int"]    -> TypeInt(mm, "int")
  | [SpecSign "unsigned"]                   -> TypeInt(mm, "int")
  | [SpecInt "short"]                       -> TypeInt(mm, "short")
  | [SpecSign "unsigned"; SpecInt "short"]  -> TypeInt(mm, "short")
  | [SpecInt "long"]                        -> TypeInt(mm, "long")
  | [SpecSign "unsigned"; SpecInt "long"]   -> TypeInt(mm, "long")
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
      | _, _          -> symtabVarDeclInit id t (snd i) false

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

let actionsExprAddr(e: expr) : expr = 
  match e with
  | ExprIdent(id) -> ExprIntConst(actionsLookupSym id)
  | _ -> ExprAddr(e)

let actionsSplitName(id) = 
  if String.contains id ':' then
    let i = 1 + String.index id ':' in
    (String.sub id 0 i, String.sub id i (String.length(id) - i))
  else
    (id, "")

let actionsTranslateGuestProbe (id: ident) : ident = 
  let isSymbol(sym) = try ignore(Int64.of_string sym); false
                      with _ -> true
  in
  match actionsSplitName(id) with
  | ("GUEST:"       as name), sym
  | ("GUEST_READ:"  as name), sym
  | ("GUEST_WRITE:" as name), sym
  | ("GUEST_RET:"   as name), sym when isSymbol(sym)  
    -> sprintf "%s0x%Lx" name (actionsLookupSym sym)
  | n,m -> id

let actionsCheckProbeArgs(id: ident) (parms) : unit =
  let numArgs = List.length parms in
  if numArgs > 10 then
    failwith ("Probe has too many arguments: " ^ id)
  else if numArgs > 1 then
    match fst(actionsSplitName id) with
    | "VMKERNEL_RET:" 
    | "GUEST_RET:" ->
        failwith ("RET probe has more than one argument: " ^ id)
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
  symtabStructDef id KENUM;
  kindToSpec id KENUM

let actionsExprIdent(id: ident) : expr =
  try ExprIntConst(symtabGetEnumConst id)
  with Not_found -> ExprIdent(id)

let actionsExprCall(id: ident) (l: expr list) : expr =
  ExprCall(id, l)

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
  | NotDeclared _ ->
      match e with
      | ExprIdent(id) -> let l1, l2 = partArgs l in
                         let t = TypeAggr(List.length l1, List.length l2) in
                         symtabVarDecl id t true; 
                         ExprAggr(id, l1, l2)
      | _ -> failwith ("Couldn't infer type of expression: " ^ ascii)

let actionsAssignBagOrAggr(e1: expr) (e2: expr): expr =
  match e1 with
  | ExprIdent(id) ->
      if not (symtabIsVar id)
      then symtabVarDecl id (TypeAggr(0, 0)) true;
      ExprAssignAggr(id, [], [], e2)
  | ExprAggr(id, l1, l2) -> ExprAssignAggr(id, l1, l2, e2)
  | ExprBag(id, e1') -> ExprAssignBag(id, e1', e2)
  | _ -> failwith ("Invalid expression in bag/aggr assignment: " ^
                   (exprToString e1))

let actionsAggrInc(e: expr) : expr =
  match e with
  | ExprIdent(id) ->
      if not (symtabIsVar id)
      then symtabVarDecl id (TypeAggr(0, 0)) true;
      ExprAssignAggr(id, [], [], exprIntConst 1)
  | ExprAggr(id, l1, l2) -> ExprAssignAggr(id, l1, l2, exprIntConst 1)
  | ExprBag _ -> failwith ("The ++ operator is not allowed for bags: " ^
                   (exprToString e))
  | _ -> failwith ("Invalid ++ expression: " ^ (exprToString e))
  
let actionsAssign(op: string) (id: ident) (e: expr) : expr =
  if not (symtabIsVar id)
  then 
    (try symtabVarDecl id (getExprType e) true
    with _ -> symtabVarDecl id typeInt true);
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
