(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Type module: mainly repsonsible with type-checking a program. Also
 * provides a few type-related utility functions.
 *)

open Predef
open Globals
open Ast
open Symtab
open Memmodel
open Int64

(*
 * autoDeclareVars -- a variable that controls whether to auto-declare
 * undeclared variables when looking for the type of an expression.
 * Turned on during the type-checking phase, and turned off elsewhere.
 *)
let autoDeclareVars = ref false

exception TypeError   of string
exception NotDeclared of ident
exception AssignError of string

let rec unwrap : typ -> typ = function
  | TypeName id -> unwrap((symtabGetTypedef id).ttyp)
  | t -> t

let alignInt64 (n: int64) (m: int64) : int64 =
  let r = rem n m in
  if  r = zero then n else add n (sub m r)

let rec maxUnit(t: typ) : int =
  match unwrap t with
  | TypeInt(mm, "char")  -> 1
  | TypeInt(mm, "short") -> 2
  | TypeInt(mm, "int")   -> mmGetIntSize(mm)
  | TypeInt(mm, "long")  -> mmGetLongSize(mm)
  | TypePtr(mm, _)       -> mmGetPtrSize(mm)
  | TypeArray(t', _)     -> maxUnit(t')
  | TypeUnion(id)
  | TypeStruct(id) ->
      let se = symtabGetStruct id in
      let rec compute = function
        | [] -> 1
        | (_, t, _, _) :: fs -> max (maxUnit t) (compute fs) in
      compute (List.rev se.flds)
  | _ -> failwith "Unit size computation requires an external memory type"

let rec getTypeSize(t: typ) : int64 = 
  match unwrap t with
  | TypeInt(mm, "char")  -> of_int 1
  | TypeInt(mm, "short") -> of_int 2
  | TypeInt(mm, "int")   -> of_int (mmGetIntSize mm)
  | TypeInt(mm, "long")  -> of_int (mmGetLongSize mm)
  | TypePtr(mm, _)       -> of_int (mmGetPtrSize mm)
  | TypeArray(t', m)     -> mul (of_int m) (getTypeSize t')
  | TypeStruct(id)       -> let raw = getFieldOffset "$nofield" t in
                            alignInt64 raw (of_int(maxUnit t))
  | TypeUnion(id) ->
      let se = symtabGetStruct id in
      let rec compute (n: int64) = function
        | [] -> n
        | (id', t, _, _) :: fs -> compute (max n (getTypeSize t)) fs in
      let raw = compute zero (List.rev se.flds) in
      alignInt64 raw (of_int(maxUnit t))
  | _ ->  failwith "Type size computation requires an external memory type"

and getFieldOffset(f: ident) (t: typ) : int64 = 
  match unwrap t with
  | TypeUnion _ -> zero
  | TypeStruct(sid) -> 
      let se = symtabGetStruct sid in
      let rec compute (n: int64) = function
        | [] -> n
        | (f', t, _, o) :: rest ->
            let n = if o <> minus_one 
                    then checkOffset n o f
                    else alignInt64 n (of_int(maxUnit t))
            in
            if f = f' then n
            else compute (add n (getTypeSize t)) rest in
      compute zero (List.rev se.flds)
  | _ -> failwith "Not reached"

and checkOffset (n: int64) (o: int64) (f: ident) : int64 =
  if (compare o n) < 0 then
    failwith ("Non-monotonic offset for field: " ^ f)
  else
    o

let getIdType(id: ident) : typ =
  try unwrap (symtabLookupVarType id)
  with Not_found -> 
    if !autoDeclareVars 
    then (symtabVarDecl id typeInt true; typeInt)
    else raise (NotDeclared id)

let getRetType(fid: ident) : typ =
  match (symtabGetFunc fid).ftyp with
  | TypeFunc(r, _) -> r
  | _ -> failwith "Not reached"

let getParmTypes(fid: ident) : typ list =
  match (symtabGetFunc fid).ftyp with
  | TypeFunc(_, l) -> l
  | _ -> failwith "Not reached"

let typeIsInt(t: typ) : bool =
  match unwrap(t) with
  | TypeInt _ -> true
  | _ -> false

let typeIsArray(t: typ) : bool =
  match unwrap(t) with
  | TypeArray _-> true
  | _ -> false

let typeIsIntLike(t: typ) : bool =
  match unwrap(t) with
  | TypeInt _ | TypePtr _ | TypeArray _ -> true
  | _ -> false

let typeIsPtrOrArray(t: typ) : bool =
  match unwrap(t) with
  | TypePtr _ | TypeArray _-> true
  | _ -> false

let typeIsStrOrCharPtr(t: typ) : bool =
  match unwrap(t) with
  | TypeString -> true
  | TypePtr(_, TypeInt(_, "char"))
  | TypeArray(TypeInt(_, "char"), _) -> true
  | _ -> false

let checkAssignTypes(left: typ) (right: typ) : typ =
  let assignError t1 t2 = 
    raise (AssignError (", " ^ (typeToString t1) ^
                        " assigned to " ^ (typeToString t2))) in
  match unwrap(left), unwrap(right) with
  | (TypeInt _,  TypeInt _)
  | (TypeInt _,  TypePtr _)
  | (TypePtr _,  TypePtr _)
  | (TypePtr _,  TypeInt _)
  | (TypeString, TypeString) -> right
  | _ -> assignError right left

let rec exprTypeOk(e: expr) : unit =
  ignore(getExprType e)

and paramListOk (tl: typ list) (el: expr list): unit =
  match tl, el with
  | [] , [] -> ()
  | [TypeVarArgs], rest -> List.iter exprTypeOk rest 
  | t::ts, e::es -> let _ = checkAssignTypes t (getExprType e) in
                    paramListOk ts es
  | _ -> raise (TypeError "wrong number of arguments")

and getExprType(e: expr) : typ =
  let typeError() = raise (TypeError (exprToString e)) in
  let typeErrorMsg(msg) = raise (TypeError (msg ^ ": " ^ exprToString e)) in
  let rec getExprTypeRaw : expr -> typ = function
    | ExprStrConst _ -> TypeString
    | ExprIntConst _ -> typeInt
    | ExprIdent(id)  -> getIdType(id)
    | ExprAddr(e1)   -> TypePtr(!defaultMemModel, getExprTypeRaw e1)
    | ExprUnary(op, e1) ->
        let t1 = getExprType e1 in
        if (typeIsInt t1)
        then t1
        else typeError()
    | ExprBinary(op, e1, e2) ->
        let t1 = getExprType e1 in
        let t2 = getExprType e2 in
        if ((op = "==" || op = "!=" || 
             op = "<=" || op = "<" ||
             op = ">=" || op = ">") &&
            (typeIsIntLike t1) && (typeIsIntLike t2)) then
          typeInt
        else if (typeIsIntLike t1) && (typeIsIntLike t2) then
          t1
        else if ((op = "==" || op = "!=") &&
                 (t1 == TypeString) && (t2 == TypeString)) then
          typeInt
        else if (op = "+" && 
                 (t1 == TypeString) && (t2 == TypeString)) then
          TypeString
        else 
          typeError()
    | ExprCond(e, t, f) ->
        let t1 = getExprType e in
        let t2 = getExprType t in
        let t3 = getExprType f in
        if ((typeIsIntLike t1) && 
            (t2 = t3 || (typeIsIntLike t2) && (typeIsIntLike t3)))
        then t2 else typeError()
    | ExprArray(e1, e2) ->
        let t2 = getExprType e2 in
        (match getExprType e1 with
        | TypePtr(_, t)
        | TypeArray(t, _) -> 
            if typeIsInt t2 then t
            else typeError()
        | _ -> typeError()) 
    | ExprPointer(e1) ->
        (match getExprType e1 with
        | TypePtr(_, t)
        | TypeArray(t, _) -> t
        | _ -> typeError())
    | ExprField(b, e1, f) ->
        let e2 = if b then ExprPointer(e1) else e1 in
        (match getExprType e2 with
        | TypeStruct(id)
        | TypeUnion(id) -> 
            let _, t, _, _ = symtabLookupField id f in t
        | _ -> typeError())
    | ExprBag(id, e) -> 
        let tid = getIdType(id) in
        let te  = getExprType(e) in 
        (match tid, typeIsIntLike(te) with
        |  TypeBag _, true -> typeInt
        | _ -> typeError())
    | ExprAggr _ ->
        typeErrorMsg("Cannot read aggregates");
    | ExprAssign(id, e) ->
        (try checkAssignTypes (getIdType id) (getExprType e)
        with AssignError s -> typeErrorMsg("Assignment error" ^ s))
    | ExprAssignBag(id, e1, e2) -> 
        let tid = getIdType(id) in
        let t1  = getExprType(e1) in
        let t2  = getExprType(e2) in
        (match tid, typeIsIntLike(t1) with
        | TypeBag(_), true -> t2 
        | _ -> typeError())
    | ExprAssignAggr(id, l1, l2, e1) ->
        let chk f g x = if not (f(g x)) then typeError() in
        let _   = List.iter (chk typeIsIntLike getExprType) l1 in
        let _   = List.iter (chk typeIsStrOrCharPtr getExprType) l2 in
        let t1  = getExprType(e1) in
        let tid = getIdType(id) in
        let ta  = TypeAggr(List.length l1, List.length l2) in
        if tid = ta then t1
        else typeErrorMsg((typeToString tid) ^ " vs " ^ (typeToString ta))
    | ExprCall(id, el) -> 
        let tid = try (symtabGetFunc id).ftyp with 
                  Not_found -> failwith ("Function not defined: " ^ id) in
        (match tid with
        | TypeFunc(t, tl) ->
            (try paramListOk tl el; t with
            | AssignError s -> typeErrorMsg("Parameter-passing error" ^ s)
            | TypeError s -> typeErrorMsg s)
        | _ -> failwith "Not reached")
    | ExprComma(l) ->
        (match l with
        | []    -> failwith "Not reached"
        | [e]   -> getExprType(e)
        | e::es -> exprTypeOk(e); getExprType(ExprComma es))
    | ExprSizeOf _ -> typeInt
    | ExprCast(t, e) ->
        match getExprType(e) with
        | TypeAggr _ -> typeErrorMsg("Cannot cast an aggregate")
        | TypeBag  _ -> typeErrorMsg("Cannot cast a bag")
        | _ -> t
  in
  unwrap(getExprTypeRaw e)

let rec typePartitionAggrList(l: expr list) : expr list * expr list =
  match l with
  | [] -> [], []
  | e :: es -> let l1, l2 = typePartitionAggrList(es) in
               let t = getExprType(e) in
               if typeIsStrOrCharPtr(t)  then (l1, e::l2)
               else if typeIsIntLike(t) then (e::l1, l2)
               else failwith ("Aggregate index invalid: " ^ (exprToString e))

let getMemModel(t: typ) : mmident =
  match t with
  | TypeInt(mm, _)
  | TypePtr(mm, _) -> mm
  | _ -> raise Not_found

let rec statTypeOk : stat -> unit = function
  | StatEmpty           -> ()
  | StatExpr(e)
  | StatReturn(e)       -> exprTypeOk(e)
  | StatBlock l         -> List.iter statTypeOk l
  | StatIf(e, t)        -> let ty = getExprType e in
                           if not (typeIsInt ty)
                           then failwith "Type error in if condition"
                           else statTypeOk t
  | StatIfElse(e, t, f) -> let ty = getExprType e in
                           if not (typeIsInt ty)
                           then failwith "Type error in if condition"
                           else statTypeOk t; statTypeOk f

let returnOk(id: ident) (fe: funcEntry) : unit =
  if not fe.fpredef then
    let revBodyList =
      match fe.fbody with
      | StatBlock l -> List.rev l
      | _ -> failwith "Not reached" in
    match fe.ftyp, revBodyList with
    | TypeFunc(TypeVoid, _), (StatReturn e)::_ ->
        failwith ("Return statement in function returning void: " ^ id)
    | TypeFunc(TypeVoid, _), _ -> ()
    | TypeFunc(t, _), (StatReturn e)::_ ->
        (try ignore(checkAssignTypes t (getExprType e))
        with AssignError _ -> 
          failwith ("Function " ^ id ^ " must should return: " ^ 
                    (typeToString t) ^
                    ", but returns: " ^
                    (typeToString(getExprType e))))
    | TypeFunc(t, _), _ ->
        failwith ("Missing return statement in function: " ^ id)
    | _ -> failwith "Not reached"

let structOk(id, se) =
  if not se.sdefined then
    failwith ("Structure declared, but not defined: " ^ id)

(*
 * typeCheckPass -- type-check the bodies of all probes and functions.
 * Also check return type consistency for functions, and check that
 * all structures are defined.
 *)
let typeCheckPass() : unit = 
  try
    let checkFunc(id, fe)  = returnOk id fe;
                             statTypeOk(fe.fbody) in
    let checkProbe(id, pe) = statTypeOk(pe.pbody) in
    if !verbose then Printf.printf "# Type-checking...\n";
    autoDeclareVars := true;
    List.iter structOk tab.structs;
    compilerPass checkFunc checkProbe;
    autoDeclareVars := false
  with
    TypeError(s) -> Printf.eprintf "Type error: %s\n" s; exit 1
