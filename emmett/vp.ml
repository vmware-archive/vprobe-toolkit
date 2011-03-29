(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Vp module: defines the structure of VP syntax trees; provides
 * conversion functions from Emmett expressions to VP expressions; and
 * provides a pretty-printer for VP expressions. The latter is used to
 * emit VP code.
 *)

open Defaults
open Globals
open Ast
open Symtab
open Type
open Predef
open Printf

type vpExpr =
  | VPString    of string
  | VPInt       of int64
  | VPIdent     of ident
  | VPOp        of string * (vpExpr list)
  | VPCond      of (vpExpr * vpExpr) list

let one = VPInt(Int64.one)

let emitIndented = ref true

(* Operators that vp can fold fron left to right. *)
let foldingOps = ["+"; "-"; "*"; "/"; "|"; "&"; "^";
                   ">"; "<"; ">="; "<="; "=="; "!="]
                   
(* Associative operators. These can also fold from right to left. *)
let assocOps = ["+"; "*"; "|"; "&"; "^"]

(*
 * Pretty-printing VP expressions.
 *)
let rec height : vpExpr -> int = function
  | VPString _
  | VPInt _
  | VPIdent _              -> 0
  | VPOp(_, l)             -> 1 + heightList(l)
  | VPCond(l)              -> 2 + heightList2(l)

and heightList(l) = 
  let func a e = max a (height e) in
  List.fold_left func 0 l

and heightList2(l) =     
  let func a (e1, e2) = max a (max (height e1) (height e2)) in
  List.fold_left func 0 l

let rec vpToString (n: int) (e: vpExpr) : string =
  let saveIndented = !emitIndented in
  let _ = if height(e) <= !indentLevel 
          then emitIndented := false in
  let result = 
  match e with
  | VPString(s) -> indent n s
  | VPInt(m)    -> indent n (Int64.to_string m)
  | VPIdent(id) -> indent n id
  | VPCond(l)   -> let pair(e1, e2) = VPOp("", [e1; e2]) in
                   vpToString n (VPOp("cond", List.map pair l))
  | VPOp("$mask", [VPInt m; e]) 
                -> vpToString n (VPOp(mask m, [e]))
  | VPOp("$strcmp", l) 
                -> vpToString n (VPOp("strcmp", l))
  | VPOp(op, l) -> let op = if op <> "" then op ^ " " else "" in
                   let list = List.map (vpToString (n + 1)) l in
                   (indent n ("(" ^ op)) ^ 
                   (String.concat (sep ()) list) ^
                   (indent n ")") in
  if saveIndented && not (!emitIndented)
  then (emitIndented := true; indent n result)
  else result

and mask(n: int64) : string =
  if n = Int64.zero then "& 0x" else (mask (Int64.pred n)) ^ "ff"

and indent (n: int) (s: string) : string = 
  match !emitIndented, n with
  | false, _ -> s
  | true,  0 -> s ^ "\n"
  | true,  _ -> "   " ^ (indent (n - 1) s)

and sep() : string =
  if !emitIndented then "" else " "


(*
 * simplifyVP -- perform simplifications of vp expressions. These
 * include taking advantage of multi-argument operators and cond
 * expressions, i.e., switches:
 *    (op (op e1 e2) e3)  -> (op e1 e2 e3)  if op is folding
 *    (op e1 (op e2 e3))  -> (op e1 e2 e3)  if op is associative
 *    (cond ((c1, e1) (1, (cond ((c2, e2) .. ))))) -> 
 *         (cond ((c1, e1) (c2, e2) .. ))
 * Multi-argument operators and switch statements are not available in
 * emmett, so these transformations cannot be done earlier.
 *)
let rec simplifyVP(e: vpExpr) : vpExpr = 
  match e with
  | VPCond(l) ->
      let simplifyVPpair(e1, e2) = (simplifyVP e1, simplifyVP e2) in
      (match List.map simplifyVPpair l with
      | [c1, e1; c2, VPCond(l2)] when c2 = one -> VPCond((c1, e1) :: l2)
      | l1 -> VPCond(l1))

  | VPOp(op, l) ->
      let isFolding(o) = o = op && List.mem o foldingOps in
      let isAssoc(o)   = o = op && List.mem o assocOps in
      (match List.map simplifyVP l with
      | VPOp(op1, l1) :: l2 when isFolding(op1) -> VPOp(op, l1 @ l2)
      | [e1; VPOp(op2, l2)] when isAssoc(op2)   -> VPOp(op, e1 :: l2)
      | l1 -> VPOp(op, l1))

  | VPString _
  | VPInt _
  | VPIdent _ -> e


(*
 * renameId -- a function that renames variable and function names.
 * It renames locals when promoting them to the global scope; renames
 * probe arguments to ARG*; and renames all variables and functions
 * that belong to the VP name space. All other names are returned
 * unchanged.
 *)
let renameId(id: ident) : ident =
  match symtabLookupLocal(id) with
  | Some("func",  "local", n) -> sprintf "~flocal%d:%s" n id
  | Some("probe", "local", n) -> sprintf "~plocal%d:%s" n id
  | Some("probe", "parm", n)  -> sprintf "ARG%d" n
  | _ when isReservedName(id) -> sprintf "~%s" id
  | _ -> id


(*
 * exprToVP -- convert an Emmett expression to a VP expression.
 *)
let rec exprToVP : expr -> vpExpr = function
  | ExprStrConst(s)         -> VPString(s)
  | ExprIntConst(n)         -> VPInt(n)
  | ExprIdent(id)           -> VPIdent(renameId id)
  | ExprUnary(op, e)        -> VPOp(op, [exprToVP e])
  | ExprBinary(op, e1, e2)  -> VPOp(op, [exprToVP e1; exprToVP e2])
  | ExprCond(c, t, f)       -> VPCond[exprToVP c, exprToVP t; one, exprToVP f]
  | ExprCall(id, l)         -> VPOp(renameId id, List.map exprToVP l)
  | ExprBag(id, e)          -> VPOp("bagremove", [VPIdent(renameId id);
                                                  exprToVP e])
  | ExprAssignBag(id,e1,e2) -> VPOp("baginsert", [VPIdent(renameId id);
                                                  exprToVP e1; exprToVP e2])
  | ExprAssignAggr(id, l1, l2, e1) -> 
                               VPOp("aggr", [VPIdent(renameId id);
                                             VPOp("", List.map exprToVP l1); 
                                             VPOp("", List.map exprToVP l2);
                                             exprToVP e1])
  | ExprAssign(id, e1)      -> let op = getAssignName(getIdType id) in
                               VPOp(op, [VPIdent(renameId id); exprToVP e1])
  | ExprComma(l)            -> VPOp("do", List.map exprToVP l)
  | e -> failwith ("Expression not lowered: " ^ (exprToString e))

and splitAggrList(l: expr list) : (vpExpr list) * (vpExpr list) =
  let l1, l2 = typePartitionAggrList l in
  (List.map exprToVP l1, List.map exprToVP l2)

and getAssignName : typ -> string = function
  | TypePtr _
  | TypeInt _  -> "setint"
  | TypeString -> "setstr"
  | t -> failwith ("Invalid type for setint/setstr: " ^ (typeToString t))

let rec statToVP : stat -> vpExpr = function
  | StatEmpty           -> one
  | StatExpr(e)
  | StatReturn(e)       -> exprToVP(e) 
  | StatBlock([])       -> one
  | StatBlock([s])      -> statToVP(s)
  | StatBlock(l)        -> VPOp("do", List.map statToVP l)
  | StatIf(c, t)        -> VPCond[exprToVP c, statToVP t]
  | StatIfElse(c, t, f) -> VPCond[exprToVP c, statToVP t; one, statToVP f]

(* 
 * Auto-aggregation.
 *)
let autoAggregate() : unit =
  let test = ExprBinary("==", ExprIdent defaultCpuVar, exprIntConst 0) in
  let foldFunc acc (id, ve) = 
    match ve.vtyp with
    | TypeAggr _ -> StatExpr(ExprCall("logaggr",   [ExprIdent id])) :: 
        StatExpr(ExprCall("clearaggr", [ExprIdent id])) :: acc
    | _ -> acc in
  let list = List.fold_left foldFunc []  tab.vars in
  let auto = StatIf(test, StatBlock list) in
  ignore(symtabProbeInsert default1HzProbe [] auto)

(*
 * Emit VP code for all functions and probes.
 *)
let vpEmitVar(id, ventry) : unit =
  let id = renameId id in
  if not ventry.vpredef then
    match ventry.vtyp, ventry.init with
    | TypePtr _, None
    | TypeInt _, None     -> printf "(definteger %s)\n" id
    | TypePtr _, Some(ExprIntConst n) 
    | TypeInt _, Some(ExprIntConst n) 
                          -> printf "(definteger %s %s)\n" id 
                                    (Int64.to_string n)
    | TypeString, None    -> printf "(defstring %s)\n" id
    | TypeString, Some(ExprStrConst s)
                          -> printf "(defstring %s %s)\n" id s
    | TypeBag(n), None    -> printf "(defbag %s %d)\n" id n
    | TypeAggr(i,s), None -> printf "(defaggr %s %d %d)\n" id i s
    | _ -> failwith "Not reached"

let vpEmitLocalsFunc(id, fentry) : unit =
  if not fentry.fpredef then
    List.iter vpEmitVar (List.rev fentry.flocals)

let vpEmitLocalsProbe(id, pentry) : unit =
  List.iter vpEmitVar (List.rev pentry.plocals)

let vpEmitFunc(id, fentry) : unit =
  if not fentry.fpredef then
    let func(id, _) = renameId(id) in
    let parms = String.concat " " (List.map func fentry.fparms) in
    let vp    = statToVP fentry.fbody in
    let body  = vpToString 1 (simplifyVP vp) in
    printf "(defun %s (%s)\n%s)\n" (renameId id) parms body

let vpEmitProbe(id, pentry) : unit =
  let vp   = statToVP pentry.pbody in
  let body = vpToString 1 (simplifyVP vp) in
  printf "(vprobe %s\n%s)\n" (symtabProbeName id) body

let vpEmitPass() : unit =
  if !verbose then printf "# Emitting VP code...\n\n";
  printf "(version %s)\n" version;
  List.iter vpEmitVar (List.rev Symtab.tab.vars);
  compilerPass vpEmitLocalsFunc vpEmitLocalsProbe;
  if !autoAggr then autoAggregate();
  compilerPass vpEmitFunc vpEmitProbe
    
