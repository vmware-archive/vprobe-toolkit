(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Lower module: performs "expression lowering", i.e. translates
 * high-level expressions into low-level address arithmetic
 * computations. Lowered expressions include: field accesses, array
 * acceses, pointer arithmetic expressions, and char* aggregate
 * indexes.
 *)

open Globals
open Ast
open Symtab
open Type
open Memmodel
open Int64

(*
 * buildMemAccess -- create a memory access with the appropriate accessor
 * (getguest, getvmw, etc.) of the appropriate size for the expr's type.
 * A check is made that the size of the memory access will fit in a VpInt
 * (8 bytes) to guard against reading in large pieces of data (e.g. structs).
 *)
let buildMemAccess(mm: mmident) (e: expr) (t: typ) : expr =
  let size = getTypeSize(t) in 
  let accfunc = mmGetAccessor(mm) in
  let vpIntWidth = 8 in
  let expr = ExprCall(accfunc, [e]) in
  match compare size (of_int vpIntWidth) with
  | 0            -> expr
  | n when n < 0 -> exprBinary("$mask", ExprIntConst size, expr)
  | _ -> 
      failwith (Printf.sprintf
                  "Memory access: object too large (size %s, type %s, expr %s)"
                  (to_string size) (typeToString t) (exprToString e))

(*
 * Left, right -- mutually recursive functions that compute an
 * expression's lvalue and rvalue, respectively.
 *)
let rec right(e: expr) : expr =
  match e with
  | ExprStrConst _
  | ExprIntConst _
  | ExprIdent _             -> e

  | ExprCast(t, e1)         -> stringConv t e1

  | ExprAddr(e1)            -> snd(left e1)

  | ExprUnary(op, e1)       -> exprUnary(op, right e1)

  | ExprBinary(op, e1, e2) ->
      (match op, getExprType(e1) with
      | "+", TypePtr(_, t)
      | "-", TypePtr(_, t)
      | "+", TypeArray(t, _)
      | "-", TypeArray(t, _) ->
              let sz = ExprIntConst(getTypeSize t) in
              exprBinary(op, right e1, exprBinary("*", right e2, sz))

      | "+", TypeString ->
              let tmp  = freshTemp() in
              let fmt  = ExprStrConst("\"%s%s\"") in
              let args = [tmp; fmt; right e1; right e2] in
              ExprComma([ExprCall("sprintf", args); tmp])
              
      | "!=", TypeString
      | "==", TypeString ->
              let strcmp  = ExprCall("$strcmp", [right e1; right e2]) in
              exprBinary(op, strcmp, ExprIntConst(zero))

      |  _ -> exprBinary(op, right e1, right e2))
    
  | ExprCond(c, t, f)       -> ExprCond(right c, right t, right f)
  | ExprComma(l)            -> ExprComma(List.map right l)
  | ExprCall(id, al)        -> ExprCall(id, List.map right al)
  | ExprAssign(id, e1)      -> ExprAssign(id, right e1)
  | ExprAssignBag(id,e1,e2) -> ExprAssignBag(id, right e1, right e2)
  | ExprAssignAggr(id, l1, l2, e1)
                            -> let stringConv' = stringConv TypeString in
                               ExprAssignAggr(id, List.map right l1, 
                                              List.map stringConv' l2, right e1)
  | ExprBag(id, e1)         -> ExprBag(id, right e1)

  | _ when typeIsArray(getExprType e) -> snd(left e)

  | _ -> let mm, le = left(e) in 
         buildMemAccess mm le (getExprType e)

and left(e: expr) : mmident * expr =
  match e with
  | ExprCast(t, e1) -> left(e1)

  | ExprPointer(e1) -> getMemModel(getExprType e1), right(e1)

  | ExprField(b, e1, id) ->
      let e1 = if b then ExprPointer(e1) else e1 in
      let offset = getFieldOffset id (getExprType e1) in
      let mm, le1 = left(e1) in
      mm, exprBinary("+", le1, ExprIntConst offset)

  | ExprArray(e1, e2) ->
      let e1 = 
        match getExprType e1 with
        | TypePtr(_, t) -> ExprPointer(e1)
        | TypeArray(t, _) -> e1
        | _ -> failwith "Not reached" in
      let elemSize = ExprIntConst(getTypeSize(getExprType e)) in
      let offset   = exprBinary("*", right e2, elemSize) in
      let mm, le1 = left(e1) in
      mm, exprBinary("+", le1, offset)

  | e -> failwith ("Invalid lvalue: " ^ (exprToString e))

and freshTemp() =
  let id  = freshTempName() in
  symtabVarDecl id TypeString true;
  ExprIdent(id)

(*
 * stringConv -- a function that generates code to convert a char* or
 * char[] expression "e" to an emmett string, provided that type "t"
 * is string. Such string conversions are generated for cast 
 * expressions and aggregate indices.
 *)
and stringConv (t: typ) (e: expr) : expr =
  let magic(e) =  
    let mm, le = left(e) in
    let acc = (mmGetAccessor mm) ^ "str" in
    let tmp = freshTemp() in
    let e1 = ExprCall(acc, [tmp; exprIntConst(255); le]) in
    ExprComma[e1; tmp] 
  in
  match t, getExprType(e) with
  | TypeString, TypePtr(_, TypeInt(_, "char"))   -> magic(ExprPointer e)
  | TypeString, TypeArray(TypeInt(_, "char"), _) -> magic(e)
  | _ -> right(e)


(*
 * lowerExpr, lowerStat -- recursively walk syntax trees and replace
 * each use of an expression e with right(e). For statements, provide
 * the return type of the enclosing function.
 *)
let lowerExpr: expr -> expr = right

let rec lowerStat (t: typ) : stat -> stat = function
  | StatEmpty             -> StatEmpty
  | StatExpr(e)           -> StatExpr(lowerExpr e)
  | StatReturn(e)         -> StatReturn(lowerExpr e)
  | StatBlock(l)          -> StatBlock(List.map (lowerStat t) l)
  | StatIf(e, s1)         -> StatIf(lowerExpr e, lowerStat t s1)
  | StatIfElse(e, s1, s2) -> StatIfElse(lowerExpr e, 
                                        lowerStat t s1, lowerStat t s2)

(*
 * lowerPass -- lower the bodies of all probles and functions.
 *)
let lowerPass() : unit = 
  let lowerFunc(id, fe)  = fe.fbody <- lowerStat (getRetType id) fe.fbody in
  let lowerProbe(id, pe) = pe.pbody <- lowerStat TypeVoid pe.pbody in
  if !verbose then Printf.printf "# Lowering...\n";
  compilerPass lowerFunc lowerProbe

