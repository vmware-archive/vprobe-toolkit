(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Ast module: defines the structure of ASTs nodes: expressions,
 * statements, and types. Provides pretty-printing functions for AST
 * nodes, as well as a few other utility functions.
 *)

open Memmodel
open Int64
open Printf

type int64 = Int64.t (* 64-bit integers.    *)
type ident = string  (* Identifiers.        *)
type mm    = string  (* Memory model names. *)
type op    = string  (* Operator names.     *)

type qual =
  | QUAL_CONST
  | QUAL_VOLATILE

type storage =
  | ClassTypedef
  | ClassExtern
  | ClassStatic
  | ClassAuto
  | ClassRegister

type tyspec =
  | SpecSign     of string
  | SpecInt      of string
  | SpecStruct   of string
  | SpecUnion    of string
  | SpecEnum     of string
  | SpecTypeName of string
  | SpecBag
  | SpecAggr
  | SpecString
  | SpecVoid

type specs = qual list * storage list * tyspec list

(*
 * Internal memory types: int, ptr, string, bag, aggr.
 * External memory types: int, ptr, array, struct, union.
 * Memory model tagging:
 * - on int: describes width and alignment only.
 * - on ptr: describes width, alignment, and accessor function.
 *)
type typ =
  | TypeString
  | TypeBag        of int
  | TypeAggr       of int * int
  | TypeInt        of mm * string
  | TypePtr        of mm * typ
  | TypeArray      of typ * int
  | TypeStruct     of ident
  | TypeUnion      of ident
  | TypeFunc       of typ * typ list
  | TypeEnum       of ident
  | TypeVoid
  | TypeVarArgs
  | TypeName       of ident

type expr =
  | ExprStrConst   of string
  | ExprIntConst   of int64
  | ExprIdent      of ident
  | ExprAddr       of expr
  | ExprUnary      of op * expr
  | ExprBinary     of op * expr * expr
  | ExprCond       of expr * expr * expr
  | ExprArray      of expr * expr
  | ExprPointer    of expr
  | ExprField      of bool * expr * string
  | ExprBag        of ident * expr
  | ExprAggr       of ident * (expr list) * (expr list)
  | ExprAssign     of ident * expr
  | ExprAssignBag  of ident * expr * expr
  | ExprAssignAggr of ident * (expr list) * (expr list) * expr
  | ExprCall       of ident * (expr list)
  | ExprComma      of expr list
  | ExprSizeOf     of typ
  | ExprCast       of typ * expr

type stat =
  | StatEmpty
  | StatExpr       of expr
  | StatReturn     of expr
  | StatBlock      of stat list
  | StatIf         of expr * stat
  | StatIfElse     of expr * stat * stat

(*
 * Pretty-printing ASTs: expressions, statements, types.
 *)
let rec exprToString : expr -> string = function
  | ExprStrConst(s)        -> s
  | ExprIntConst(n)        -> Int64.to_string n
  | ExprIdent(id)          -> id
  | ExprAddr(e)            -> "& " ^ (exprToString e)
  | ExprUnary(op, e)       -> "(" ^ op ^ (exprToString e) ^ ")"
  | ExprBinary(op, e1, e2) -> "(" ^ (exprToString e1) ^ " " ^ op ^
                              " " ^ (exprToString e2) ^ ")"
  | ExprCond(e1, e2, e3)   ->  "("   ^ (exprToString e1) ^
                               " ? " ^ (exprToString e2) ^
                               " : " ^ (exprToString e3) ^ ")"
  | ExprArray(e1, e2)      -> (exprToString e1) ^ "[" ^ (exprToString e2) ^ "]"
  | ExprBag(id, e)         -> id ^ "{" ^ (exprToString e) ^ "}"
  | ExprAggr(id, l1, l2)   -> id ^ "{" ^ (exprListToString l1) ^ "; "
                                       ^ (exprListToString l2) ^ "}"
  | ExprAssign(id, e)      -> "(" ^ id ^ " = " ^ (exprToString e) ^ ")"
  | ExprAssignBag(id, e1, e2) ->
                              "(" ^ id ^ "[" ^ (exprToString e1) ^ "] <- "
                                             ^ (exprToString e2) ^ ")"
  | ExprAssignAggr(id, l1, l2, e) ->
                              "(" ^ id ^ "[" ^ (exprListToString l1) ^ ";" ^
                              (exprListToString l2) ^ "] <- " ^ 
                              (exprToString e) ^ ")"
  | ExprCall(id, l)        -> id ^ "(" ^ (exprListToString l) ^ ")"
  | ExprComma(l)           -> (exprListToString l)
  | ExprField(b, e, s)     -> (exprToString e) ^ (if b then "->" else ".") ^ s
  | ExprSizeOf t           -> "sizeof (" ^ (typeToString t) ^ ")"
  | ExprCast(t, e)         -> "(" ^ (typeToString t) ^ ")" ^ (exprToString e)
  | ExprPointer e          -> "*" ^ (exprToString e)

and statToString (n: int) : stat -> string = function
  | StatEmpty              -> indent n ";\n"
  | StatExpr e             -> indent n ((exprToString e) ^ ";\n")
  | StatReturn(e)          -> indent n ("return " ^ (exprToString e) ^ ";\n")
  | StatBlock l            -> (indent n "{\n") ^ (statListToString (n+1) l) ^
                              (indent n "}\n")
  | StatIf(e,s)            -> (indent n "if (") ^ (exprToString e) ^ ")\n" ^
                              (statToString (n+1) s)
  | StatIfElse(e, s1, s2)  -> (indent n "if (") ^ (exprToString e) ^ ")\n" ^
                              (statToString (n+1) s1)  ^ (indent n "else\n") ^
                              (statToString (n+1) s2)

and typeToString : typ -> string = function
  | TypeString      -> "string"
  | TypeBag(n)      -> "bag[" ^ (string_of_int n) ^ "]"
  | TypeAggr(m, n)  -> "aggr[" ^ (string_of_int m) ^
                          ", " ^ (string_of_int n) ^ "]"

  | TypeFunc(t, l)  -> (if l = [] then "void" else typeListToString l) ^
                              " -> " ^ (typeToString t)
  | TypePtr(mm, t)  -> "ptr(" ^ (typeToString t) ^ ")"
  | TypeInt(mm, s)  -> "int" ^ (string_of_int (8 * (mmGetSize mm s)))
  | TypeArray(t, n) -> "array[" ^ (typeToString t) ^ ", " ^
                                  (string_of_int n) ^ "]"
  | TypeStruct(id)  -> "struct " ^ id
  | TypeUnion(id)   -> "union " ^ id

  | TypeEnum id    -> "enum " ^ id
  | TypeName id    -> id
  | TypeVarArgs    -> "..."
  | TypeVoid       -> "void"

and exprListToString(l)    = String.concat ", " (List.map exprToString l)
and statListToString(n)(l) = String.concat ""   (List.map (statToString n) l)
and typeListToString(l)    = String.concat ", " (List.map typeToString l)
and qualListToString(l)    = String.concat " "  (List.map qualToString l)
                                          
and qualToString = function
  | QUAL_CONST    -> "const"
  | QUAL_VOLATILE -> "volatile"

and indent n s = if n = 0 then s else "   " ^ indent (n - 1) s

let specToString : tyspec -> string = function
  | SpecSign s
  | SpecInt s
  | SpecTypeName s -> s
  | SpecStruct s   -> "struct " ^ s
  | SpecUnion s    -> "union " ^ s
  | SpecEnum s     -> "enum " ^ s
  | SpecBag        -> "bag"
  | SpecAggr       -> "aggr"
  | SpecString     -> "string"
  | SpecVoid       -> "void"

(*
 * exprBinary, exprUnary -- same as ExprBinary and ExprUnary, but fold 
 * constants. Not recursive, so constants in subtrees must be already folded.
 *)
let exprBinary(op, e1, e2) =
  match op, e1, e2 with
  | "/",  _, ExprIntConst n when n = zero -> failwith "Divide by zero" 
  | "%",  _, ExprIntConst n when n = zero -> failwith "Modulo by zero" 

  | "+",  _, ExprIntConst n when n = zero -> e1
  | "-",  _, ExprIntConst n when n = zero -> e1
  | "*",  _, ExprIntConst n when n = one  -> e1
  | "/",  _, ExprIntConst n when n = one  -> e1
  | "<<", _, ExprIntConst n when n = zero -> e1
  | ">>", _, ExprIntConst n when n = zero -> e1

  | "+",  ExprIntConst n, _ when n = zero -> e2
  | "*",  ExprIntConst n, _ when n = one  -> e2

  | "+",  ExprIntConst i, ExprIntConst j -> ExprIntConst(add i j)
  | "-",  ExprIntConst i, ExprIntConst j -> ExprIntConst(sub i j)
  | "*",  ExprIntConst i, ExprIntConst j -> ExprIntConst(mul i j)
  | "/",  ExprIntConst i, ExprIntConst j -> ExprIntConst(div i j)
  | "&",  ExprIntConst i, ExprIntConst j -> ExprIntConst(logand i j)
  | "|",  ExprIntConst i, ExprIntConst j -> ExprIntConst(logor  i j)
  | "^",  ExprIntConst i, ExprIntConst j -> ExprIntConst(logxor i j)
  | "<<", ExprIntConst i, ExprIntConst j 
                                 -> ExprIntConst(shift_left  i (to_int j))
  | ">>", ExprIntConst i, ExprIntConst j
                                 -> ExprIntConst(shift_right i (to_int j))
  | _  -> ExprBinary(op, e1, e2)

let exprUnary(op, e) =
  match op, e with
  | "+",  _ -> e
  | "-",  ExprIntConst i -> ExprIntConst(neg i)
  | "~",  ExprIntConst i -> ExprIntConst(lognot i)
  | "!",  ExprIntConst i -> ExprIntConst(if i = zero then zero else one)
  | _  -> ExprUnary(op, e)

(*
 * exprIntConst -- same as the ExprIntConst constructor, but takes an
 * int argument instead of int64.
 *)
let exprIntConst(n: int) : expr =
  ExprIntConst(Int64.of_int n)

(* 
 * A function that generates fresh names for anonymous structs and unions.
 *)
let structCounter = ref 0
let freshStructName () : string =
  structCounter := !structCounter + 1;
  "$" ^ (string_of_int !structCounter)

(* 
 * A function that generates fresh names for temporary variables.
 *)
let tempCounter = ref 0
let freshTempName () : string =
  tempCounter := !tempCounter + 1;
  "~tmp" ^ (string_of_int !tempCounter)
