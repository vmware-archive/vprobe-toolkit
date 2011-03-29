(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Lexer.mll: the lexical specification for Emmett. This is mostly a
 * mechanical translation of the lexical analysis appendix of the C
 * ISO standard, with a few Emmett-specific modifications: we added a
 * few keywords, we removed storage class specifiers, we tolerate :
 * and ! in identifier names, and we don't support floating point.
 *)

{
open Parser
open Printf
open Symtab
open Int64
open Char

let incLine(lexbuf) =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

let parseNumber(s: string) : Int64.t =
  try
    of_string(s)
  with Failure _ ->
    failwith ("Not a valid 64-bit integer: " ^ s)

(*
 * Int64 uses signed integers. Trick it into accepting large unsigned
 * 64-bit values.
 *)
let parseDecimal(s: string) : Int64.t =
  try
    of_string(s)
  with Failure _ ->
    let n  = (String.length s) - 1 in
    let hi = String.sub s 0 n in
    let lo = String.sub s n 1 in
    try 
      let res = add (of_string lo) (mul (of_string hi) (of_int 10)) in
      if Int64.compare res zero >= 0 then raise (Failure "") else res 
    with Failure _ ->
      failwith ("Not a valid 64-bit integer: " ^ s)

let notSupported(s: string) =
  failwith ("Keyword not supported: " ^ s)

let reservedVP(s: string) =
  failwith ("VP-reserved keyword: " ^ s)
} 

let D   = ['0'-'9']
let L   = ['a'-'z''A'-'Z''_']
let H   = ['a'-'f''A'-'F''0'-'9']

let OCT = '0'
let HEX = '0'['x''X']

let WS  = [' ''\t']
let COMM1 = '#'[^'\n']*
let COMM2 = '/''/'[^'\n']*

let IDENT = L(L|D|'!'|':')*
let STR   = '"' ([^'"'] | '\\' '"')* '"'

rule token = parse
 | WS         { token lexbuf }
 | '\n'       { incLine lexbuf; token lexbuf }

 | "else"     { ELSE }
 | "if"       { IF }
 | "return"   { RETURN }
 | "sizeof"   { SIZEOF }
 | "typedef"  { TYPEDEF }
 | "void"     { VOID }
 | "char"     { CHAR }
 | "int"      { INT }
 | "long"     { LONG }
 | "short"    { SHORT }
 | "signed"   { SIGNED }
 | "unsigned" { UNSIGNED }
 | "struct"   { STRUCT }
 | "union"    { UNION }
 | "enum"     { ENUM }
 | "const"    { CONST }

 | "string"   { STRING }
 | "bag"      { BAG }
 | "aggr"     { AGGR }
 | "memmodel" { MEMMODEL }

 | "for"
 | "do"
 | "while"
 | "switch"
 | "break"
 | "continue"
 | "case"
 | "default"
 | "float"
 | "double"
 | "goto"
 | "static"
 | "extern"
 | "register"
 | "auto"
 | "volatile" { notSupported(Lexing.lexeme lexbuf) }

 | ">>="      { RIGHT_ASSIGN }
 | "<<="      { LEFT_ASSIGN }
 | "+="       { ADD_ASSIGN }
 | "-="       { SUB_ASSIGN }
 | "*="       { MUL_ASSIGN }
 | "/="       { DIV_ASSIGN }
 | "%="       { MOD_ASSIGN }
 | "&="       { AND_ASSIGN }
 | "^="       { XOR_ASSIGN }
 | "|="       { OR_ASSIGN }
 | "<-"       { AGGR_ASSIGN }
 | ">>"       { RIGHT_OP }
 | "<<"       { LEFT_OP }
 | "++"       { INC_OP }
 | "->"       { PTR_OP }
 | "&&"       { AND_OP }
 | "||"       { OR_OP }
 | "<="       { LE_OP }
 | ">="       { GE_OP }
 | "=="       { EQ_OP }
 | "!="       { NE_OP }
 | "<"        { LT_OP }
 | ">"        { GT_OP }
 | ";"        { SEMI }
 | "{"        { LBRACE }
 | "}"        { RBRACE }
 | ","        { COMMA }
 | ":"        { COLON }
 | "="        { ASSIGN }
 | "("        { LPAREN }
 | ")"        { RPAREN }
 | "["        { LBRACK }
 | "]"        { RBRACK }
 | "."        { DOT }
 | "&"        { AMPERSAND }
 | "!"        { BANG }
 | "~"        { TILDA }
 | "-"        { MINUS }
 | "+"        { PLUS }
 | "*"        { STAR }
 | "/"        { DIV }
 | "%"        { MOD }
 | "^"        { CARET }
 | "|"        { BAR }
 | "?"        { QUESTION }
 | "@"        { AT }

 | IDENT as s { if symtabIsTypedef(s) then TYPE_NAME(s) else IDENT(s) }
 | STR   as s { STRCONST(s) }

 | OCT (D+ as s) { INTCONST(parseNumber ("0o" ^ s)) }
 | HEX (H+ as s) { INTCONST(parseNumber ("0x" ^ s)) }
 |     (D+ as s) { INTCONST(parseDecimal s) }

 | COMM1      { failwith ("Pound-style comments (#) are deprecated. " ^ 
                          "Use C-style comments.")}

 | COMM2      { token lexbuf }
 | "/*"       { comment lexbuf }

 | eof        { EOF }

 | _ as c     { let pos  = lexbuf.Lexing.lex_curr_p in
                let line = pos.Lexing.pos_lnum in
                eprintf "Lexer error: line %d: '%c'\n" line c;
                exit 1 }

and comment = parse
 | "*/"       { token lexbuf }
 | "\n"       { incLine lexbuf; comment lexbuf }
 | _          { comment lexbuf }
