(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Lexer.mll: the lexical specification for Emmett. This is mostly a
 * mechanical translation of the lexical analysis appendix of the C
 * ISO standard, with a few Emmett-specific modifications: we added a
 * few keywords, we removed storage class specifiers, we tolerate !
 * in identifier names and we don't support floating point.
 *)

{
open Parser
open Printf
open Symtab
open Int64
open Char

(* 
 * incLine -- Helper function that increments the line count and
 * resets the beginning-of-line marker.
 *)
let incLine(lexbuf) =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

(* 
 * resetBol -- Helper function that resets the beginning-of-line marker.
 *)
let resetBol(lexbuf) =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_bol = pos.Lexing.pos_cnum;
 }

(* 
 * getLine -- Accessor for the line number.
 *)
let getLine(lexbuf) =
  let pos = lexbuf.Lexing.lex_curr_p in
  pos.Lexing.pos_lnum

(*
 * parseNumber -- Parses a 64-bit integer from a string. Returns a
 * friendly error message in case of errors.
 *)
let parseNumber(s: string) : Int64.t =
  try
    of_string(s)
  with Failure _ ->
    failwith ("Not a valid 64-bit integer: " ^ s)

(*
 * parseDecimal -- Int64 uses signed integers. Trick it into accepting
 * large unsigned 64-bit values.
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

(* Keeps track of nesting level for nested comments. *)
let commentNesting = ref 0
} 

let D   = ['0'-'9']
let L   = ['a'-'z''A'-'Z''_']
let H   = ['a'-'f''A'-'F''0'-'9']

let OCT = '0'
let HEX = '0'['x''X']

let WS  = [' ''\t']
let COMM1 = '#'[^'\n']*
let COMM2 = '/''/'[^'\n']*

let VMENV = "VM" D*
let ARG = [^'\n']*
let SPEC_ASSIGN = WS* "=" WS*
let EOL = WS* '\n'

let IDENT  = L(L|D|'!')*
let STR   = '"' ([^'\\' '"' '\n'] | '\\' [^'\n'])* '"'

let domain = "VMK" | "VMM" D* | "VMX" D* | "GUEST" D* | "POSIX"
let domainspec = domain ':'

let unsigned_number = (HEX H+) | D+
let pointsym  = L (L | D | ['!'  '.'  '@' '#'])*
let pointspec = pointsym | unsigned_number
let timerspec = D+ ("sec" | "msec" | "usec")

let PNAME =  domainspec? (
                   IDENT
                 | ("OFFSET:"   pointspec ':' unsigned_number)
                 | ("ENTER:"    pointspec)
                 | ("EXIT:"     pointspec)
                 | ("STEP:"     pointspec)
                 | ("READ:"     pointspec)
                 | ("WRITE:"    pointspec)
                 | ("TIMER:"    timerspec)
                 | ("PROFILE:"  timerspec))

(*
 * Recognize old style probe names so we can issue an error
 * message and abort compilation.
 *
 * The old name 'GUEST:xxx' is not recognized as and old
 * style probe name because it is also a new style probe name.
 *)
let old_pointspec = ((IDENT ':')? pointsym) | unsigned_number
let OLDPNAME = ("VMKERNEL:"      old_pointspec)
             | ("VMKERNEL_RET:"  old_pointspec)
             | ("VMKERNEL_DYN:"  old_pointspec ('+' unsigned_number)? )
             | ("VMKERNEL_STEP:" old_pointspec)
             | ("MONITOR:"       old_pointspec)
          (* | ("GUEST:"         old_pointspec) *)
             | ("GUEST_READ:"    old_pointspec)
             | ("GUEST_WRITE:"   old_pointspec)
             | ("USEC:"          D+)


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

 | "string"   { STRING }
 | "bag"      { BAG }
 | "aggr"     { AGGR }
 | "memmodel" { MEMMODEL }

 |  "assert"  { ASSERT(getLine(lexbuf)); }

 | "perthread" { PERTHREAD }
 | "pervm"     { PERVM }
 | "pervmk"    { PERVMK }
 | "perhost"   { PERHOST }

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
 | "const"
 | "bool"     (* reserve for future support of bool *)
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
 | PNAME as s { PNAME(s) }
 | OLDPNAME as s { failwith ("Old probe name no longer supported: " ^ s) }
 | STR   as s { STRCONST(s) }

 | OCT (D+ as s) { INTCONST(parseNumber ("0o" ^ s)) }
 | HEX (H+ as s) { INTCONST(parseNumber ("0x" ^ s)) }
 |     (D+ as s) { INTCONST(parseDecimal s) }

 | COMM1      { failwith ("Pound-style comments (#) are deprecated. " ^ 
                          "Use C-style comments.")}

 | COMM2      { token lexbuf }

 (* Option specifications.  *)
 | "@MEMORY_MODEL" SPEC_ASSIGN (ARG as model) EOL
     { resetBol(lexbuf); MEMMODEL_SPEC(model) }

 | "@SYMBOL_FILE"  SPEC_ASSIGN (ARG as file)  EOL
     { resetBol(lexbuf); SYMFILE_SPEC(file)   }

 (* Domain specifications.  *)
 | "@" "VMK" EOL
     { resetBol(lexbuf); TARGET_SPEC("VMK","") }

 | "@" (VMENV as env) SPEC_ASSIGN (ARG as tgt) EOL
     { resetBol(lexbuf); TARGET_SPEC(env, tgt) }

 | "@VM" [^'\n']* as s
     { failwith ("Invalid domain specification: " ^ s) }

 | "/*"       { commentNesting := !commentNesting + 1;
                comment lexbuf }

 | eof        { EOF }

 | _ as c     { let pos  = lexbuf.Lexing.lex_curr_p in
                let line = pos.Lexing.pos_lnum in
                eprintf "Lexer error: line %d: '%c'\n" line c;
                exit 1 }

and comment = parse
 | "/*"       { commentNesting := !commentNesting + 1;
                comment lexbuf }

 | "*/"       { commentNesting := !commentNesting - 1;
                if !commentNesting > 0 then comment lexbuf else token lexbuf }

 | "\n"       { incLine lexbuf; comment lexbuf }

 | eof        { let pos  = lexbuf.Lexing.lex_curr_p in
                let line = pos.Lexing.pos_lnum in
                eprintf "Lexer error: line %d: unterminated comment\n" line;
                exit 1 }

 | _          { comment lexbuf }
