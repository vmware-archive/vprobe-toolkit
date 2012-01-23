(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Main module: processes command-line arguments, then calls other
 * modules to perform the remaining compilation steps: parse inputs,
 * type-check, lower expressions, and emit VP code.
 *)
 
open Globals
open Defaults
open Symtab
open Predef
open Type
open Lower
open Vp
open Domain
open Printf;;

type inputKind =
  | InputStdin
  | InputCmd  of string
  | InputFile of string

let inputs = ref []
let forceStdin = ref false

(*
 * parseCommandLine -- a function that parses the command line
 * arguments. If either one of the options -c, -i, -m, and -s occurs
 * more than once, then the last occurrence is the only one that counts.
 * If -E or -H are used to override the compiler defaults, then
 * set new defaults.
 *)
let parseCommandLine() =
  let defaultESX() =
    defaultDomain := "VMK";
    multidomainSupported := true
    in
  let defaultHosted() =
    defaultDomain := "VMM";
    multidomainSupported := false
    in
  let setCmd  cmd  = inputs := InputCmd(cmd)   :: !inputs in
  let setFile file = inputs := InputFile(file) :: !inputs in
  let printUsage() = eprintf "%s"
      ("Usage: emmett [-a] [-c <cmd>] [-i <num>] [-h] [<file1> ...]\n" ^
       "    -a        : Periodically log and clear aggregates\n" ^
       "    -c <cmd>  : Compile the given string\n" ^
       "    -f        : Force parsing stdin in addition to files \n" ^
       "    -i <num>  : Output indentation level (default=2)\n" ^
       "    -m <mem>  : Set the memory model (default=vmw64)\n" ^
       "    -s <file> : Guest symbol file (default=none)\n" ^
       "    -h        : Print this usage message\n");
      exit(0) in
  let specs = [ "-a",     Arg.Set(autoAggr), "";
                "-c",     Arg.String(setCmd), "";
                "-E",     Arg.Unit(defaultESX), "";
                "-H",     Arg.Unit(defaultHosted), "";
                "-f",     Arg.Set(forceStdin), "";
                "-i",     Arg.Set_int(indentLevel), "";
                "-m",     Arg.Set_string(defaultMemModel), "";
                "-s",     Arg.Set_string(symbolFile), "";
                "-v",     Arg.Set(verbose), "";
                "-h",     Arg.Unit(printUsage), "";
                "-help",  Arg.Unit(printUsage), "";
                "--help", Arg.Unit(printUsage), "";
              ]
  in
  (try Arg.parse_argv Sys.argv specs setFile ""
  with _ -> printUsage());
  if !inputs = [] || !forceStdin then
    inputs := InputStdin :: !inputs;
  inputs := List.rev !inputs

let codeLooksLikeFilename (kind: inputKind) (token: string) : bool =
  match kind with
  | InputCmd(cmd) -> Sys.file_exists(cmd) ||
                     Filename.check_suffix cmd "emt"
  | _ -> token = "/"

let filenameLooksLikeCode(fname: string) : bool =
  String.contains fname ';'

let openFile(file: string) : in_channel =
  try open_in file 
  with Sys_error(s)
    -> let extra = if filenameLooksLikeCode(file)
                   then " (expecting a file name, not code)" else ""
       in eprintf "Error: %s%s\n" s extra;
       exit 1

let getLexbuf : inputKind -> Lexing.lexbuf = function
  | InputStdin      -> Lexing.from_channel(stdin)
  | InputCmd(cmd)   -> Lexing.from_string(cmd)
  | InputFile(file) -> Lexing.from_channel(openFile file)

let inputName : inputKind -> string = function
  | InputStdin      -> "<stdin>"
  | InputCmd _      -> "<cmdline>"
  | InputFile(file) -> file

let line(lexbuf) : int =
  let pos = lexbuf.Lexing.lex_curr_p in 
  pos.Lexing.pos_lnum

let col(lexbuf) : int =
  let pos = lexbuf.Lexing.lex_curr_p in 
  let bol = pos.Lexing.pos_bol in
  pos.Lexing.pos_cnum - bol

let token(lexbuf) : string =
  Lexing.lexeme lexbuf

let parseInputs() =
  let parseInput(input) =
    let lexbuf = getLexbuf(input) in
    let name   = inputName(input) in
    if !verbose then printf "# Parsing %s...\n" name;
    try Parser.program Lexer.token lexbuf 
    with 
    | Parsing.Parse_error 
      -> let extra = if codeLooksLikeFilename (input) (Lexing.lexeme lexbuf)
                     then " (expecting code, not a file name)" else ""
         in eprintf "Syntax error: %s: line %d, col %d, token '%s'%s\n" 
                    name (line lexbuf) (col lexbuf) (token lexbuf) extra;
         exit 1
    | Failure msg 
      -> eprintf "Error: %s: line %d: %s\n" name (line lexbuf) msg;
         exit 1

  in
  List.iter parseInput !inputs;
  symtabPrintASTs();;

(* Main program. *)
try
  insertPredefs();
  parseCommandLine();
  parseInputs();
  typeCheckPass();
  domainPass();
  lowerPass();
  vpEmitPass()
with
  Failure s -> eprintf "Error: %s\n" s; exit 1
