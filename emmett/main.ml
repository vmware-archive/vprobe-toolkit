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
open Symtab
open Predef
open Type
open Lower
open Vp
open Printf;;

type inputKind =
  | InputStdin
  | InputCmd  of string
  | InputFile of string

let inputs = ref []

let parseCommandLine() =
  let setCmd  cmd  = inputs := InputCmd(cmd)   :: !inputs in
  let setFile file = inputs := InputFile(file) :: !inputs in
  let printUsage() = eprintf "%s"
      ("Usage: emmett [-a] [-c <cmd>] [-i <num>] [-h] [<file1> ...]\n" ^
           "    -a        : Periodically log and clear aggregates\n" ^
           "    -c <cmd>  : Compile the given string\n" ^
           "    -i <num>  : Output indentation level (default=2)\n" ^
           "    -m <mem>  : Default memory model (default=vmw64)\n" ^
           "    -s <file> : Guest symbol file (default=none)\n" ^
           "    -h        : Print this usage message\n");
      exit(0) in
  let specs = [ "-a",     Arg.Set(autoAggr), "";
                "-c",     Arg.String(setCmd), "";
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
  if !inputs = [] then inputs := [InputStdin];
  inputs := List.rev !inputs;
  symtabInitMemModel(!defaultMemModel)

let codeLooksLikeFilename : inputKind -> bool = function
  | InputCmd(cmd) -> Sys.file_exists(cmd) ||
                     Filename.check_suffix cmd "emt"
  | _ -> false

let filenameLooksLikeCode(fname: string) : bool =
  String.contains fname ';'

let openFile(file: string) : in_channel =
  try open_in file 
  with Sys_error(s)
    -> if filenameLooksLikeCode(file)
       then eprintf "Error: file name looks like code. Missing '-c'?\n"
       else eprintf "Error: %s\n" s;
       exit 1

let getLexbuf : inputKind -> Lexing.lexbuf = function
  | InputStdin      -> Lexing.from_channel(stdin)
  | InputCmd(cmd)   -> Lexing.from_string(cmd)
  | InputFile(file) -> Lexing.from_channel(openFile file)

let inputName : inputKind -> string = function
  | InputStdin      -> "<stdin>"
  | InputCmd _      -> "<cmdline>"
  | InputFile(file) -> file

let posInfo(lexbuf) : string =
  let pos    = lexbuf.Lexing.lex_curr_p in 
  let line   = pos.Lexing.pos_lnum in
  let bol    = pos.Lexing.pos_bol in
  let col    = pos.Lexing.pos_cnum - bol in
  let tok    = Lexing.lexeme lexbuf in
  sprintf "line %d, col %d, token '%s'" line col tok;;

let parseInputs() =
  let parseInput(input) =
    let lexbuf = getLexbuf(input) in
    let name   = inputName(input) in
    if !verbose then printf "# Parsing %s...\n" name;
    try Parser.program Lexer.token lexbuf 
    with Parsing.Parse_error 
      -> if codeLooksLikeFilename(input)
         then eprintf "Error: code looks like file name. Extra '-c'?\n"
         else eprintf "Syntax error: %s: %s\n" name (posInfo lexbuf);
         exit 1
  in
  List.iter parseInput !inputs;
  symtabPrintASTs();;

(* Main program. *)
try
  insertPredefs();
  parseCommandLine();
  parseSymbolFile();
  parseInputs();
  typeCheckPass();
  lowerPass();
  vpEmitPass()
with
  Failure s -> eprintf "Error: %s\n" s; exit 1
