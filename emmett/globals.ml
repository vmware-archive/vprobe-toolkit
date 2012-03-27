(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * **********************************************************)

(*
 * Globals module: just a few global variables.
 *)

(* Version number. *)
let version = "1.0"

(* Verbose output? *)
let verbose = ref false

(* Indentation level. *)
let indentLevel = ref 2

(* Auto-aggregate? *)
let autoAggr = ref false

(* Default memory model. *)
let defaultMemModel = ref "vmw64"

(* Guest symbol file *)
let symbolFile = ref ""
