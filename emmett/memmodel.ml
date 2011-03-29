(* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Memory model module: a memory model describes memory layout, i.e.,
 * alignment and width, for int, long, and pointer types. Only integer
 * and pointer types are tagged with a memory model. The layout of
 * structs, unions, and arrays follows from the the layout of their
 * inner integer and pointer components. For pointers, the memory
 * model additionally indicates the accessor function, used for
 * accessing the pointer's target memory.
 *)

type memmodel = { acc: string; i: int; l: int; p: int }
type mmident  = string 

let memModelMap = [
   "vmw32",     { acc="getvmw";       i=4; l=4; p=4 };
   "vmw64",     { acc="getvmw";       i=4; l=8; p=8 };
   "guest16",   { acc="getguest";     i=2; l=4; p=2 };
   "guest32",   { acc="getguest";     i=4; l=4; p=4 };
   "guest64",   { acc="getguest";     i=4; l=8; p=8 };
   "guestphys", { acc="getguestphys"; i=8; l=8; p=8 };
]

let mmValidMemModel(id: mmident) : bool =
    List.mem_assoc id memModelMap

let mmGetIntSize(id: mmident) : int =
    (List.assoc id memModelMap).i

let mmGetLongSize(id: mmident) : int =
    (List.assoc id memModelMap).l

let mmGetPtrSize(id: mmident) : int =
    (List.assoc id memModelMap).p

let mmGetAccessor(id: mmident) : string =
    (List.assoc id memModelMap).acc

let mmGetSize(id: mmident) (s: string) : int =
  match s with
  | "char"  -> 1
  | "short" -> 2
  | "int"   -> mmGetIntSize(id)
  | "long"  -> mmGetLongSize(id)
  | "ptr"   -> mmGetPtrSize(id)
  | _ -> failwith ("Invalid mem model size request: " ^ s)
