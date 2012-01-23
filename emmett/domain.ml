(* **********************************************************
 * Copyright 2010 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************)

(*
 * Handling of domains and reachability analysis. For scripts that
 * span multiple domains, we traverse the AST and annotate functions
 * with all domains in which they are used. A function is used in
 * domain D if it is called directly or indirectly from a probe which
 * fires in domain D.
 *)

open Globals
open Defaults
open Ast
open Symtab
open Printf
open Scanf


(*
 * domainParts -- Separate letters from optional digits in domains or
 * environments.
 *)  
let domainParts(dom: string): (string * string) =
  try sscanf dom "%[A-Z]%[0-9]" (fun dom' num -> (dom', num))
  with _ -> (dom, "")

(*
 * envToDomain --
 * envFromDomain --
 * domainMatchesEnv --
 *
 * Functions to match probe domains and vprobe environments.
 * Domains VMM, VMX and GUEST are in the VM environment.
 * For VMK and POSIX, domain = environment.
 *)
let envToDomain(env: string) : string =
  match domainParts env with
  | ("VM", n) -> "VMM" ^ n
  | ("VMK", "") -> "VMK"
  | ("POSIX", "") -> "POSIX"
  | _ -> failwith ("Invalid environment: '" ^ env ^ "'")

let envFromDomain(dom: string): string =
  match domainParts dom with
  | ("VMM", n)
  | ("VMX", n)
  | ("GUEST", n)  -> "VM" ^ n
  | ("VMK", "")   -> "VMK"
  | ("POSIX", "") -> "POSIX"
  | _ -> failwith ("Invalid domain: '" ^ dom ^ "'")

let domainMatchesEnv(dom: string)(env: string) : bool = 
  env = (envFromDomain dom)

(*
 * domainInitDefaultDom -- initialize the default domain. If the
 * domain specifications consist of a single target, then that 
 * defines the default domain. Otherwise, the defaultDomain global
 * defines it to "VMK" for the ESX compiler or "VMM" for the 
 * hosted compiler.
 *)
let domainInitDefaultDom() : unit =
  if List.length tab.targs = 1 then
    tab.defdom <- envToDomain(fst(List.hd tab.targs))
  else
    tab.defdom <- !defaultDomain

(*
 * Returns the probe name for the one herz periodic probe.
 *)
let domainOneHzProbe(env: string): string =
  match domainParts env with
  | "VMK", "" -> "VMK:VMK1Hz"
  | "VM", n   -> "VMM" ^ n ^ ":VMM1Hz"
  | _ -> failwith ("No default periodic probe defined for '" ^ env ^"'")

(*
 * Returns the builtin variable for (V|P)CPU number.
 *)
let domainCPUVar(env: string): string =
  match domainParts env with
  | "VMK", "" -> "PCPU"
  | "VM", n   -> "VCPUID"
  | _ -> failwith ("No CPU id variable defined for '" ^ env ^"'")  

let domainTagVar(id: ident) (dom: string): unit =
  let (sc, t) = symtabLookupVar id in
  let env = envFromDomain(fst (domainParts dom)) in
  let pfx = match t with
    | TypeBag _ -> "Bag" | TypeAggr _ -> "Aggr" | _ -> "Variable"
  in
  match sc, env with
  | ClassPerVM, "VMK"
  | ClassPerVMK, "VM" ->
      failwith (sprintf "%s %s cannot be used in domain %s"
                        pfx id dom)
  | _ -> ()

let rec domainTagExpr(e: expr)(dom: string): unit =
  match e with
  | ExprIdent id ->
      domainTagVar id dom
  | ExprAddr(_, e)
  | ExprUnary(_, e)
  | ExprPointer(e)
  | ExprBag(_, e)
  | ExprField(_, e, _)
  | ExprCast(_, e)
  | ExprAssign(_, e)
      -> domainTagExpr e dom
  | ExprBinary(_, e1, e2)
  | ExprArray(e1, e2)
      -> domainTagExpr e1 dom;
         domainTagExpr e2 dom
  | ExprAssignBag(id, e1, e2) 
      -> domainTagVar id dom; 
         domainTagExpr e1 dom;
         domainTagExpr e2 dom
  | ExprCond(e1, e2, e3) 
      -> domainTagExprList [e1; e2; e3] dom
  | ExprCall(id, l) 
      -> domainTagFunc id dom; domainTagExprList l dom
  | ExprAggr(id, l1, l2) 
      -> domainTagVar id dom;
         domainTagExprList l1 dom;
         domainTagExprList l2 dom
  | ExprAssignAggr(id, l1, l2, e) 
      -> domainTagVar id dom;
         domainTagExprList (e :: l1) dom;
         domainTagExprList l2 dom
  | ExprComma(l) -> domainTagExprList l dom
  | ExprSizeOf   _
  | ExprStrConst _
  | ExprIntConst _ -> ()

and domainTagExprList(l: expr list)(dom: string): unit =
  List.iter (fun e -> domainTagExpr e dom) l

and domainTagStat(s: stat) (dom: string) : unit =
  match s with
  | StatExpr(e)
  | StatReturn(e) -> domainTagExpr e dom
  | StatBlock(l)  -> List.iter (fun s -> domainTagStat s dom) l
  | StatIf(e, s)  -> domainTagExpr e dom; domainTagStat s dom
  | StatIfElse(e, s0, s1)
      -> domainTagExpr e dom; domainTagStat s0 dom; domainTagStat s1 dom
  | _ -> ()

and domainTagFunc(id: ident) (dom: string): unit =
  let fe = symtabGetFunc id in
  if not (List.mem dom fe.fdoms) then
    (fe.fdoms <- dom :: fe.fdoms;
     symtabPushCtx(CtxFunc id);
     domainTagStat fe.fbody dom;
     symtabPopCtx())

let domainIsValid(dom: string): bool =
  (envFromDomain dom) = (envFromDomain tab.defdom) ||
  List.exists (fun(env,targ) -> domainMatchesEnv dom env) tab.targs

let domainFromProbename(id: ident): string =
  let str = try String.sub id 0 (String.index id ':')
            with Not_found -> tab.defdom in
  match fst(domainParts str) with
  | "VMM" | "VMX" | "GUEST" | "VMK" | "POSIX" -> str
  | _ -> tab.defdom (* Covers non-static probes without a domain prefix. *)

let domainTagProbe(id, pentry): unit =
  let addEnvSeen env =  if not (List.mem env tab.envsseen) then
    tab.envsseen <- tab.envsseen @ [env] in
  let dom = domainFromProbename (symtabProbeName id) in
  pentry.pdom <- dom;
  if not (domainIsValid dom) then
    failwith ("no target specified for probe " ^ 
              (symtabProbeName id) ^ " in domain " ^ dom);
  addEnvSeen (envFromDomain dom);
  domainTagStat pentry.pbody dom

(*
 * Traverse the AST of each probe and tag all functions that are
 * reachable from a probe with the domain of the probe.
 *)
let domainPass(): unit =
  let ignoreFuncs(id, fe) = () in
  ignore(envFromDomain tab.defdom); (* check defdom's validity. *)
  if !verbose then
    printf "# Splitting (default domain %s)...\n" tab.defdom;
  compilerPass ignoreFuncs domainTagProbe
