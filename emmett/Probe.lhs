Copyright 2007, VMware, Inc.

A "Probe" is a tree in the parse forest of an Emmett program: one of a
declaration, function definition, or probe definition.

> module Probe(Probe(ProbeStatement, ProbeFuncdef, ProbeDecl, ProbeTypedef),
>               emitProbe, checkProbe) where
> import Statement
> import Ident
> import Type
> import CompileCtx
> import Expr
> import Data.Maybe

> data Probe  = ProbeStatement Ident Statement |
>                ProbeFuncdef Type Ident CompileCtx Statement
>                (Maybe Expr) |
>                ProbeTypedef [(Type, Ident)] |
>                ProbeDecl [(Type, Ident)]
> instance Show Probe where
>     show (ProbeStatement id stmt) = "Probe: " ++ (show id) ++ "\n\t" ++
>                                      "stmt: " ++ (show stmt) ++ "\n"
>     show (ProbeDecl ((tp, id):xs)) = "Decl id: " ++ (show id) ++ "\n\t" ++
>                                      "type: " ++ (show tp) ++ "\n" ++
>                                      (show (ProbeDecl xs))
>     show (ProbeDecl [])  = "<none>"
>     show (ProbeTypedef ((tp, id):xs)) = "Definition of type: " ++ (show id)
>     show (ProbeFuncdef t id _ _ _) = "Definition of function: " ++ (show id)
> hdr = "\n ;;;;;\n(vprobe "
> emitProbe (ProbeStatement id stmt) ctx =  hdr ++ (emitIdent id) ++
>                                    " " ++ (emitStatement stmt ctx) ++ ")"
> emitProbe (ProbeFuncdef ty id locals blk ret) ctx =
>        "(defun " ++ (emitIdent id) ++ "(" ++ arguments ++ ")" ++
>                 (emitStatement blk ctx) ++
>              if (isJust ret) then
>                 (emitExpr (fromJust ret)) ++ ")"
>              else 
>                 ")"
>         where arguments = concatMap (\x -> (emitIdent x) ++ " ")
>                            (getVars locals) 
> emitProbe _ _ = ""

checkProbe -- perform a semantic check of the given probe

> checkProbe :: Probe -> CompileCtx -> Either String String
> checkProbe (ProbeStatement id stmt) ctx = checkStatement stmt ctx
> checkProbe (ProbeFuncdef ty id locals blk ret) ctx =
>    case (checkStatement blk nestedCtx) of
>       Left e  -> Left e
>       Right _ -> if (isJust ret) then
>                     if ((exprType (fromJust ret) nestedCtx) == ty) then
>                        Right "ok"
>                     else  Left ("return type of " ++ (show id) ++
>                                  "() is defined to be " ++ (show ty) ++
>                                  " but is returning '" ++
>                                  (show (fromJust ret)) ++ "' of " ++
>                                  (show (exprType (fromJust ret)
>                                          nestedCtx)) ++ "\n")
>                  else 
>                     if (ty == TypeVoid) then Right "ok"
>                     else Left ("return type of " ++ (show id) ++
>                                "() should be void but is " ++ (show ty) ++
>                                "\n")
>    where nestedCtx = pushCtx ctx locals
> checkProbe _ ctx = Right "ok"
