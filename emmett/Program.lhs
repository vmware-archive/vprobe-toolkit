> module Program(Program(ProgramProbes), emitProgram, checkProgram) where
> import Probe
> import Statement
> import CompileCtx
> data Program = ProgramProbes [Probe]
> instance Show Program where
>   show (ProgramProbes probes) = foldr (++) "" (map show probes)

emitProgram -- Emit the program. Note that we start with a version
prologue indicating which version of vp is being emitted.

> emitProgram :: Program -> CompileCtx -> String
> emitProgram (ProgramProbes probes) ctx = "(version 0.2)\n" ++
>       (decls ctx) ++
>       foldr (++) "" (map (\x -> emitProbe x ctx) probes)

checkProgram -- semantic checker

> checkProgram :: Program -> CompileCtx -> Either String String
> checkProgram (ProgramProbes (x:xs)) ctx = case (checkProbe x ctx) of
>       Left e  -> Left e
>       Right b -> checkProgram (ProgramProbes xs) ctx
> checkProgram (ProgramProbes []) ctx = Right "ok"
