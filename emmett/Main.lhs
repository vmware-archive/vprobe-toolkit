Copyright 2007-2011, VMware, Inc.

This is the main module of a simple compiler for Emmett, a small language
with C-like expressions, block structure, etc. This compiler emits vp
suitable for use with VMware, Inc.'s vprobe facility. Very little logic
exists in this file; we open an input file, parse command-line options,
and otherwise drive the parser found in the Parse module.

> module Main(main) where
> import Program
> import Probe
> import Statement
> import Expr
> import Ident
> import IdMap
> import CompileCtx
> import Type
> import Parse
> import Text.ParserCombinators.Parsec
> import IO
> import System
> import System.Console.GetOpt
> import Directory
> import List

The main program. We parse command-line arguments, then
check and show the output of the parser.

> main = do { (flags, files) <- (getArgs >>= parseArgs)
>           ; cmdLineStr <- findCmd flags
>           ; compOut <- parseEmmett cmdLineStr files
>           ; compOut <- checkEmmett compOut
>           ; emit compOut flags
>           }
>

Command-line parsing. Equivalent to 'ahc:' in getopt-ese.

> data Flag = Help | AutoAggr | Test | Cmd String
> flags = 
>   [Option ['a'] ["auto-aggregate" ] (NoArg AutoAggr)
>    "Automatically dump and clear all aggregating variables",
>    Option ['h'] ["help" ] (NoArg AutoAggr)
>    "Print usage",
>    Option ['c'] ["cmd" ] (ReqArg (\str -> Cmd str) "CMD")
>    "Specify an Emmett program on the command-line",
>    Option ['t'] ["test" ] (NoArg Test)
>    "Descend test subdirectory to check compiler consistency"]
> parseArgs :: [String] -> IO ([Flag], [String])
> parseArgs argv = case getOpt Permute flags argv of
>       (fs, args, []) -> if (testOpts fs) then
>                         (do runTests
>                             return (fs, args)) else
>                                                 return (fs, args)
>       (_, _, errs)   -> do hPutStr stderr (concat errs)
>                            exitWith (ExitFailure 1)
> findCmd :: [Flag] -> IO String
> findCmd [] = return ""
> findCmd ((Cmd str):xs) = return str
> findCmd (Help:xs) = do usage ExitSuccess
>                        return ""
> findCmd (x:xs) = findCmd xs
> autoAggr :: [Flag] -> Bool
> autoAggr [] = False
> autoAggr (AutoAggr:xs) = True
> autoAggr (x:xs) = autoAggr xs
> testOpts [] = False
> testOpts (Test:xs) = True
> testOpts (x:xs) = testOpts xs

parseEmmett: given parsed command-line, show the output.

> type CompileOutput = Either ParseError (Program, CompileCtx)
> nullProg = ProgramProbes []
> nullOut = Right (nullProg, defaultCtx)
> defaultCtx = makeCompileCtx (buildIdMap defaultIdMap)
>              (buildIdMap defaultTypeMap)
> endParse ex = do { usage ExitSuccess
>                  ; return nullOut }
> parseEmmett :: String -> [String] -> IO CompileOutput
> parseEmmett str fs = parseFilesAndStr str fs nullOut

Parse the files. Things are slightly complicated by our attempt to keep the
parser aware of what file we're in. It would be easier to just slurp
everything together into one string, but we'd lose the file name. We
basically cons up a pipeline of parser invocations, one per file. Each
one passes its output along to the next one.

parseFilesAndStr should parse files *first*, then the '-c' (if any); the
files might have routines that the '-c' calls.

> parseFilesAndStr s fs out@(Right (p, ctx)) =
>      parseFiles fs out >>= parseStr s
> concatProgs (ProgramProbes oldp)
>             (ProgramProbes newp) = ProgramProbes (oldp ++ newp)
> concatOuts (Left e) _ = Left e
> concatOuts _ (Left e) = Left e
> concatOuts (Right (p, ctx)) (Right (q, newctx)) = Right ((concatProgs p q),
>                                                          newctx)
> parseOneStr :: String -> String -> CompileOutput -> IO CompileOutput
> parseOneStr _ _ l@(Left e) = return l
> parseOneStr s fn (Right (p, ctx))  = return (runParser emmettParse ctx fn
>                                              s)
> parseStr :: String -> CompileOutput -> IO CompileOutput
> parseStr str out = do { newout <- parseOneStr str "*command-line*" out
>                       ; return (concatOuts out newout) }
> parseFile f out  = do { s <- readFile f
>                        ; newout <- parseOneStr s f out
>                        ; return (concatOuts out newout) }
> parseFiles :: [String] -> CompileOutput -> IO CompileOutput
> parseFiles _ l@(Left e) = return l
> parseFiles (f:fs) out@(Right p) = parseFile f out >>= parseFiles fs
> parseFiles [] out@_ = return out

checkEmmett -- Given a parse output and set of flags, perform a type check.

> checkEmmett :: CompileOutput -> IO CompileOutput
> failCheck act ex = do { act
>                       ; exitWith ex
>                       ; return nullOut }
> checkEmmett (Left e) = failCheck (hPutStr stderr
>                                   ("--- parse error: " ++ (show e)))
>                        (ExitFailure 1)
> checkEmmett pr@(Right (prog, ctx)) = case (checkProgram prog ctx) of
>                                      Left e  -> failCheck
>                                                 (do { hPutStr stderr
>                                                       ("--- type error: "
>                                                        ++ e ++ "\n")
>                                                     ; hPutStr stderr
>                                                       (show ctx)})
>                                                 (ExitFailure 1)
>                                      Right e -> return pr

emit -- Output the program.

> emit :: CompileOutput -> [Flag] -> IO ()
> emit p@(Left e) _ = do hPutStr stderr (show e)
>                        exitWith (ExitFailure 2)
> emit p@(Right (parseOut, ctx)) flags = do putStr (emitProgram parseOut ctx)
>                                           putStr (emitAutoAggr ctx flags)
> emitAutoAggr ctx flags =
>     if (autoAggr flags) then
>        concat ["\n;;;;;;;;\n(vprobe VMM1Hz ",
>               "(cond ((== VCPUID 0) (do ",
>                concatMap (\id -> concat["(logaggr ",
>                                         (emitIdent id),
>                                         ")\n(clearaggr ",
>                                         (emitIdent id),
>                                         ")\n"])
>                              aggrIds, "))))"]
>     else "" where
>             aggrIds = filter (\x -> (typeIsAggr (varType ctx x)))
>              (getVars ctx)

> usage exitVal = do { pn <-getProgName
>                    ; hPutStr stderr ("usage: " ++ pn ++ " <options> <inputfile>\n")
>                    ; hPutStr stderr (" -a: auto-aggregate\n")
>                    ; hPutStr stderr (" -c 'program': use command-line program\n")
>                    ; hPutStr stderr (" -h: print this message\n")
>                    ; exitWith exitVal
>                    }

defaultIdMap -- XXX. These are "built-in" functions that Emmett knows
about up-front. We should really slurp these out of a user-provided file.

> idMapFunc str ret params = (makeIdent str, TypeFunc ret params)
> idMapInt  str = (makeIdent str, (TypePreDef TypeInt))
> idMapStr  str = (makeIdent str, (TypePreDef TypeString))
> defaultIdMap = [ idMapFunc "printf" TypeVoid [TypeString, TypeEllipsis],
>                  idMapFunc "sprintf" TypeVoid [TypeString, TypeString,
>                                                TypeEllipsis],
>                  idMapFunc "strcmp" TypeInt [TypeString, TypeString],
>                  idMapFunc "logint" TypeVoid [TypeInt],
>                  idMapFunc "logstr" TypeVoid [TypeString],
>                  idMapFunc "logaggr" TypeVoid [TypeEllipsis],
>                  idMapFunc "clearaggr" TypeVoid [TypeEllipsis],
>                  idMapFunc "curprocname" TypeString [],
>                  idMapFunc "curpid" TypeInt [],
>                  idMapFunc "vmmstack" TypeVoid [TypeString, TypeEllipsis],
>                  idMapFunc "getmonitor" TypeInt [TypeInt],
>                  idMapFunc "gueststack" TypeVoid [TypeString, TypeEllipsis],
>                  idMapFunc "getguest" TypeInt [TypeInt],
>                  idMapFunc "getguestphys" TypeInt [TypeInt],
>                  idMapFunc "getgueststr" TypeVoid [TypeString, TypeInt],
>                  idMapFunc "getvmkernel" TypeInt [TypeInt],
>                  idMapFunc "getsystemtime" TypeVoid [TypeInt],
>                  idMapFunc "syscname" TypeVoid [TypeString, TypeInt],
>                  idMapFunc "offatret" TypeInt [TypeString],
>                  idMapFunc "offatstrcpy" TypeInt [TypeString],
>                  idMapFunc "offatseg" TypeInt [TypeString],
>                  idMapFunc "setstr" TypeVoid [TypeString, TypeString],
>                  idMapStr "PROBENAME",
>                  idMapInt  "CS",  idMapInt "SS",
>                  idMapInt  "DS",  idMapInt "ES",
>                  idMapInt  "FS",  idMapInt "GS",
>                  idMapInt  "RIP", idMapInt "RAX", idMapInt "RBX",
>                  idMapInt  "RCX", idMapInt "RDX", idMapInt "RSI",
>                  idMapInt  "RDI", idMapInt "RSP", idMapInt "RBP",
>                  idMapInt  "R8",  idMapInt "R9",  idMapInt "R10",
>                  idMapInt  "R11", idMapInt "R12", idMapInt "R13",
>                  idMapInt  "R14", idMapInt "R15",
>                  idMapInt  "TSC", idMapInt "PTSC",
>                  idMapInt  "TSC_HZ", idMapInt "PTSC_HZ",
>                  idMapInt  "VCPUID", idMapInt "EFER",
>                  idMapInt  "APIC_BASEPA", idMapInt "APICREGS",
>                  idMapInt  "CR0", idMapInt "CR2",
>                  idMapInt  "CR3", idMapInt "CR4",
>                  idMapInt  "CR8",
>                  idMapInt  "DR6", idMapInt "DR7",
>                  idMapInt  "ARG0", idMapInt "ARG1", idMapInt "ARG2",
>                  idMapInt  "ARG3", idMapInt "ARG4", idMapInt "ARG5",
>                  idMapInt  "ARG6", idMapInt "ARG7", idMapInt "ARG8",
>                  idMapInt  "ARG9",
>                  idMapInt "VMCS_EXIT_REASON", idMapInt "VMCS_EXIT_INTR_INFO",
>                  idMapInt "VMCS_EXIT_INTR_ERR", idMapInt "VMCS_CS",
>                  idMapInt "VMCS_CS_AR",      idMapInt "VMCS_CS_BASE",
>                  idMapInt "VMCS_CS_LIMIT",   idMapInt "VMCS_SS",
>                  idMapInt "VMCS_SS_AR",      idMapInt "VMCS_SS_BASE",
>                  idMapInt "VMCS_SS_LIMIT",   idMapInt "VMCS_DS",
>                  idMapInt "VMCS_DS_AR",      idMapInt "VMCS_DS_BASE",
>                  idMapInt "VMCS_DS_LIMIT",   idMapInt "VMCS_ES",
>                  idMapInt "VMCS_ES_AR",      idMapInt "VMCS_ES_BASE",
>                  idMapInt "VMCS_ES_LIMIT",   idMapInt "VMCS_FS",
>                  idMapInt "VMCS_FS_AR",      idMapInt "VMCS_FS_BASE",
>                  idMapInt "VMCS_FS_LIMIT",   idMapInt "VMCS_GS",
>                  idMapInt "VMCS_GS_AR",      idMapInt "VMCS_GS_BASE",
>                  idMapInt "VMCS_GS_LIMIT",   idMapInt "VMCS_EIP",
>                  idMapInt "VMCS_ESP",        idMapInt "VMCS_EFLAGS",
>                  idMapInt "VMCS_INSTRLEN",   idMapInt "VMCS_EXIT_QUAL",
>                  idMapInt "VMCS_IDTVEC_INFO", idMapInt "VMCS_IDTVEC_ERR",
>                  idMapInt "VMCS_VMENTRY_INTR_INFO",
>                  idMapInt "VMCS_VMENTRY_XCP_ERR",
>                  idMapInt "VMCS_IDTR_BASE",  idMapInt "VMCS_IDTR_LIMIT",
>                  idMapInt "VMCS_GDTR_BASE",  idMapInt "VMCS_GDTR_LIMIT",
>                  idMapInt  "VMCB_EXITCODE",  idMapInt "VMCB_EXITINFO1",
>                  idMapInt "VMCB_EXITINFO2",  idMapInt "VMCB_CS",
>                  idMapInt "VMCB_CS_ar",      idMapInt "VMCB_CS_base",
>                  idMapInt "VMCB_CS_limit",   idMapInt "VMCB_SS",
>                  idMapInt "VMCB_SS_ar",      idMapInt "VMCB_SS_base",
>                  idMapInt "VMCB_SS_limit",   idMapInt "VMCB_DS",
>                  idMapInt "VMCB_DS_ar",      idMapInt "VMCB_DS_base",
>                  idMapInt "VMCB_DS_limit",   idMapInt "VMCB_ES",
>                  idMapInt "VMCB_ES_ar",      idMapInt "VMCB_ES_base",
>                  idMapInt "VMCB_ES_limit",   idMapInt "VMCB_FS",
>                  idMapInt "VMCB_FS_ar",      idMapInt "VMCB_FS_base",
>                  idMapInt "VMCB_FS_limit",   idMapInt "VMCB_GS",
>                  idMapInt "VMCB_GS_ar",      idMapInt "VMCB_GS_base",
>                  idMapInt "VMCB_GS_limit",   idMapInt "VMCB_RIP",
>                  idMapInt "VMCB_RSP",        idMapInt "VMCB_RFLAGS",
>                  idMapInt "VMCB_CR3",        idMapInt "VMCB_EXITINTINFO",
>                  idMapInt "VMCB_EVENTINJ",   idMapInt "VMCB_GDTR",
>                  idMapInt "VMCB_GDTR_ar",    idMapInt "VMCB_GDTR_base",
>                  idMapInt "VMCB_GDTR_limit", idMapInt "VMCB_LDTR",
>                  idMapInt "VMCB_LDTR_ar",    idMapInt "VMCB_LDTR_base",
>                  idMapInt "VMCB_LDTR_limit", idMapInt "VMCB_IDTR",
>                  idMapInt "VMCB_IDTR_ar",    idMapInt "VMCB_IDTR_base",
>                  idMapInt "VMCB_IDTR_limit", idMapInt "VMCB_TR",
>                  idMapInt "VMCB_TR_ar",      idMapInt "VMCB_TR_base",
>                  idMapInt "VMCB_TR_limit",   idMapInt "VMCB_DR6",
>                  idMapInt "VMCB_DR7", idMapInt "VMCB_TLBCTL",
>                  idMapInt "VMCB_VAPIC",
>                  idMapInt "GSBASE", idMapInt "KERNELGSBASE",
>                  idMapInt "FSBASE",
>                  idMapInt "GDTRBASE", idMapInt "GDTRLIMIT"
>                ]

The default typemap. We know about the VMware family of types initially.

> typeMapInt str n = (makeIdent str, typeCreateInt n n)
> defaultTypeMap = [ typeMapInt "uint8" 1, typeMapInt "uint16" 2, typeMapInt "uint32" 4,
>                    typeMapInt "uint64" 8, typeMapInt "char" 1,
>                    typeMapInt  "short" 2, typeMapInt "long" 8 ]

Testing. If we find any '.emt/.out' pairs in the specified directory, make
sure that our compiler produces the expected output.

> testParser :: String -> String -> IO ()
> testParser inp out = do { str1 <- readFile inp
>                         ; str2 <- readFile out
>                         ; output <- parseOneStr str1 str2 nullOut
>                         ; testParserOut output str2 inp } where
> testParserOut :: CompileOutput -> String -> String -> IO ()
> testParserOut (Left e) _ _= do { hPutStr stderr
>                                  ("unexpected compile error" ++ (show e)) }
> testParserOut (Right (p, ctx)) exp filename = let
>     out = emitProgram p ctx in
>     if out == exp then return ()
>     else do hPutStr stderr (filename ++ " output mismatch: got:\n" ++
>                             (indent out) ++ "\n expected:\n" ++ (indent exp))
> testPairs ((f1, f2):fs) = do hPutStr stderr ("---testing " ++ f1 ++ "\n")
>                              testParser f1 f2
>                              testPairs fs
> testPairs [] = return ()
> indent [] = []
> indent (c:cs) = (if c == '\n' then
>                  ([c] ++ "   ")
>                  else
>                  ([c])) ++ (indent cs)

XXX: should use "pathSeparator", but it causes a link error for our version
of ghc. (???)

> pathSeparator = "/"
> runTests = do { testcnts <- getDirectoryContents "test"
>               ; let cnts = map ((++) ("test" ++ pathSeparator)) testcnts
>                     f2out f = f ++ ".out"
>                     emtfiles = filter (isSuffixOf ".emt") cnts 
>                     pairs = zip emtfiles (map (\x -> x ++ ".out") emtfiles) in
>                 testPairs pairs }
