Copyright 2007-2011, VMware, Inc.

> module Statement(Statement(Assign, Block, If,
>                    IfElse, FuncStatement),
>                  makeAggrSample, emitStatement, checkStatement, splitAggrKeys)
>   where
> import Expr
> import Ident
> import CompileCtx
> import Type

A statement is an assignment, a function call, or a block (which is just
a bunch o' statements).

> data  Statement = Assign Ident Expr |
>                   AggrSample Ident [Expr] Expr |
>                   BagInsert Ident Expr Expr |
>                   Block [Statement] |
>                   If Expr Statement |
>                   IfElse Expr Statement Statement |
>                   FuncStatement Ident [Expr]

Show instances for the statements, to aid in debugging.

> instance Show Statement where
>   show (Assign id expr)   = (show id) ++ " = " ++ (show expr) ++ ";\n"
>   show (FuncStatement id exprs) = (show id) ++ (parens (commaSepArgs exprs))
>   show (AggrSample id exprs rhs)
>                             = (show id) ++ "[" ++ (commaSepArgs exprs) ++ 
>            "] " ++ "<- " ++ (show rhs) ++ ";\n"
>   show (BagInsert id idx rhs) = (show id) ++ "[" ++ (show idx) ++ "] = " ++
>                                 (show rhs) ++ ";\n"
>   show (Block stmts)      = "{" ++
>                          (indent (concat (map show stmts))) ++ "\n}"
>   show (If bool stmt)     = "if (" ++ (show bool) ++ ") then" ++
>                             (indent (show stmt))
>   show (IfElse bool ifs elses) = "if (" ++ (show bool) ++ ") then " ++
>                                    (indent (show ifs)) ++ " else " ++
>                                    (indent (show elses))
> commaSepArgs exprs = concat (map (\x -> (show x) ++ ", ") exprs)
> indent ('\n':xs) = "\n    " ++ (indent xs)
> indent (c:xs)    = [c]      ++ (indent xs)
> indent [] = []
> parens str = concat ["(", str, ")"]
> parencat list = parens (concat (map (\x -> " " ++ x) list))

makeAggrSample -- The parser can't tell an aggregation from a bag insertion.
Only the type system can distinguish them; bags can only occur with one
key.

> makeAggrSample id (singleKey:[]) rhs ctx = let
>          idType = varType ctx id in
>   if typeIsBag idType then
>      (BagInsert id singleKey rhs)
>   else
>      (AggrSample id [singleKey] rhs)
> makeAggrSample id ks rhs _ = AggrSample id ks rhs


Variable assignments are translated depending on the type of the
ident: (setstr ident expr) for string vars and (setint ident expr) for
integers.

> emitStatement (Assign ident expr) ctx = 
>    parencat [ case (varType ctx ident) of
>                 TypeString -> "setstr"
>                 _            -> "setint",
>               (emitIdent ident), " ", (emitExpr expr)]

Block: emit all the statements in order, sequenced via "do"

> emitStatement (Block stmts) ctx =
>       parencat ["do ", (concat (map (\x -> emitStatement x ctx) stmts))]

If maps to vp's cond construct. We could implement "If" in terms of
"IfElse", but I am tragically aware of vp's implementation, and
realize that a (1 (do)) thrown on the end of a cond block will
actually cause it to spin its wheels :/.


> emitStatement (IfElse pred t f)   ctx =
>       parencat ["cond ", (parens ((emitExpr pred) ++ 
>                                   (emitStatement t ctx))),
>                 (parens
>                  ("1 " ++ (emitStatement f ctx)))]
> emitStatement (If pred block)     ctx =
>       parencat ["cond ", (parens ((emitExpr pred) ++
>                                   (emitStatement block ctx)))]

The incredibly impossible to remember vp syntax for aggrs goes:
(aggr <aggrId> (<intKey0> ... <intKeyN<) (<strKey0> ... <strKeyN)
<expr>). We do the string/integer key separation on behalf of the user,
which is the entire reason that emitStatement has a CompileCtx arg.

> emitStatement (AggrSample id keys rhand) ctx =
>    parencat ["aggr ", (emitIdent id),
>              parencat (map emitExpr intkeys),
>              parencat (map emitExpr strkeys),
>              (emitExpr rhand)] where
>        (intkeys, strkeys) = splitAggrKeys keys ctx

Bag insertions.

> emitStatement (BagInsert id idx rhs) ctx = 
>    parencat ["baginsert", (emitIdent id), (emitExpr idx), (emitExpr rhs)]

Function calls. Note that there's a slight syntactic weirdness here, in
that function calls can be both expressions and statements.  There's no
context in the grammar where this causes a problem, though.

> emitStatement (FuncStatement id exprs) ctx = 
>     parencat [(emitIdent id), " ", (emitExprs exprs)]


checkFold -- utility routine to "fold" checks of "Either a b"; the first
error (Left) value encountered stops evaluation. If no errors are found,
we return success.

> ok = Right "ok"
> checkAnd :: (Either a b) -> (Either a b) -> (Either a b)
> checkAnd a b = case a of
>    Left err -> Left err
>    Right s  -> case b of
>       Left err -> Left err
>       Right s  -> Right s
> checkFold :: (a -> CompileCtx -> Either String String) -> [a] ->
>              CompileCtx -> Either String String
> checkFold func (x:xs) ctx = case (func x ctx) of
>            Left e  -> Left e
>            Right s -> checkFold func xs ctx
> checkFold func [] ctx = ok
> checkStmtList = checkFold checkStatement

typeMatch -- maps type's == operator onto Left/Right

> typeMatch err a b = if (a == b) then ok
>                 else Left ("(" ++ err ++"): " ++
>                            (show a) ++ " vs. " ++ (show b))

checkStatement -- maps a list of statements into an error (Left errorStr)
or a success value (ok).

> checkStatement :: Statement -> CompileCtx -> Either String String
> checkStatement (Block list) ctx = checkStmtList list ctx
> checkStatement (If cond blk) ctx =
>    if (TypeInt == (exprType cond ctx)) then
>       checkAnd (checkExpr cond ctx) (checkStatement blk ctx)
>    else
>       Left ("\"if ()\" on non-integer: " ++ (show cond) ++ "type: " ++
>             (show (exprType cond ctx)))
> checkStatement (IfElse cond thens elses) ctx =
>    if (TypeInt == (exprType cond ctx)) then
>       checkAnd (checkExpr cond ctx) (checkStmtList (thens:[elses]) ctx)
>    else
>       Left ("\"if () else\" on non-integer: " ++ (show cond))
> checkStatement (Assign id expr) ctx = 
>    checkAnd (checkExpr expr ctx)
>             (typeMatch ("assignment of " ++ (show id))
>              lhsType (exprType expr ctx)) where
>          lhsType = (varType ctx id)
> checkStatement (AggrSample id keys rhs) ctx =
>    checkAnd (typeMatch ("aggregation of non-integer " ++ (show id))
>              rhsType TypeInt)
>        (checkAnd (checkFold checkExpr keys ctx)
>             (typeMatch ("aggregate keys of " ++ (show id))
>                  (varType ctx id) (TypeAggr (length intkeys)
>                                                    (length strkeys)))) where
>       (intkeys, strkeys) = splitAggrKeys keys ctx
>       rhsType = exprType rhs ctx
> checkStatement (BagInsert id key rhs) ctx = let
>              idType = varType ctx id
>              keyType = exprType key ctx
>              rhsType = exprType rhs ctx in
>    checkAnd (checkAnd (typeMatch ("bag insert to non-bag" ++ (show id))
>                        typeCreateBag idType)
>                       (typeMatch ("non-integral bag key" ++ (show key))
>                        TypeInt keyType))
>             (typeMatch ("non-integral bag value" ++ (show rhs))
>                        TypeInt rhsType)
> checkStatement (FuncStatement id exprs) ctx = 
>    case (typeFuncParams funcType) of
>    Nothing     -> Left ("call of non-function " ++ (show id))
>    Just params -> if (typeFuncParamMatch argTypes params) then ok
>                   else Left("parameter mismatch: " ++ (show argTypes) ++
>                             " vs. " ++ (show params) ++
>                             " in function call " ++
>                             (show (FuncStatement id exprs)))
>    where
>       funcType = varType ctx id
>       argTypes = map (\x -> (exprType x ctx)) exprs

splitAggrKeys -- We often have to separate the string from integer keys in
aggregates. Let's share the code.

> splitAggrKeys :: [Expr] -> CompileCtx -> ([Expr], [Expr])
> splitAggrKeys keys ctx = 
>   (typefilt TypeInt keys, typefilt TypeString keys) where
>    typefilt ty keys = filter (\x -> (exprType x ctx == ty)) keys
