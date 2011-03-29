Copyright 2007-2011, VMware, Inc.

> module Expr(Expr(StrConstExpr, IntConstExpr, IdentExpr, FuncExpr),
>             emitExpr, emitExprs, checkExpr, exprType, makeStructRef,
>             exprEvalConstInt,
>             makePointerDeref, makeArraySub, makeAddr, makeArithExpr,
>             makeCondExpr) where
> import Ident
> import CompileCtx
> import Type
> import MemModel

An expression is either a string or an integer. Integers might be complicated.

> data      Expr = StrConstExpr String |
>                    IntConstExpr Integer |
>                    IdentExpr Ident |
>                    ParenExpr Expr  |
>                    ArithExpr String [Expr] |
>                    FuncExpr Ident [Expr] |
>                    CastExpr Type Expr |
>                    CondExpr Expr Expr Expr |
>                    MemExpr Expr MemModel Type |
>                    BagRmExpr { lhs, idx :: Expr } |
>                    PointerExpr {addr :: Expr, mm :: MemModel, tp :: Type }
> instance Show Expr where
>   show (StrConstExpr str) = "\"" ++ str ++ "\""
>   show (IntConstExpr num) = (show num)
>   show (IdentExpr id)     = show id
>   show (ParenExpr x)      = "(" ++ (show x) ++ ")"
>   show (ArithExpr strop exprs) = strop ++ " (" ++
>                                    (foldr (\x y -> x ++ ", " ++ y) []
>                                           (map show exprs))
>   show (FuncExpr id exprs) = show (ArithExpr (show id) exprs)
>   show (MemExpr addr m tp) = "*" ++ (show (PointerExpr addr m tp))
>   show (BagRmExpr lhs idx)   = "bag__" ++ (show lhs) ++ "[" ++ (show idx) ++ "]"
>   show (PointerExpr addr m tp) = "((" ++ (show tp) ++ "*)" ++ (show addr) ++ ")"

makePointerDeref --

Construct an expression for a pointer dereference. Subtle point: we use
the pointer's memmodel, rather than the current memory model. This lets us
mix and match pointers from different memory models in the same, e.g.,
expression.

Straight pointer derefernces and struct references have a const int
offset expression. Array references have a non-trivial offset
expression.

> arrayIfy m@(MemExpr e mm tp) = if typeIsArray tp then
>                              e else
>                              m
> makePointerDeref :: CompileCtx -> Expr -> Expr
> makePointerDeref ctx expr = let
>     ptrTp = exprType expr ctx
>     tp = typePointedTo ptrTp in
>    arrayIfy (MemExpr expr (typePtrMemModel ptrTp) tp)
> makeStructRef ctx lhs@(MemExpr e mm tp) id = let
>     (fieldTp, fieldOff) = typeStructField tp id
>     width = typeWidth fieldTp in
>    arrayIfy (MemExpr (makeArithExpr "+"
>                       [e, (IntConstExpr (toInteger fieldOff))]) mm fieldTp)
> makeStructRef _ e id = error ("Cannot dereference field " ++ (show id) ++
>                               " of non-memory object " ++ (show e))
> makeAddr ctx e@(MemExpr addr mm tp) = PointerExpr addr mm tp
> makeAddr _ base = error ("Cannot take address of non-memory object " ++
>                          (show base))

makeArraySub -- Constuct an array subscript expression. This is kind
of like a pointer derefernce, but instead of having a known offset,
the offset is provided by an expression.

Since the parser can't tell the difference between array references
and bag removals, we are prepared to be invoked with a bag type,
rather than a pointer type. Since this is an "expression", as opposed
to a statement, we know the bag is being read, and so this is a destructive
operation.

> makeArraySub :: CompileCtx -> Expr -> Expr -> Expr
> makeArraySub ctx base idx = let
>     ptrTp  = exprType base ctx in
>   if (typeIsBag ptrTp) then
>      BagRmExpr base idx
>   else let
>     baseTp = typePointedTo ptrTp
>     tpw    = IntConstExpr (toInteger (typeWidth baseTp))
>     offExp = (makeArithExpr "*" [idx, tpw])
>     addExp = (makeArithExpr "+" [base, offExp]) in
>   MemExpr addExp (typePtrMemModel ptrTp) baseTp

> emitExpr :: Expr -> String
> emitExpr (IntConstExpr int)    = show int
> emitExpr (StrConstExpr str)    = show str
> emitExpr (IdentExpr id)        = emitIdent id
> emitExpr (BagRmExpr lhs idx)   = "(bagremove " ++ (show lhs) ++ " " ++
>                                  (show idx) ++ ")"

Arithmetic evaluation is a little odd. Rather than export a boatload of
different expr constructors for every possible operand, we trust that the
parser knows what the heck it's doing.

> emitExpr (ArithExpr s exprs) = " (" ++ s ++ (emitExprs exprs) ++ ")"

Along similar lines, we don't check that a function call really
exists, etc. We just pass along output to vp, figuring it will sort
out the meaning of it all.

> emitExpr (FuncExpr id exprs) = " (" ++ (emitIdent id) ++ 
>                                    (emitExprs exprs) ++ ")"

Emit a memory dereference.

> emitExpr (MemExpr addr memModel tp) = 
>     mask w ("(" ++ load ++ (emitExpr addr) ++ ")")
>     where
>     w = (typeWidth tp)
>     mask w x | w < 8  = "(& " ++ (show ((2 ^ (w * 8)) - 1)) ++ " " ++ x ++ ")"
>              | w >= 8 = x
>     load = emitRef memModel

> emitExpr (CondExpr c t f) = concat["(cond (", (emitExpr c), " ", (emitExpr t), ")",
>                                    "(1 ", (emitExpr f), ")", ")"]
> emitExpr PointerExpr{addr=addr} = emitExpr addr

Convenience functions: "padded emit" emits an expression with whitespace
separators. The public function emitExprs emits an evaluation for the entire
list of expressions, with the first items in the list pushed first.

> paddedEmit e = " " ++ (emitExpr e) ++ " "
> emitExprs  exprs = foldr (++) "" (map paddedEmit exprs)

Types. Each expression has a primitive type (integer or string), and
a type checker which returns a Boolean value indicating whether the
expression is well-typed.

> exprType :: Expr -> CompileCtx -> Type 
> exprType (StrConstExpr str) ctx = TypeString
> exprType (IntConstExpr int) ctx = TypeInt
> exprType (IdentExpr id)     ctx = typeExprType (varType ctx id)
> exprType (ArithExpr id str) ctx = TypeInt
> exprType (FuncExpr id es)   ctx = typeExprType (varType ctx id)
> exprType (MemExpr _ _ tp)   ctx = typeExprType tp
> exprType (PointerExpr _ m tp) ctx = TypePointer tp m
> exprType (CondExpr c t f)   ctx = exprType t ctx
> exprType b@(BagRmExpr {})   ctx = TypeInt

checkExpr: returns true if the expression is well typed. Primitive
expressions are always well-typed; only "compound" stuff (arithmetic
expressions, and function invocations whose parameter lists must match
the declarations) needs elaborate checking.

> ok = Right "ok"
> checkList :: [Expr] -> (Expr -> Bool) -> String -> Either String String
> checkList [] func err     = ok
> checkList (x:xs) func err = if (func x) then
>                              checkList xs func err
>                           else Left (err ++ ": " ++ show x)

> checkExpr :: Expr -> CompileCtx -> Either String String
> checkExpr (IdentExpr id)     ctx = ok
> checkExpr (StrConstExpr str) ctx = ok
> checkExpr (IntConstExpr int) ctx = ok
> checkExpr (BagRmExpr lhs idx) ctx = ok
> checkExpr (ArithExpr str es) ctx =
>    checkList es (\x -> (exprType x ctx) == TypeInt) "non-arithmetic sub-expr"
> checkExpr (FuncExpr id es)   ctx = case (typeFuncParams
>                                          (varType ctx id)) of
>    Just params -> if (typeFuncParamMatch params callParams) then ok else
>                       Left ("bogus function parameters: " ++
>                             (show (FuncExpr id es))) where
>                       callParams = (map (\x -> exprType x ctx) es)
>    Nothing     -> Left ("call of non-function: " ++ (show id))
> checkExpr (CondExpr c t f) ctx = if ltype == rtype then ok else
>                                  Left ("?: of mis-typed members: " ++ (show ltype) ++
>                                                                    (show rtype)) where
>                                  ltype = exprType t ctx
>                                  rtype = exprType f ctx

makeArithExpr: constant-folding constructor for arithmetic instructions.
Also use a few identities. Since this is bottom-up, it doesn't catch things
like "12 * foo + 11" -> "23 + foo"; it's mostly intended for the address
calculations above.

> makeArithExpr :: String -> [Expr] -> Expr
> makeArithExpr "+" [(IntConstExpr 0), e] = e
> makeArithExpr "+" [e, (IntConstExpr 0)] = e
> makeArithExpr "+" [(IntConstExpr i1), (IntConstExpr i2)] =
>     IntConstExpr (i1 + i2)
> makeArithExpr "-" [(IntConstExpr i1), (IntConstExpr i2)] = (IntConstExpr (i1 - i2))
> makeArithExpr "*" [(IntConstExpr 1), e] = e
> makeArithExpr "*" [e, (IntConstExpr 1)] = e
> makeArithExpr "*" [(IntConstExpr i1), (IntConstExpr i2)] =
>     IntConstExpr (i1 * i2)
> makeArithExpr s es = ArithExpr s es

> makeCondExpr cond t f = CondExpr cond t f

> exprEvalConstInt (IntConstExpr i) = i
