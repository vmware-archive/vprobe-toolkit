Copyright 2007-2011, VMware Inc. All rights reserved.

> module Parse where
> import Program
> import Probe
> import Statement
> import Expr
> import Ident
> import IdMap
> import Type
> import CompileCtx
> import MemModel
> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import Text.ParserCombinators.Parsec.Expr

> data ComposType = Enum | Struct | Union
> parseComposType = do reserved "struct"
>                      return Struct
>                   <|>
>                   do reserved "enum"
>                      return Enum
>                   <|>
>                   do reserved "union"
>                      return Union
> data StorageClass = DefaultStClass | TypedefStClass
> parseStorageClass = do cls <- (("auto" `stclass` DefaultStClass) <|>
>                                ("static" `stclass` DefaultStClass) <|>
>                                ("register" `stclass` DefaultStClass) <|>
>                                ("typedef" `stclass` TypedefStClass) <|>
>                                ("inline" `stclass` DefaultStClass) <|>
>                                ("__inline" `stclass` DefaultStClass))
>                        nextCls <- parseStorageClass
>                        return (combineClasses cls nextCls)
>                     <|> return DefaultStClass
>                            where
>        stclass s cls = do reserved s
>                           return cls
>        combineClasses DefaultStClass DefaultStClass = DefaultStClass
>        combineClasses _ TypedefStClass = TypedefStClass
>        combineClasses TypedefStClass _ = TypedefStClass

appendIds --
appendTypes --

State update functions for adding identifiers to our type and variable
Smaps.

> appendX f [] ctx = ctx
> appendX f ((tp,id):xs) ctx = appendIds xs (f ctx id tp)
> appendIds = appendX addVar
> appendTypes = appendX addType
> appendStCls DefaultStClass = appendIds
> appendStCls TypedefStClass = appendTypes

An Emmett program is a series of probes, declarations, function
definitions, type declarations, and MemModel blocks. We interleave an
informal BNF grammar with the code throughout.

   Program ::= (Probe | Decl | FuncDef | TypeDef | MemModel | <EOF>) Program

> type EmmettParser a = CharParser CompileCtx a
> emmettParse :: EmmettParser (Program, CompileCtx)
> emmettParse = do whiteSpace
>                  probes <- parseProbeList
>                  eof
>                  ctx <- getState
>                  return ((ProgramProbes probes), ctx)
> parseProbeList :: EmmettParser [Probe]
> parseProbeList = do p <- (parseDecl <|> parseProbe)
>                     rest <- parseProbeList
>                     return (p:rest)
>                  <|>
>                  do ps <- parseMemModel
>                     rest <- parseProbeList
>                     return (ps ++ rest)
>                  <|>
>                  return []

A memmodel block encloses types that are packed and accessed in a
particular way. See MemModel.lhs.

   MemModel ::= memmodel <ident> { Program }

> parseMemModel :: EmmettParser  [Probe]
> parseMemModel =  do reserved "memmodel"
>                     id <- parseIdent
>                     updateState (pushMemModel (nameToMemModel (show id)))
>                     l <- braces (parseProbeList)
>                     updateState popMemModel
>                     return l

  Decl ::=
     (struct|enum|union) IDENT; |
     StorageClass Type DeclIdList; |
     StorageClass Type PointerIdent\(ArgList\); |
  StorageClass ::= EXTERN | AUTO | STATIC | REGISTER | Nothing
  DeclIdList ::= commaSep (PointerIdent DeclSuffix)
  PointerIdent ::= DeclPrefix IDENT
  DeclPrefix ::= *DeclPrefix | CONST DeclPrefix | Nothing
  DeclSuffix ::= [INT]DeclSuffix | @INT DeclSuffix | Nothing

All decl's begin with a typeName/id pair. All declarations have a side
effect of updating the CompileCtx with a variable mapping the given
identifier to the given type.

The shenanigans with "parseDeclPastType" concern disambiguating
composite type forward declarations (e.g., 'struct IDENT;') from
variable declartions ('struct Foo a, *b, ...'). The former
are special, but there's no way to know we're dealing with one until
we see the semicolon. Note that empty declarations are always allowed;
"int;" is an ANSI C program.

parseDecl -- Parse a declaration, or perhaps a function body (since
they're indistinguishable until we see the semi or the '{').

> parseDecl = do maybeDecl <- parseDorF
>                res <- do semi
>                          return maybeDecl
>                      <|> do ctx <- getState
>                             tryUpgradeFuncDeclToDef maybeDecl (getDeclArgs ctx)
>                return res where
>   tryUpgradeFuncDeclToDef (ProbeDecl [(ftp@(TypeFunc tp argTypes), id)]) args =
>       do (body,ret) <- braces(do body <- many parseStatement
>                                  parseOptionalReturn body)
>          updateState (appendIds [(ftp, id)])
>          return (ProbeFuncdef tp id
>                  (makeCompileCtx (buildOrderedIdMap (flip args))
>                   (buildIdMap []))
>                  (Block body) ret)
>       <?> ("function " ++ (show id)) where
>       flip = map (\(a, b) -> (b, a))
>   tryUpgradeFuncDeclToDef _ _ = fail "mal-formed function declaration?"
>   parseOptionalReturn body = do reserved "return"
>                                 ret  <- parseExpr
>                                 semi
>                                 return (body, Just ret)
>                              <|> return (body, Nothing)

parseDorF -- try to parse a decl. There's a chance we'll end up parsing a
function body; leave the semicolon or { intact for our caller to discover.

> parseDorF = 
>     do stcls <- parseStorageClass
>        decl <- do composType <- parseComposType
>                   tp <- (parseIdent >>= parseComposPastIdent composType) <|>
>                         parseComposNoIdent composType
>                   parseDeclPastType tp
>                <|>
>                do tp <- parseType
>                   parseDeclPastType tp
>        updateState (appendStCls stcls decl)
>        return (ProbeDecl decl)
> parseTypeQual = many (reserved "volatile" <|> reserved "const" <|> reserved "__const"
>                       <|> reserved "__extension__" <|> reserved "extern")
> parseDeclPastType tp = do fIdPairs <- parseDeclIdList
>                           finishDecls tp fIdPairs
>                        <|> return [] where
>     finishDecls tp ptrIds = return (zipTypeIntoDeclIds tp ptrIds)
> zipTypeIntoDeclIds tp xs = map (\(fn, id) -> ((fn tp), id)) xs
> flatten = foldl (++) []



> parseArgList :: EmmettParser [(Type, Ident)]
> parseArgList = commaSep (do tp <- parseType
>                             (f, declId) <- (parseDeclId optionalId)
>                             return ((f tp), declId))

Note that we're not able to left-factor away the check for a dynamic
type; it has to look ahead just one token to see if the token is a
type or not.

> parseType :: EmmettParser Type
> parseType = do parseTypeQual
>                parseBaseType
> parseBaseType = do many1 (reserved "short" <|> reserved "long" <|>
>                           reserved "signed" <|> reserved "unsigned" <|>
>                           reserved "__signed" <|>
>                           reserved "int" <|> reserved "char" <|>
>                           reserved "__builtin_va_list")
>                    return TypeInt
>                 <|>
>                 do reserved "string"
>                    return TypeString
>                 <|>
>                 do composType <- parseComposType
>                    parseCompos composType
>                 <|>
>                 do reserved "bag"
>                    return typeCreateBag
>                 <|>
>                 do reserved "void"
>                    return TypeVoid
>                 <|>
>                 do reservedOp "..."
>                    return TypeEllipsis
>                 <|>
>                 try (do ctx <- getState
>                         id <- parseIdent
>                         parseDynamicType (typeNameToType ctx id) id)
>                 <?> "type"
> parseDynamicType :: Type -> Ident -> EmmettParser Type
> parseDynamicType TypeInvalid id = fail("unknown type "++(show id))
> parseDynamicType t@_ _ = return t
> parseComposPastIdent Struct id = parseStructPastIdent id
> parseComposPastIdent Union id = parseUnionPastIdent id -- XXX
> parseComposPastIdent Enum id = parseComposNoIdent Enum <|> return TypeInt -- XXX
> parseCompos ctp = (parseIdent >>= parseComposPastIdent ctp) <|>
>                   parseComposNoIdent ctp
> parseComposNoIdent Struct = parseStructNoIdent typeCreateStruct
> parseComposNoIdent Union  = parseStructNoIdent typeCreateUnion
> parseComposNoIdent Enum   = do braces (do updateState (setEnum 0)
>                                           parseEnumeratorList
>                                           return TypeInt)

parseEnumeratorList --

Can't just use commaSep, because we accept trailing commas before the
curly brace.

> parseEnumeratorList = do parseEnumerator
>                          v <- option () (do comma
>                                             rest <- parseEnumeratorList
>                                             return ())
>                          return ()
>                       <|> return ()
> parseEnumerator = do id <- parseIdent
>                      rv <- do reservedOp "="
>                               rhs <- parseExpr
>                               updateState (addEnumVal id
>                                            (exprEvalConstInt rhs))
>                               return (id, (exprEvalConstInt rhs))
>                            <|>
>                            do ctx <- getState
>                               updateState (addEnum id)
>                               return (id, 0)
>                      return rv

> parseStructPastIdent id = parseStructOrUnionPastIdent id typeCreateStruct
> parseUnionPastIdent id = parseStructOrUnionPastIdent id typeCreateUnion
> parseStructOrUnionPastIdent id typeCtor
>     = do tp <- (parseStructNoIdent typeCtor)
>          updateState (\x -> addStruct x id tp)
>          return tp
>       <|>                         -- struct ID1 *ID2 ...
>       do ctx <- getState
>          return (structNameToType ctx id)
> parseStructNoIdent typeCtor
>    = do list <- parseStructBody
>         ctx <- getState
>         return (typeCtor list (getMemModel ctx)) where
>    parseStructMember = do tp   <- parseType
>                           ids  <- parseDeclIdList
>                           semi
>                           return (zipTypeIntoDeclIds tp ids)
>    parseStructBody   = do list <- braces(many parseStructMember)
>                           return (flatten list)
> parseDeclIdList = do r <- commaSep1 (parseDeclId requiredId)
>                      parseGccAttributes
>                      return r
> parseGccAttributes = many (do (reserved "__attribute__" <|> reserved "__asm")
>                               discardParenthesizedGoop
>                               return ())
> discardParenthesizedGoop = do reservedOp "("
>                               skipMany (noneOf "()")
>                               discardParenthesizedGoop
>                               reservedOp ")"
>                            <|> return ()

A Probe consists of the name of the probe, followed by a statement to
be executed. (Recall that "blocks," or { statement;* }, are a form of
statement.)

| Probe:
|     IDENTIFIER Statement

> parseProbe :: EmmettParser Probe
> parseProbe = do { id <- parseIdent
>                 ; stmt <- parseStatement
>                 ; return (ProbeStatement id stmt)
>                 } <?> "probe definition"

Things start to fan out a bit with statements. Statements can be
blocks, conditionals, or a "simple statement." We left-factor the
parsing of simple statements by observing that they
all start with an identifier. We then switch on the next
token to differentiate aggrs, scalar assignments, and function calls.


| Statement:
|     IfStatement
|     BlockStatement
|     SimpleStatement

> parseStatement = parseIf <|>
>                  parseBlock <|>
>                  parseSimpleStatement

An "simple statement" is one of an assignment, a function call, or an
aggregate sample. I.e., a "thing followed by a semi-colon". Note that
we're ducking out on C's whole "assignments can be expressions, too!"
trick; foo = bar = 0; is just a parse error.

| SimpleStatement:
|     ScalarAssignment;
|     AggrAssignment;
|     FunctionCall;
| 
| ScalarAssignment:
|    IDENTIFIER = Expression

E.g.:

   // assignment: <ident> = <expr>
   varName = (myInt & 3) << (otherInt - 12);

| AggrAssignment:
|    IDENTIFIER (optional [ ExpressionList ]) <- Expression
|    IDENTIFIER (optional [ ExpressionList ]) ++

Aggregate samples looks somewhat like assignment, but with the
"<-" operator or ++, and possibly integer or string keys:

       simpleAggr++;
       simpleAggr <- myVar;
       aggrWithIntKeys[var, RIP, 13] <- TSC - firstTSC;
       aggrWithIntAndStrKeys[RIP, curprocname] <- 14;
       aggrWithJustStrKeys[curprocname] <- var;

Function calls are just a function name, followed by a parenthesized
comma-separated list of arguments:

| FunctionCall:
|    IDENTIFIER ( ExpressionList )

   funCall(arg0, int12);
   arglessFunc();

> parseSimpleStatement  = do id <- parseIdent
>                            stmt <- do nkeys <- parseOptionalAggrKey
>                                       parseAggrRHS id nkeys
>                                   <|>
>                                   do exprs <- parens (commaSep parseExpr)
>                                      return (FuncStatement id exprs)
>                                   <|>
>                                   do reservedOp "="
>                                      expr <- parseExpr
>                                      ctx <- getState
>                                      updateState (appendIds
>                                                   [((exprType expr ctx), id)])
>                                      return (Assign id expr)
>                            semi
>                            return stmt
>     where
>     parseAggrKey = squares (commaSep parseExpr)
>     parseOptionalAggrKey = parseAggrKey <|> return []
>     parseAggrRHS id keys = do reservedOp "<-"
>                               expr <- parseExpr
>                               updateState (appendAggrId id keys)
>                               ctx <- getState
>                               return (makeAggrSample id keys expr ctx)
>                            <|>
>                            do reservedOp "++"
>                               updateState (appendAggrId id keys)
>                               ctx <- getState
>                               return (makeAggrSample id keys (IntConstExpr 1)
>                                       ctx)
>     appendAggrId id keys ctx = addVar ctx id aggrType where
>                                aggrType = TypeAggr (length intkeys)
>                                                    (length strkeys)
>                                (intkeys, strkeys) = splitAggrKeys keys ctx

An if starts with the keyword if, followed by a parenthesized expression and
a statement. If we see an "else", suck in another statement and return an
if-else compound statement.

| IfStatement:
|    if ( Expression ) Statement (optional else Statement)

> parseIf = do reserved "if"
>              expr <- parens parseExpr
>              stmt <- parseStatement
>              ifs <- do reserved "else"
>                        els <- parseStatement
>                        return (IfElse expr stmt els)
>                    <|>
>                    return (If expr stmt)
>              return ifs


A block is a series of statements, demarcated by curly braces.

| BlockStatement:
|    { StatementList }
| StatementList:
|    Statement
|    Statement StatementList

> parseBlock = do stmts <- braces (many parseStatement)
>                 return (Block stmts)

The table below defines a lexer for a C-like language; we have '#' as
a line comment character, as well. To accomodate NT's symbol table
format and the GUEST: notation for probe types, we accept ':' and '!'
as part of symbol names. I don't *think* ambiguities with the unary
'!' operator are possible, but I haven't proven it.

> lexer :: P.TokenParser CompileCtx
> lexer = P.makeTokenParser (javaStyle
>                          { commentLine  = "#",
>                            nestedComments = False,
>                            identStart = letter <|> char '_',
>                            identLetter = alphaNum <|> oneOf "_:!",
>                            opStart = oneOf "*/+-!=<>|&~^.[",
>                            opLetter = oneOf "+=<>|&.",
>                            caseSensitive = True,
>                            reservedOpNames = ["*", "/", "+", "-",
>                                               "!", "==", "!=", "<", ">",
>                                               ">=", "<=", "||", "&&",
>                                               "|", "&", "~", "^", "=",
>                                               "<-", "<<", ">>", "++", "...",
>                                               ".", "->", "(", ")", "[", "]" ],
>                            reservedNames = [ "if", "else", "int", "string",
>                                              "aggr", "bag", "void", "vprobe",
>                                              "return", "while", "for", "do",
>                                              "switch", "case", "struct",
>                                              "typedef", "memmodel", "const",
>                                              "__const",
>                                              "restrict", "__restrict",
>                                              "__extension__", "__asm",
>                                              "__attribute__",
>                                              "volatile", "extern", "static",
>                                              "inline", "__inline", "long",
>                                              "short",
>                                              "unsigned", "char", "__builtin_va_list" ]
>                          })
> whiteSpace = P.whiteSpace lexer
> lexeme     = P.lexeme lexer
> symbol     = P.symbol lexer
> integer    = P.integer lexer
> parens     = P.parens lexer
> braces     = P.braces lexer
> squares    = P.squares lexer
> commaSep   = P.commaSep lexer
> commaSep1  = P.commaSep lexer
> semi       = P.semi lexer
> comma      = P.comma lexer
> ident      = P.identifier lexer
> reserved   = P.reserved lexer
> reservedOp = P.reservedOp lexer
> operator   = P.operator lexer
> strLit     = P.stringLiteral lexer

buildExpressionParser only allows *one* unary operator per precedence
level so, e.g., C's "!!expr" doesn't parse. This turns a list of prefix
operators into a big old prefix operator that can be repeated.

> repeatOp foo = do f <- foo
>                   childF <- repeatOp foo
>                   return (f . childF)
>                <|> return id

We use a special expression parser to handle parsing the "type
modifiers" that ornament a declaration. A given declaration
produces an identifier and a type transformer. The precedence
goes "[]" > "(...)" > "*".

> parseDeclId :: EmmettParser Ident -> EmmettParser (Type -> Type, Ident)
> parseDeclId parseId = buildExpressionParser table (parseBasicDecl parseId) where
>             table = [
>                      [Postfix funcArgs,
>                       Postfix (repeatOp array)],
>                      [Prefix  (repeatOp (ptr <|> const))],
>                      [Postfix offset]
>                     ]
>             array = do reservedOp "["
>                        idx <- option (IntConstExpr 0) parseExpr
>                        reservedOp "]"
>                        ctx <- getState
>                        return (\(f, id) ->
>                                ((f .
>                                  (\tp -> typeCreateArray tp
>                                   (fromInteger (exprEvalConstInt idx))
>                                   (getMemModel ctx))), id))
>             funcArgs = do args <- parens parseArgList
>                           updateState (setDeclArgs args)
>                           return (\(f, id) ->
>                                   (f . (\tp -> (TypeFunc tp (map fst args))),
>                                    id))
>             ptr = do reservedOp "*"
>                      ctx <- getState
>                      return (\(f, id) ->
>                              (f . (\tp -> (pointerFrom tp ctx)), id))
>             pointerFrom tp ctx = let
>                                  mm = getMemModel ctx in
>                                  TypePointer tp mm
>             const = do (reserved "const" <|> reserved "restrict" <|>
>                         reserved "__const" <|> reserved "__restrict")
>                        return id
>             offset = do reservedOp "@"
>                         e <- parseExpr
>                         return (\(f, id) ->
>                                 (f, (setIdentOff id (fromInteger (exprEvalConstInt e)))))
> optionalId = parseIdent <|> return (makeIdent "")
> requiredId = parseIdent
> parseBasicDecl f = parens (parseDeclId f)
>                    <|> do ident <- f
>                           return (id, ident)

"Expressions" are what you think they are: values composed of numeric
constants, variables, and operators that combine them. Our expressions
attempt a C-like look and feel, but I'm far too ignorant about the
nasty ins and outs to claim that every little nook and cranny of
precedence and associativity is identical. We use parsec's
ridiculously cool expression parser factory. The precedence table is
based on the subset of C's expression syntax which we use in vp. See
http://www.difranco.net/cop2220/op-prec.htm, e.g.

The second parameter to buildExpressionParser is a parse function for
the "atomic" members of the expression. In our case, this is either a
parenthesised grouping of a more complex expression, or one of the
"fundamental" sources of values: a variable, a constant, or a function
call.

You know what expressions look like, but e.g.,
   3 * 2 + 2 * 4           // precedence works the way you'd expect
   b == 12 || c == 1 || !a // booleans are really integers, too

For clarity, I won't spell out all the prefix and infix ops in our
informal grammar, but:

| Expression:
|    STRINGLITERAL
|    IDENTIFIER
|    FunctionCall
|    \( Expression \)
|    PrefixOp Expression 
|    Expression InfixOp Expression
|    Expression [ Expression ]

> parseExpr :: EmmettParser Expr
> parseExpr = buildExpressionParser table parseFactor where
>             table = [ 
>                      [ postfix ],
>                      [ prefixAmpersand, prefixStar ],
>                      [ prefixArithOp ],
>                      [ infixOp "*", infixOp "/", infixOp "%"],
>                      [ infixOp "+", infixOp "-"],
>                      [ infixOp "<<", infixOp ">>"],
>                      [ infixOp "<", infixOp "<=",
>                        infixOp ">", infixOp ">=" ],
>                      [ infixOp "==", infixOp "!=" ],
>                      [ infixOp "^", infixOp "|", infixOp "&" ],
>                      [ infixOp "&&", infixOp "||" ],
>                      [ Postfix trinaryCond ] ]

A "MemRef" is a dereference of a typed pointer. It has an offset and a type.
Both the type and offset fields can be modified by trailing "." operators,
which indicate struct fields.

XXX: errors in this area should bubble through the usual Parsec error
handling, so that we can print a file and line number.

Postfix handling influenced by the C yacc grammar available
online. This is more manual than just making a bunch of postfix
operators in the expression table, because the default expression
parser will only apply a single postfix expression from a given precedence
level.

>             postfix = Postfix (postfixHelper (\a->a))
>             postfixHelper fSoFar = (dot fSoFar) <|>
>                                    (arrow fSoFar) <|>
>                                    (arrayRef fSoFar) <|>
>                                    (return fSoFar)
>             arrowOrDot opName ctor fSoFar = do reservedOp opName
>                                                ctx <- getState
>                                                id <- parseIdent
>                                                postfixHelper ((\a-> ctor ctx a id) .
>                                                                 fSoFar)
>             dot = arrowOrDot "." makeStructRef
>             arrow = arrowOrDot "->" doArrow
>             doArrow ctx l r  = (makeStructRef ctx (makePointerDeref ctx l) r)
>             arrayRef fSoFar = (do idx <- squares parseExpr
>                                   ctx <- getState
>                                   postfixHelper ((\b -> makeArraySub ctx b
>                                                   idx) . fSoFar))
>             addrOf       ctx = return (makeAddr ctx)
>             prefixStar = Prefix (repeatOp
>                                  (do reservedOp "*"
>                                      ctx <- getState
>                                      return (makePointerDeref ctx)))
>             prefixAmpersand = Prefix (do reservedOp "&"
>                                          ctx <- getState
>                                          addrOf ctx)
>             infixOp s = Infix
>                         (do reservedOp s
>                             return (\a b -> makeArithExpr s [a, b])) AssocLeft
>             prefixArithOp = Prefix (repeatOp
>                                     (do op <- do reservedOp "!"
>                                                  return "!"
>                                               <|>
>                                               do reservedOp "~"
>                                                  return "~"
>                                         return (\a -> makeArithExpr op [a])))
>             parseFactor = (parens parseExpr) <|>
>                           parseSimpleExpr
>             parseSimpleExpr = parseInt <|>
>                               parseString <|>
>                               parseSizeof <|>
>                               (try parseFuncall) <|>
>                               do id <-parseIdent
>                                  expr <- do exprs <- parens (commaSep parseExpr)
>                                             return (FuncExpr id exprs)
>                                          <|>
>                                          do ctx <- getState
>                                             return (id2Expr id ctx)
>                                  return expr
>                               <?> "simple expression"
>             parseFuncall = do id <- parseIdent
>                               exprs <- (parens (commaSep parseExpr))
>                               return (FuncExpr id exprs)
>             parseSizeof = do reserved "sizeof"
>                              tp <- do e <- parseSimpleExpr
>                                       ctx <- getState
>                                       return (exprType e ctx)
>                                    <|>
>                                    parens (do tp <- parseType
>                                               (fn, u) <- parseDeclId optionalId
>                                               return (fn tp))
>                              return (IntConstExpr (toInteger (typeWidth tp)))
>             trinaryCond = do reservedOp "?"
>                              ife <- parseExpr
>                              reservedOp ":"
>                              elsee <- parseExpr
>                              return (\e -> makeCondExpr e ife elsee)

Getting down to the terminal symbols. Parsec's token module
automagically handles all C-like integer inputs by default: octal,
hex, signs, etc.

> parseInt = do { int <- integer
>               ; return (IntConstExpr int)
>               } <?> "integer"
> parseIdent = do { str <- ident
>                 ; return (makeIdent str)
>                 } <?> "identifier"
>    where firstAllowed = oneOf "_" <|> letter
> parseString = do { str <- strLit
>                  ; return (StrConstExpr str)
>                  } <?> "string"

> id2Expr id ctx = case (enumLookup id ctx) of
>     Just v  -> (IntConstExpr v)
>     Nothing -> (IdentExpr id)
