Copyright 2007-2011, VMware, Inc.

Emmett's type system. Primitives are integers and strings. Compound types
are aggregates (which are typed by the number and type of primitive
keys) and functions (which are typed by return value, and order/type
of arguments.)  In a well-typed program, all uses of a given identifier
have the "compatible" types.

We implement the Eq interface, but "compatibility" isn't necessarily
equality. It's fine to have the left half of an assignment be TypeInt,
while the right side is (TypeFunc TypeInt []).

A word about recursive types: structs are not ok for lvalues, just for
guest, vmm, or vmkernel values. VProbe script variables are not themselves
open to arbitrary byte layout. The pointer type takes a width, because
there is no reliable way of knowing whether the guest uses 32-bit or
64-bit addresses: e.g., the address space in question could be
physical memory, it could be MacOS where it's a 32-bit address space
even though we're in long mode, 32-bit user level apps on 64-bit
kernels, vice versa, and so on. Plus, this lets us write scripts for
16-bit applications, so we've got that going for us :p

> module Type(Type(TypeString, TypeInt, TypePreDef, TypeAggr, TypePointer,
>                  TypeFunc, TypeVoid, TypeInvalid, TypeEllipsis, TypeBag),
>             typeExprType, typeIsAggr, typeIsArray, typeIsBag, typeFuncParams,
>             typeFuncParamMatch, typeDecl,
>             typeCreateInt, typeCreateStruct, typeCreateUnion,
>             typeCreateArray, typeCreateBag,
>             typeWidth, typeAlign, typeStructField, typePointedTo,
>             typePtrMemModel, undefinedStruct) where
> import Ident
> import MemModel
> type StructField = (Ident, Type, Int)
> data Type = TypeString | TypeInt | TypeEllipsis |
>               TypePreDef Type |
>               TypeAggr    { nIntKeys, nStrKeys :: Int } |
>               TypeFunc    { retval :: Type, args :: [Type] } |
>               TypePointer { tp :: Type, mm :: MemModel } |
>               TypeArray   { tp :: Type, width :: Int, mm :: MemModel } |
>               TypeUserInt { w, al :: Int } |
>               TypeStruct  { fields ::[StructField], mm :: MemModel } |
>               TypeBag     { size :: Int } |
>               TypeUndefinedStruct |
>               TypeVoid |
>               TypeInvalid

Int and PreDef Int are distinguished in that the latter doesn't need to be
declared in our scm output, and is read-only.

> instance Eq Type where
>    TypeString    == TypeString    = True
>    TypeInt       == TypeInt       = True
>    TypePreDef t  == TypePreDef u  = t == u
>    TypeVoid      == TypeVoid      = True
>    TypeInvalid   == TypeInvalid   = True

For now, allow conversion willy-nilly back and forth between pointers and
integers.

>    TypeInt == (TypePointer {}) = True
>    (TypePointer {}) == TypeInt = True
>    (TypeAggr nInts1 nStr1) == (TypeAggr nInts2 nStr2)
>                                       = nInts1 == nInts2 && nStr1 == nStr2
>    (TypeFunc ret1 params1) == (TypeFunc ret2 params2) = 
>       (ret1 == ret2) && (typeFuncParamMatch params1 params2)
>    (TypePointer t1 mm) == (TypePointer t2 nn) = t1 == t2 && mm == nn
>    (TypeArray t1 n1 mm1)  == (TypeArray t2 n2 mm2) = t1 == t2 && n1 == n2 &&
>                                                      mm1 == mm2
>    (TypeBag n1) == (TypeBag n2) = True
>    t == u = False
> instance Show Type where
>   show (TypeString)        = "string"
>   show (TypeInt)           = "int"
>   show (TypePreDef t)      = "const " ++ (show t)
>   show (TypeAggr a b)      = "aggr [" ++ (show a) ++ " ints, " ++ 
>                                 (show b) ++ " strings]"
>   show (TypeVoid)          = "void"
>   show (TypePointer t mm)= (show t) ++ "*"
>   show (TypeArray t n mm)  = (show t) ++ "[" ++ (show n) ++ "]"
>   show (TypeInvalid)       = "!!INVALID!!"
>   show (TypeFunc ret args) = (show ret) ++ "(*)" ++ "(" ++
>     (foldr (++) "" (map (\x -> (show x) ++ ", ") args)) ++ ")"
>   show (TypeEllipsis)      = "..."
>   show (TypeUserInt w al)  = "uint" ++ (show w)
>   show (TypeStruct f mm)   = "struct {\n" ++
>     (foldr (++) [] (map (\x -> ("   " ++ (show x) ++ ";\n")) f)) ++ "}"
>   show (TypeBag n)         = "bag [" ++ (show n) ++ "]"
>   show (TypeUndefinedStruct) = "struct"

typeFuncParamMatch: returns True iff the two functions are compatible. Mostly
a list equality function, but the ellipsis pattern in either argument list
will cause us to match anything.

> typeFuncParamMatch :: [Type] -> [Type] -> Bool
> typeFuncParamMatch [TypeEllipsis] x = True
> typeFuncParamMatch x [TypeEllipsis] = True
> typeFuncParamMatch (x:xs) (y:ys)      = (x == y) &&
>                                           (typeFuncParamMatch xs ys)
> typeFuncParamMatch [] []              = True
> typeFuncParamMatch x y                = False

typeExprType -- Returns the "rvalue" type; the type that this type may be
assigned to, or its type when used in an expression. Aggrs, ellipses, and
vprobe identifiers shouldn't be used in expressions.

> typeExprType TypeInt           = TypeInt
> typeExprType (TypePreDef t)    = t
> typeExprType (TypeUserInt _ _) = TypeInt
> typeExprType p@(TypePointer t m) = p
> typeExprType (TypeArray {tp=t, mm=m}) = (TypePointer t m)
> typeExprType TypeString        = TypeString
> typeExprType TypeVoid          = TypeVoid
> typeExprType (TypeFunc rt _)   = rt
> typeExprType b@(TypeBag t)     = b 
> typeExprType  _ = TypeInvalid

typeIs* -- Predicates for types whose guts are not publicly exposed.

> typeIsAggr (TypeAggr a b) = True
> typeIsAggr _ = False
> typeIsArray TypeArray { } = True
> typeIsArray _ = False
> typeIsBag (TypeBag b)     = True
> typeIsBag _ = False
> typeFuncParams (TypeFunc ret params) = Just params
> typeFuncParams t = Nothing

typeDecl -- the vp declaration for a given type

> typeDecl :: Ident -> Type -> String
> typeDecl id (TypeString)    = "(defstring " ++ (emitIdent id) ++ ")\n"
> typeDecl id (TypeInt)       = "(definteger " ++ (emitIdent id) ++ ")\n"
> typeDecl id (TypePointer t m) = typeDecl id (TypeInt)
> typeDecl id (TypePreDef t)  = ""
> typeDecl id (TypeAggr ints strs) = "(defaggr " ++ (emitIdent id) ++ 
>              " " ++ (show ints) ++ " " ++ (show strs) ++ ")\n"
> typeDecl id (TypeBag n) = "(defbag " ++ (emitIdent id) ++ " " ++ (show n)
>                           ++ ")\n"
> typeDecl id t = ""

typeCreate* -- Constructors so the entire type system isn't hanging out
of this module's interface. CreateArray contains an unusual wart: since
the syntax for declaring a bag's size matches the syntax for declaring
an array, typeCreateArray will accept bag types as inputs, and return a
resized bag. This has some weird consequences; e.g.,

bag foo[12][13]; # bag of size 13, not a "bag of bags" or whatever.

> typeCreateInt w al   = (TypeUserInt w al)
> typeCreateArray tp@(TypeBag old) n mm = tp{size = n}
> typeCreateArray tp n m = (TypeArray tp n m)
> typeCreateBag        = (TypeBag 0)

typePointedTo -- Used by other modules dereferencing pointers. Note that
arrays are effectively pointers to their first element. Hah.

> typePointedTo (TypePointer t m) = t
> typePointedTo (TypeArray t n m) = t
> typePointedTo (TypeBag {}) = TypeInt
> typePointedTo t = error ("type " ++ (show t) ++ "is not a pointer")
> typePtrMemModel (TypePointer t m) = m
> typePtrMemModel (TypeArray t n m) = m
> typePtrMemModel (TypeBag {}) = defaultMemModel
> typePtrMemModel t = error ("type " ++ (show t) ++ "is not a pointer")

typeCreateStruct -- Structs have named members with a given alignment
and size. They're composed of user-defined ints. We use a sentinel identifier
(that isn't a legal Emmett identifier) to locate the final offset.

> structSent = makeIdent " ! emmett"
> typeCreateStructOrUnion :: (Int -> [StructField] -> [(Type, Ident)] ->
>                             MemModel -> Type) ->
>                            [(Type, Ident)] -> MemModel -> Type
> typeCreateStructOrUnion packer = packer 0 []
> typeCreateStruct :: [(Type, Ident)] -> MemModel -> Type
> typeCreateStruct = typeCreateStructOrUnion structPacker where
>                    structPacker :: Int -> [(Ident, Type, Int)] ->
>                                                         [(Type, Ident)] ->
>                                                         MemModel -> Type
>                    structPacker offSoFar fields ((tp,id):xs) mm = let
>                                      w     = typeWidth tp
>                                      al    = typeAlign tp
>                                      start = case (getIdentOff id) of 
>                                                Just off -> off
>                                                Nothing  -> align offSoFar al
>                                      end   = start + w in
>                              structPacker end (fields ++ [(id, tp, start)]) xs mm
>                    structPacker lastOff fields [] mm =
>                        TypeStruct (fields ++ [(structSent, TypeVoid,
>                                                lastOff)]) mm

Unions are just structs where every field begins at offset zero. They have
a trivial packer. The packer passes along the *maximal* field width, rather
than the last field width, to compute the union's size.

> typeCreateUnion :: [(Type, Ident)] -> MemModel -> Type
> typeCreateUnion = typeCreateStructOrUnion unionPacker where
>     unionPacker szSoFar fields ((tp,id):xs) mm = let
>	 maxSz = max (typeWidth tp) szSoFar in
>         unionPacker maxSz (fields ++ [(id, tp, 0)]) xs mm
>     unionPacker sz fields [] mm = 
>         TypeStruct (fields ++ [(structSent, TypeVoid, sz)]) mm

typeStructField -- External accessor for struct guts. Given an identifier,
returns an offset and a type.

> typeStructField :: Type -> Ident -> (Type, Int)
> typeStructField (TypeStruct {fields=((fid,tp,off):xs), mm=m}) id =
>     if id == fid then (tp, off) else
>        typeStructField (TypeStruct {fields=xs, mm=m}) id
> typeStructField (TypeStruct {fields=[]}) id =
>        error ("could not find id " ++ (show id))

Generic width and align routines.

> typeWidth (TypeUserInt w al) = (align w al)
> typeWidth s@(TypeStruct {})  = (align (snd (typeStructField s structSent))
>                                 (typeAlign s))
> typeWidth (TypePointer t m)  = pointerWidth m
> typeWidth (TypeArray t n m)  = n * (typeWidth t)   
> typeWidth (TypeInt) = 8 -- XXX
> typeWidth t = error ("width for type " ++ (show t)++  " meangingless")
> typeAlign (TypeStruct {mm=m})= defaultAlign m
> typeAlign (TypePointer t m)  = defaultAlign m
> typeAlign (TypeUserInt w al) = al
> typeAlign (TypeInt) = 8 -- XXX
> typeAlign (TypeArray t n m)  = defaultAlign m
> typeAlign t = error ("alignment for type " ++ (show t) ++ " meangingless")
> align i al = i + (mod (al - i) al)

> undefinedStruct = TypeUndefinedStruct
