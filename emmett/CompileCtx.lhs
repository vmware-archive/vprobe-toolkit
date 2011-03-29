Copyright 2008, VMware, Inc. All rights reserved.

CompileCtx -- The compiler context.

> module CompileCtx(CompileCtx(), makeCompileCtx, nullCtx, varType,
>                   typeNameToType, structNameToType, addType, addStruct, addVar,
>                   pushCtx, popCtx, getVars, decls, getMemModel, pushMemModel,
>                   popMemModel, getDeclArgs, setDeclArgs,
>                   enumLookup, addEnumVal, addEnum, setEnum) where
> import IdMap
> import Ident
> import Type
> import MemModel
> import Data.Map (Map)
> import qualified Data.Map as Map
> data CompileCtx = Ctx {ids, types :: IdMap,
>                        enums :: (Map Ident Integer),
>                        mm :: [MemModel],
>                        args :: [(Type, Ident)],
>                        parent :: CompileCtx,
>                        curEnum :: Integer } |
>                   NullCtx
> instance Show CompileCtx where
>    show (Ctx {ids=ids, types=tps, mm=m}) =
>     ("---ids: "++ (show ids) ++ "---types: " ++ (show tps) ++
>      "---mem model: " ++ (show m) ++ "\n")
> makeCompileCtx idmap typemap = Ctx { ids = idmap,
>                                      types = typemap,
>                                      enums = (Map.fromList []),
>                                      mm = [defaultMemModel],
>                                      args = [],
>                                      curEnum = 0,
>                                      parent = NullCtx }
> nullCtx = NullCtx

Struct names are logically separate from type and variable names. Rather
than keep a separate map just for them, let's keep them in the typemap and
hide them behind an impossible identifier.

> toStruct id = makeIdent ("struct " ++ (show id))

Map a variable name to a type.

> varType (Ctx {ids=ids, parent=p}) id = if tp == TypeInvalid then
>                              varType p id else tp where
>                              tp = idToType ids id
> varType NullCtx  id = TypeInvalid

Map a typename to a type. For structures, just saying "struct foo"
introduces an as-yet-undefined struct, so filter idMaps desire to
return TypeInvalid.

> typeNameToType (Ctx {types=tps}) id = idToType tps id
> typeNameToType NullCtx  id = TypeInvalid
> structNameToType (Ctx {types=tps}) id = 
>   let rawType = idToType tps (toStruct id) in
>       case rawType of
>          TypeInvalid -> undefinedStruct
>          _ -> rawType

Add types, variables.

> addType :: CompileCtx -> Ident -> Type -> CompileCtx
> addType c@Ctx {types=tps} id tp = c {types=(idMapAdd tps id tp)}
> addStruct ctx id tp = addType ctx (toStruct id) tp
> addVar :: CompileCtx -> Ident -> Type -> CompileCtx
> addVar  c@Ctx {ids=ids} id tp = c {ids=(idMapAdd ids id tp)}

push/pop: used to get at locals, etc.

> pushCtx c oldCtx = c{parent=oldCtx}
> popCtx Ctx{parent=NullCtx} = error "popped top-level context"
> popCtx Ctx{parent=p} = p

getVars: obtain the variable identifiers from this context

> getVars NullCtx = []
> getVars Ctx{ids=ids} = idKeys ids

decls: declare all the variables in the compile context.

> decls :: CompileCtx -> String
> decls NullCtx = ""
> decls Ctx{ids=ids, parent=p} = (idMapDecls ids) ++ (decls p)

> getMemModel Ctx{mm=m} = head m
> pushMemModel mnew c@Ctx{mm=mold} = c{mm=(mnew:mold)}
> popMemModel c@Ctx{mm=(m:old)} = c{mm=old}
> popMemModel Ctx{mm=[]} = error "popped outer memmodel"

> getDeclArgs Ctx{args=args} = args
> setDeclArgs args c = c{args=args}

Enumerator semantics. Only the compileCtx knows for sure if an ident is
a variable or just an enumerator value. We also maintain the internal
enumerator state here.


> incEnum c@Ctx{curEnum = n} = c{curEnum = n + 1}
> getEnum Ctx{curEnum = n} = n

> addEnumVal id v c@Ctx{enums=e, curEnum=cur} = c{enums=Map.insert id v e,
>                                                 curEnum=v}
> addEnum id c@Ctx{enums=e, curEnum=cur} = c{enums=Map.insert id (cur+1) e,
>                                            curEnum=cur+1}
> setEnum n c@Ctx{} = c{curEnum = n}
> enumLookup id Ctx{enums=e} = Map.lookup id e