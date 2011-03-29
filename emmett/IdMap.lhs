Copyright 2007, VMware, Inc.

Map of identifiers to types. We use it to check the program's validity
(we don't, e.g., assign strings to integers), to emit code (e.g.,
distinguishing string keys from integer keys in aggregate samples),
and to declare variables in our output. This does double duty for matching
typedef names to types as well. Thin wrapper around Haskell's Map type.

> module IdMap(IdMap(),
>         buildIdMap, buildOrderedIdMap, idToType, idMapAdd, idKeys, idTypes,
>         idMapDecls) where
> import Ident
> import Type
> import Data.Map (Map)
> import qualified Data.Map as Map
> data IdMap = Idm (Map Ident Type) |
>              OrderedIdMap [ (Ident, Type) ]
> instance Show IdMap where
>   show (Idm m) = show m

> buildIdMap :: [ (Ident, Type) ] -> IdMap
> buildIdMap l = Idm (Map.fromList l)
> buildOrderedIdMap :: [ (Ident, Type) ] -> IdMap
> buildOrderedIdMap l = OrderedIdMap l
> idMapAdd :: IdMap -> Ident -> Type -> IdMap
> idMapAdd idm@(Idm m) id ty = case (Map.lookup id m) of
>    Just ty -> idm
>    Nothing -> Idm (Map.insert id ty m)
> idMapAdd (OrderedIdMap l) id ty = OrderedIdMap (l ++ [(id, ty)])

idToType: given an ID, return a type

> idToType :: IdMap -> Ident -> Type
> idToType (Idm m) id = case (Map.lookup id m) of
>     Just ty -> ty
>     Nothing -> TypeInvalid
> idToType (OrderedIdMap ((cur, ty):xs)) id = if cur == id then ty else
>                                             idToType (OrderedIdMap xs) id
> idToType (OrderedIdMap []) _ = TypeInvalid

idKeys: return all the IDs for a map

> idKeys :: IdMap -> [Ident]
> idKeys (Idm m) = Map.keys m
> idKeys (OrderedIdMap l) = map fst l
  
idTypes: return the types in order.

> idTypes (Idm m) = Map.elems m
> idTypes (OrderedIdMap l) = map snd l

> idMapDecls :: IdMap -> String
> idMapDecls (Idm m) = 
>    Map.foldWithKey (\id tp ids -> (typeDecl id tp) ++ ids) [] m
> idMapDecls (OrderedIdMap m) = 
>    foldr (\(id, tp) s -> (typeDecl id tp) ++ s) [] m
