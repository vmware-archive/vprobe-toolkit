Test driver for Type module.

An empty structure.

> module TypeTest where
> import Type
> import Ident
> emptyStruct = typeCreateStruct []

Some handy integers:

> uint32 = typeCreateInt 4 4
> uint64 = typeCreateInt 8 8
> uint8  = typeCreateInt   1 1

An oddly packed structure:

> structFields = [(uint8, "c"), (uint32, "my32"), (uint64, "my64")]
> structIds = map (\(t,s) -> (t, IdentString s)) structFields
> myStruct = typeCreateStruct structIds
> testOffset fNm = typeStructOffset myStruct fNm
