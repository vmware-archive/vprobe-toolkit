Copyright 2007-2011, VMware, Inc.

> module Ident(Ident, makeIdent, emitIdent, setIdentOff, getIdentOff)
>   where
> data Ident = IdentString { nm :: String, offset :: (Maybe Int) }
> instance Show(Ident) where
>    show (IdentString {nm = string}) = string
> instance Ord Ident where
>    compare (IdentString {nm = s1}) (IdentString {nm = s2}) = compare s1 s2
> instance Eq Ident where
>    (IdentString {nm = a}) == (IdentString {nm = b}) = (a == b)
> emitIdent (IdentString {nm = string}) = string
> makeIdent str = IdentString {nm = str, offset = Nothing}
> setIdentOff i@IdentString{} off = i{offset = (Just off)}
> getIdentOff IdentString{offset = off} = off
