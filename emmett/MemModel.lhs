Copyright 2008, VMware, Inc.

A MemModel is a pointer width and access mode. E.g., 32-bit guest pointers,
64-bit guest pointers, etc.

> module MemModel(MemModel(), nameToMemModel, defaultMemModel, pointerWidth,
>                 defaultAlign, emitRef) where
> data MemModel = Mem Int String
> instance Show MemModel where
>    show (Mem i s) = (show i) ++ (" bytes, ") ++ s
> instance Eq MemModel where
>     Mem i s == Mem j t = i == j && s == t
> nameToMemModelMap = [ ("guest32", memModelGuest32),
>                       ("guest64", memModelGuest64),
>                       ("guestphys", memModelGuestPhys),
>                       ("vmm32",   memModelVMM32),
>                       ("vmm64",   memModelVMM64),
>                       ("vmk",     memModelVMKernel) ]
> nameToMemModel s = case lookup s nameToMemModelMap of
>                      Just m -> m
>                      Nothing -> error ("unknown memory model: " ++ s)
> defaultMemModel  = memModelGuest64
> memModelVMM32 = Mem 4   "getmonitor"
> memModelVMM64 = Mem 8   "getmonitor"
> memModelGuest32 = Mem 4 "getguest"
> memModelGuest64 = Mem 8 "getguest"
> memModelGuestPhys = Mem 8 "getguestphys"
> memModelVMKernel = Mem 8 "getvmkernel"
> pointerWidth (Mem w _) = w
> defaultAlign = pointerWidth
> emitRef (Mem w s) = s ++ " "
