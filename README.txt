License
-------

See License.txt

Misc
----

This is a set of scripts and programs for providing a textual user
interface around the VProbes instrumentation mechanism. These scripts
assume a UNIX-like environment. To date, they've been tested under
Cygwin/Windows XP, ESX 3.5 (Console OS), Linux, and Mac OS X. Some
modifications might be necessary; please contact Keith Adams and/or Rob
Benson (kma@vmware.com, rbenson@vmware.com) with any problems you
encounter.

Compiling
---------

The Emmett compiler is written in Haskell. To compile it, you'll need
a version of GHC (the Glasgow Haskell Compiler) for your platform,
invokable as "ghc" from your $PATH.

Installing
----------

Invoke ./install.sh, which will compile and install our compiler in-place,
and provide instructions for setting environment variables. If you'd like
to install into /usr/bin, /usr/local/bin, etc., copy all the files in 
`pwd`/bin/* into the same destination directory.

Thanks for giving VProbes a whirl!

Team VProbes

