#!/bin/bash

function die {
   echo $1 1>&2
   exit 1
}

dir=$(pwd)
emmettdir=$(pwd)/emmett
emmettbin=$emmettdir/emmett

ghc --version              || die "could not invoke ghc"
(cd $emmettdir; make )     || die "could not build emmett"
[ -x $emmettdir/emmett   ] || die "could not find emmett binary in $emmettbin"
cp $emmettdir/emmett bin/  || die "could not copy $emmettbin to bin"

echo \
"Add $dir/bin to your PATH environment variable, e.g.,
    export PATH=\$PATH:$dir/bin # (sh, bash, zsh)
 or
    setenv PATH \$PATH:$dir/bin # (csh, tcsh)
and invoke vprobe"
