#!/bin/sh

# run this script when you've changed the compiler's output.
for t in test/*.emt; do
   echo $t; ./emmett $t > $t.out
done
