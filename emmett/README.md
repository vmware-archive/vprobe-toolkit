This is a compiler for Emmett, a simple dynamic instrumentation
language. It currently targets vp, the "portable assembly language"
for VMware's VProbes instrumentation facility.

Examples
--------

An Emmett program is a series of declarations, and probe / statement pairs.
E.g., in the canonical Hello world:

   VMM1Hz printf("Hi!\n");

We're pairing an event of interest (VMM1Hz, which happens once a second
in the VMM) with a statement to be executed (printf("Hi!\n")).

We can group statements together with { }'s:

    VMM1Hz {
	var = 123;
	printf("var: %d\n", var);
    }

The user can also define typed functions:

    int myfunc(int arg1, string arg2)
    {
	printf("%s: %d\n", arg2, arg1);
	return arg1 + 1;
    }

...and subroutines:

    void myfunc(int arg1, string arg2)
    {
	    printf("%s: %d\n", arg2, arg1);
    }

if/else behave as a C programmer would expect, and integer expressions
roughly follow C's precedence and syntax.

Bugs/Limitations
----------------

Currently all variables are global, and can be implicitly declared by
being assigned a given type. The only exception is function arguments,
which have function scope. It would be nice to provide block-local
variables; these could be supported using anonymous functions.

Building
--------

You'll need the OCaml Compiler, which is available for all popular
platforms.  make in the emmett directory should suffice.
