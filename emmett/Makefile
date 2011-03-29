LHSFILES=Ident.lhs MemModel.lhs Type.lhs IdMap.lhs CompileCtx.lhs	\
	Expr.lhs Statement.lhs Probe.lhs Program.lhs Parse.lhs		\
	Main.lhs

#GHCFLAGS=-O4
GHCFLAGS=--make
GHC=ghc

emmett: $(LHSFILES)
	$(GHC) -static $(GHCFLAGS) -o $@ $(LHSFILES)

clean:
	rm -f *.o *.lhs~ *.hi emmett
