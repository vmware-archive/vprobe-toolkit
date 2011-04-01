# Makefile for building the emmett compiler jar.
#
# Usage: make -f Makefile.java <options>
# where the options are:
#  OSTYPE=<os>     set to w32 on windows hosts
#  OCAMLJAVA=<dir> path to the ocaml jar files (from cadmium distribution)
#
# See http://ocamljava.x9c.fr/ for more details regarding OCaml-Java.
#

ifeq ($(OCAMLJAVA),)
$(error Please set OCAMLJAVA to the path to the Ocaml to java compiler)
endif

ifeq ($(OSTYPE), w32)
CP		:= copy
else
CP		:= cp
endif

SRCDIR		:= .
TGTDIR		:= build

PROGNAME  := emmett
EXTENSION := .jar
EXEC      := $(TGTDIR)/$(PROGNAME)$(EXTENSION)

OCAMLLEX   := ocamllex
OCAMLYACC  := ocamlyacc

OCAMLJ  := java -Xss500m -Xmx700m -Xms500m -jar $(OCAMLJAVA)/bin/ocamljava.jar
OCAMLLINK  := java -jar $(OCAMLJAVA)/bin/ocamljava.jar -standalone
# Ocaml object files. Must be ordered by dependencies.
ML_OBJS   := $(addprefix $(TGTDIR)/,                                     \
		defaults.cmj globals.cmj memmodel.cmj ast.cmj symtab.cmj \
		predef.cmj type.cmj actions.cmj lower.cmj     \
		vp.cmj lexer.cmj parser.cmj main.cmj)

# Main target
build: setup $(EXEC)

setup:
	mkdir -p $(TGTDIR)

# Build the executable
$(EXEC): $(ML_OBJS)
	$(OCAMLLINK) -o $@ $(ML_OBJS)

# Generate lexer.ml
$(TGTDIR)/lexer.ml: $(SRCDIR)/lexer.mll
	$(OCAMLLEX) -q -o $@ $<

# Generate parser.ml
$(TGTDIR)/parser.ml: $(SRCDIR)/parser.mly
	$(CP) $< $(TGTDIR)
	$(OCAMLYACC) $(TGTDIR)/$(<F)
	$(RM) $(TGTDIR)/$(<F)
	$(RM) $(TGTDIR)/$(@F)i

# Use defaults for the hosted compiler
$(TGTDIR)/defaults.ml: $(SRCDIR)/defaults.hosted.ml
	$(RM) $@
	$(CP) $< $@

# Copy source files to build directory (they are needed by ocamlc)
$(TGTDIR)/%.ml: $(SRCDIR)/%.ml
	$(RM) $@
	$(CP) $< $@

# Compile ocaml source code
$(TGTDIR)/%.cmj: $(TGTDIR)/%.ml
	cd $(TGTDIR) && $(OCAMLJ) -c  $(<F)

# Object file dependencies
$(TGTDIR)/ast.cmj: $(addprefix $(TGTDIR)/, \
	memmodel.cmj)

$(TGTDIR)/symtab.cmj: $(addprefix $(TGTDIR)/, \
	memmodel.cmj globals.cmj defaults.cmj ast.cmj)

$(TGTDIR)/predef.cmj: $(addprefix $(TGTDIR)/, \
	symtab.cmj ast.cmj)

$(TGTDIR)/type.cmj: $(addprefix $(TGTDIR)/, \
	symtab.cmj predef.cmj memmodel.cmj globals.cmj ast.cmj)

$(TGTDIR)/actions.cmj: $(addprefix $(TGTDIR)/, \
	type.cmj symtab.cmj predef.cmj ast.cmj)

$(TGTDIR)/lower.cmj: $(addprefix $(TGTDIR)/, \
	type.cmj symtab.cmj memmodel.cmj globals.cmj ast.cmj)

$(TGTDIR)/vp.cmj: $(addprefix $(TGTDIR)/, \
	type.cmj symtab.cmj predef.cmj globals.cmj \
	defaults.cmj ast.cmj)

$(TGTDIR)/main.cmj: $(addprefix $(TGTDIR)/, \
	vp.cmj type.cmj symtab.cmj predef.cmj parser.cmj \
	lower.cmj lexer.cmj globals.cmj defaults.cmj)

$(TGTDIR)/lexer.cmj: $(addprefix $(TGTDIR)/, \
	symtab.cmj parser.cmj)

$(TGTDIR)/parser.cmj: $(addprefix $(TGTDIR)/, \
	symtab.cmj globals.cmj ast.cmj actions.cmj)

clean:
	$(RM) -r $(TGTDIR)

# Declare phony targets
.PHONY: build setup clean
