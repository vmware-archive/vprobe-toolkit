# Makefile for building the emmett compiler jar.
#
# Usage: make -f Makefile.java <options>
# where the options are:
#  OSTYPE=<os>    the host os type: win, linux, or mac (required)
#  TGTDIR=<dir>   the target directory (required)
#

SRCDIR	:= .

ifeq ($(OSTYPE),)
$(error Undefined host OS type (OSTYPE))
endif

ifneq ($(OSTYPE), win)
ifneq ($(OSTYPE), linux)
ifneq ($(OSTYPE), mac)
$(error Invalid host OS type (must be win, linux, or mac))
endif
endif
endif

ifeq ($(TGTDIR),)
$(error Undefined target directory (TGTDIR))
endif

PROGNAME  := emmett
EXTENSION := .jar
EXEC      := $(TGTDIR)/$(PROGNAME)$(EXTENSION)

OCAMLROOT  := /build/toolchain/mac32/ocaml-3.11.0/bin/
OCAMLLEX   := $(OCAMLROOT)ocamllex
OCAMLYACC  := $(OCAMLROOT)ocamlyacc

OCAMLJBIN  := $(HOME)/Downloads/ocamljava-bin-1.4/bin
OCAMLJAVA  := java -Xss500m -Xmx700m -Xms500m -jar $(OCAMLJBIN)/ocamljava.jar
OCAMLLINK  := java -jar $(OCAMLJBIN)/ocamljava.jar -standalone
# Ocaml object files. Must be ordered by dependencies.
ML_OBJS   := $(addprefix $(TGTDIR)/,                                     \
		defaults.cmj globals.cmj memmodel.cmj ast.cmj symtab.cmj \
		predef.cmj type.cmj actions.cmj lower.cmj     \
		vp.cmj lexer.cmj parser.cmj main.cmj)

# Main target
$(EXEC): $(ML_OBJS)
	$(OCAMLLINK) -o $(EXEC) $(ML_OBJS)

$(TGTDIR)/%.ml: $(SRCDIR)/%.ml
	$(RM) -f $@
	$(CP) $< $@

# Generate lexer.ml
$(TGTDIR)/lexer.ml: $(SRCDIR)/lexer.mll
	$(OCAMLLEX) -q -o $@ $<

# Generate parser.ml
$(TGTDIR)/parser.ml: $(SRCDIR)/parser.mly
	$(CP) $< $(TGTDIR)
	$(OCAMLYACC) $(TGTDIR)/$(<F)
	$(RM) -f $(TGTDIR)/$(<F)
	$(RM) -f $(TGTDIR)/$(@F)i

$(TGTDIR)/%.cmj: $(TGTDIR)/%.ml
	cd $(TGTDIR) && $(OCAMLJAVA) -c  $(<F)

# Use defaults for the hosted compiler
$(TGTDIR)/defaults.ml: $(SRCDIR)/defaults.hosted.ml
	$(CP) $(<) $(@)

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
	$(RM) $(ML_OBJS) $(ML_OBJS:.cmj=.cmi) \
	       $(TGTDIR)/parser.ml $(TGTDIR)/parser.mli \
	       $(TGTDIR)/lexer.ml $(EXEC)
