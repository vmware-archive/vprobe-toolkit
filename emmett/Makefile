# Makefile for building the emmett compiler executable.
#
# Usage: make -f Makefile <options>
# where the options are:
#  OSTYPE=<os>    the host os type: win, linux, or mac (required)
#  TGTDIR=<dir>   the target directory (required)
#

ifneq ($(OSTYPE), win)
ifneq ($(OSTYPE), linux)
ifneq ($(OSTYPE), mac)
$(error Invalid host OS type (must be win, linux, or mac))
endif
endif
endif

ifeq ($(OSTYPE), Win32)
EXE_EXTENSION	:= .exe
CP		:= copy
else
EXE_EXTENSION	:=
CP		:= cp
endif

ifeq ($(TGTDIR),)
$(error Undefined target directory (TGTDIR))
endif

SRCDIR		:= .
TGTDIR		:= 

PROGNAME  := emmett
EXEC      := $(TGTDIR)/$(PROGNAME)$(EXTENSION)

OCAMLC	  = ocamlc
OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc

ifeq ($(OSTYPE), Win32)
OCAMLLINK = $(SRCDIR)/ocamllink.bat
else
OCAMLLINK = $(OCAMLC) -custom
endif

# Ocaml object files. Must be ordered by dependencies.
ML_OBJS = $(addprefix $(TGTDIR)/,					    \
		defaults.cmo globals.cmo memmodel.cmo ast.cmo symtab.cmo    \
		predef.cmo type.cmo actions.cmo lower.cmo vp.cmo \
		lexer.cmo parser.cmo main.cmo)

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

# Generate parser.ml. Also remove parser.mli. Dealing with interfaces
# (parser.cmi and parser.mli) has caused problems for incremental
# builds when compiling to native code due to link-time module
# implementation checks. Such checks essentially nullify the benefit
# of interfaces: modules depend not only on other module's interfaces,
# but also on other module's implementations. See PR 553266. Even
# though this should not be an issue here since we compile to
# bytecodes, let's have all scons and make files use the same
# dependency graph.
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
$(TGTDIR)/%.cmo: $(TGTDIR)/%.ml
	cd $(TGTDIR) && $(OCAMLC) -c $(<F)

# Object file dependencies
$(TGTDIR)/ast.cmo: $(addprefix $(TGTDIR)/, \
	memmodel.cmo)

$(TGTDIR)/symtab.cmo: $(addprefix $(TGTDIR)/, \
	memmodel.cmo globals.cmo defaults.cmo ast.cmo)

$(TGTDIR)/predef.cmo: $(addprefix $(TGTDIR)/, \
	symtab.cmo ast.cmo)

$(TGTDIR)/type.cmo: $(addprefix $(TGTDIR)/, \
	symtab.cmo predef.cmo memmodel.cmo globals.cmo ast.cmo)

$(TGTDIR)/actions.cmo: $(addprefix $(TGTDIR)/, \
	type.cmo symtab.cmo predef.cmo ast.cmo)

$(TGTDIR)/lower.cmo: $(addprefix $(TGTDIR)/, \
	type.cmo symtab.cmo memmodel.cmo globals.cmo ast.cmo)

$(TGTDIR)/vp.cmo: $(addprefix $(TGTDIR)/, \
	type.cmo symtab.cmo predef.cmo globals.cmo \
	defaults.cmo ast.cmo)

$(TGTDIR)/main.cmo: $(addprefix $(TGTDIR)/, \
	vp.cmo type.cmo symtab.cmo predef.cmo parser.cmo \
	lower.cmo lexer.cmo globals.cmo defaults.cmo)

$(TGTDIR)/lexer.cmo: $(addprefix $(TGTDIR)/, \
	symtab.cmo parser.cmo)

$(TGTDIR)/parser.cmo: $(addprefix $(TGTDIR)/, \
	symtab.cmo globals.cmo ast.cmo actions.cmo)

clean:
	$(RM) -r $(TGTDIR)

# Declare phony targets
.PHONY: build setup clean
