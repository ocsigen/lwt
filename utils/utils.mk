# utils.mk
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of Lwt.

# +------------------------------------------------------------------+
# | Tools                                                            |
# +------------------------------------------------------------------+

# Tools
OCAMLFIND := ocamlfind
OCAMLBUILD := ocamlbuild

# Use classic-display when compiling under a terminal which does not
# support ANSI sequence:
ifeq ($(TERM),dumb)
OCAMLBUILD += -classic-display
endif

# +------------------------------------------------------------------+
# | Functions                                                        |
# +------------------------------------------------------------------+

# Execute the given command and return "yes" if it succeeds, and the
# empty string if it fails:
exec = $(shell $(1) &> /dev/null && echo -n yes)

# Check for the presence of an ocaml package installed with findlib:
have_package = $(call exec,$(OCAMLFIND) query $(1))

# Returns "yes" or "no " according to the boolean value of the first
# argument:
yes_no = $(if $(strip $(1)),yes,no )

# +------------------------------------------------------------------+
# | Common checking                                                  |
# +------------------------------------------------------------------+

HAVE_FINDLIB := $(call exec,$(OCAMLFIND) printconf)
HAVE_BYTE := $(call exec,$(OCAMLFIND) ocamlc -version)
HAVE_NATIVE := $(call exec,$(OCAMLFIND) ocamlopt -version)
HAVE_OCAMLBUILD := $(call exec,$(OCAMLBUILD) -version)

ifeq ($(HAVE_FINDLIB),)
  MISSING := yes
  $(warning findlib is missing!)
endif

ifeq ($(HAVE_BYTE),)
  MISSING := yes
  $(warning ocamlc is missing!)
endif

ifeq ($(HAVE_OCAMLBUILD),)
  MISSING := yes
  $(warning ocamlbuild is missing!)
endif

ifeq ($(MISSING),yes)
  $(error Needed dependencies are missing, cannot build! (see README for details))
endif
