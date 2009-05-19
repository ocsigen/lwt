# utils.mk
# --------
#
# Copyright (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# All rights reserved.
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Jeremie Dimino nor the names of his
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE AUTHOR AND CONTRIBUTORS BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
exec = $(shell $(1) &> /dev/null && echo -n yes || true)

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

ifeq ($(HAVE_OCAMLOPT),)
  OCAMLBUILD += -byte-plugin
endif

ifeq ($(MISSING),yes)
  $(error Needed dependencies are missing, cannot build! (see README for details))
endif
