USE_OPT   := 1

OCAMLFIND := ocamlfind

OCAML_FLAGS := 

ifdef USE_OPT
  OCAMLFLAGS  += -noassert -unsafe
  OCAMLC      := $(OCAMLFIND) ocamlopt
  OCAMLYACC   := ocamlyacc
  OCAMLLEX    := ocamllex.opt
  OCAMLDEP    := ocamldep.opt -native
  OEXT  := cmx
  IEXT  := cmxi
  LOEXT := cmxa
else
  OCAMLC      := $(OCAMLFIND) ocamlc
  OCAMLYACC   := ocamlyacc
  OCAMLLEX    := ocamllex
  OCAMLDEP    := ocamldep
  OEXT  := cmo
  IEXT  := cmi
  LOEXT := cma
endif

SOURCES := \
	ffi.ml \
	test.ml

OBJS := $(SOURCES:.ml=.$(OEXT))

PACKAGES := dynlink

GL_PACKS = glMLite glMLite.glut glMLite.glu
GFX_FLAGS = -I +sdl2 sdl2.cmxa $(GL_PACKS:%=-package %) \
	-cclib "-framework CoreFoundation -framework Cocoa"

BUILD_FLAGS := $(OCAML_FLAGS) $(PACKAGES:%=-package %)
LINK_FLAGS  := $(BUILD_FLAGS) -linkpkg

test: $(OBJS) extern.cmxs
	$(OCAMLC) $(LINK_FLAGS) $(OBJS) -o $@

gfx: extern_gfx.cmxs

extern.$(OEXT): extern.ml
	$(OCAMLC) $(BUILD_FLAGS) $^ -c

extern.cmxs: ffi.$(OEXT) extern.$(OEXT) 
	$(OCAMLC) $(LINK_FLAGS) -shared $^ -o extern.cmxs

extern_gfx.$(OEXT): extern_gfx.ml
	$(OCAMLC) $(BUILD_FLAGS) $(GFX_FLAGS) $^ -c

extern_gfx.cmxs: ffi.$(OEXT) extern_gfx.$(OEXT) 
	$(OCAMLC) $(LINK_FLAGS) $(GFX_FLAGS) -shared $^ -o extern_gfx.cmxs

clean_build:
	rm -rf *~ *.cm* *.o *.annot imgrun lexer.ml parser.ml parser.mli parser.output *.dSYM .dep .DS_Store 

clean:
	make clean_build
	rm -Rf test

.PHONY: clean depend

include .dep

depend: .dep

%.ml : %.mll
	$(OCAMLLEX) $<

%.ml %.mli : %.mly
	$(OCAMLYACC) $<

%.cmi : %.mli
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmx %.cmxi : %.ml
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmo %.cmi : %.ml
	$(OCAMLC) $(BUILD_FLAGS) -c $<

.dep: $(SOURCES)
	$(OCAMLDEP) $(SOURCES) > .dep
