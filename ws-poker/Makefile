USE_OPT   := 1

OCAMLFIND := ocamlfind

OCAML_FLAGS := -linkpkg -linkall

ifdef USE_OPT
  OCAMLFLAGS  += -noassert -unsafe
  OCAMLC      := $(OCAMLFIND) ocamlopt
  OCAMLDEP    := ocamldep.opt -native
  OEXT  := cmx
  IEXT  := cmxi
  LOEXT := cmxa
  PEXT  := cmxs
else
  OCAMLC      := $(OCAMLFIND) ocamlc
  OCAMLDEP    := ocamldep
  OEXT  := cmo
  IEXT  := cmi
  LOEXT := cma
  PEXT  := cma
endif

SOURCES_COMMON := \
	util.ml \
	wsffi.ml

SOURCES_TEST := \
	test.ml

SOURCES_POKER := \
	sdldefs.ml \
	glutil.ml sdlutil.ml \
	loader.ml \
	cards.ml \
	glcards.ml \
	poker.ml

OBJS_COMMON := $(SOURCES_COMMON:.ml=.$(OEXT))
OBJS_TEST := $(SOURCES_TEST:.ml=.$(OEXT))
OBJS_POKER := $(SOURCES_POKER:.ml=.$(OEXT))

PACKS_COMMON := dynlink extlib unix
PACKS_TEST := dynlink extlib unix
PACKS_POKER := extlib sdl2 sdl2_img glMLite glMLite.glut glMLite.glu

BUILD_FLAGS_COMMON := $(OCAML_FLAGS) $(PACKS_COMMON:%=-package %)
BUILD_FLAGS_TEST := $(BUILD_FLAGS_COMMON) $(PACKS_TEST:%=-package %)
BUILD_FLAGS_POKER := $(BUILD_FLAGS_COMMON) $(PACKS_POKER:%=-package %) \
	-cclib "-framework CoreFoundation -framework Cocoa"

LINK_FLAGS_COMMON := $(BUILD_FLAGS_COMMON) 
LINK_FLAGS_TEST := $(BUILD_FLAGS_TEST)
LINK_FLAGS_POKER := $(BUILD_FLAGS_POKER)

poker.$(PEXT): $(OBJS_COMMON) $(OBJS_POKER)
	$(OCAMLC) $(LINK_FLAGS_POKER) -shared $^ -o $@

test: $(OBJS_COMMON) $(OBJS_TEST) poker.$(PEXT)
	$(OCAMLC) $(LINK_FLAGS_TEST) $(OBJS_COMMON) $(OBJS_TEST) -o $@

run: test
	./test

clean_build:
	rm -rf *~ *.cm* *.o *.annot test *.dSYM .dep .DS_Store 

clean:
	make clean_build
	rm -Rf test

.PHONY: clean depend

include .dep

depend: .dep

$(SOURCES_COMMON:.ml=.$(OEXT)) : $(@:.$(OEXT)=.ml)
	$(OCAMLC) $(BUILD_FLAGS_COMMON) -c $(@:.$(OEXT)=.ml)
$(SOURCES_TEST:.ml=.$(OEXT)) : $(@:.$(OEXT)=.ml)
	$(OCAMLC) $(BUILD_FLAGS_TEST) -c $(@:.$(OEXT)=.ml)
$(SOURCES_POKER:.ml=.$(OEXT)) : $(@:.$(OEXT)=.ml)
	$(OCAMLC) $(BUILD_FLAGS_POKER) -c $(@:.$(OEXT)=.ml)

.dep: $(SOURCES)
	$(OCAMLDEP) $(SOURCES_COMMON) $(SOURCES_POKER) $(SOURCES_TEST) > .dep
