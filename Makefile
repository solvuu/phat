PROJECT=$(shell grep "^name" opam/opam | cut -d\" -f2)
LIBS = $(addprefix $(PROJECT)_,$(basename $(notdir $(wildcard lib/*))))
APPS = $(basename $(notdir $(wildcard app/*)))

OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag "package(opam-lib)"

default: byte project_files.stamp

native: $(patsubst %,lib/%.cmxa,$(LIBS)) \
	$(patsubst %,lib/%.cmxs,$(LIBS)) \
	$(patsubst %,app/%.native,$(APPS))

byte: $(patsubst %,lib/%.cma,$(LIBS)) \
	$(patsubst %,app/%.byte,$(APPS))

%.cma %.cmxa %.cmxs: %.mlpack
	$(OCAMLBUILD) $@

%.native %.byte lib/%.mlpack:
	$(OCAMLBUILD) $@

project_files.stamp META:
	$(OCAMLBUILD) $@

.merlin $(PROJECT).install:
	$(OCAMLBUILD) $@ && ln -s _build/$@ $@

clean:
	$(OCAMLBUILD) -clean

.PHONY: default native byte clean
