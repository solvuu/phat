PROJECT = phat
LIBS = $(basename $(notdir $(wildcard lib/*.mlpack)))
APPS = $(basename $(notdir $(wildcard app/*)))

# Default targets to build for developers.
default: byte project_files.stamp

native: $(patsubst %,lib/%.cmxa,$(LIBS)) \
	$(patsubst %,lib/%.cmxs,$(LIBS)) \
	$(patsubst %,app/%.native,$(APPS))

byte: $(patsubst %,lib/%.cma,$(LIBS)) \
	$(patsubst %,app/%.byte,$(APPS))

%.cma %.cmxa %.cmxs %.native %.byte:
	ocamlbuild $@

project_files.stamp META:
	ocamlbuild $@

.merlin $(PROJECT).install:
	ocamlbuild $@ && ln -s _build/$@ $@

tests: app/phat_tests.native
	_build/$<

clean:
	ocamlbuild -clean

.PHONY: default all native byte clean test
