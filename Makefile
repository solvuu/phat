PROJECT = phat
LIBS = pure async_unix
APPS = phat_tests

# Default targets to build for developers.
default: byte project_files.stamp

native: $(patsubst %,lib/$(PROJECT)_%.cmxa,$(LIBS)) \
	$(patsubst %,lib/$(PROJECT)_%.cmxs,$(LIBS)) \
	$(patsubst %,app/%.native,$(APPS))

byte: $(patsubst %,lib/$(PROJECT)_%.cma,$(LIBS)) \
	$(patsubst %,app/%.byte,$(APPS))

%.cma %.cmxa %.cmxs %.native %.byte:
	ocamlbuild $@

project_files.stamp .merlin META $(PROJECT).install:
	ocamlbuild $@

tests: app/phat_tests.native
	_build/$<

clean:
	ocamlbuild -clean

.PHONY: default all native byte clean test
