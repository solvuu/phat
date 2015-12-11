PROJECT = phat
LIBS = pure async_unix
APPS = run_ounit_tests

# Default targets to build for developers.
default: byte project_files.stamp

native: $(patsubst %,lib/$(PROJECT)_%.cmxa,$(LIBS)) \
	$(patsubst %,lib/$(PROJECT)_%.cmxs,$(LIBS)) \
	$(patsubst %,app/%.native,$(APPS))

byte: $(patsubst %,lib/$(PROJECT)_%.cma,$(LIBS)) \
	$(patsubst %,app/%.byte,$(APPS))

%.cma %.cmxa %.cmxs %.native %.byte:
	ocamlbuild $@

project_files.stamp META:
	ocamlbuild $@

test: app/run_ounit_tests.native
	_build/$<

clean:
	ocamlbuild -clean

.PHONY: default all native byte clean test
