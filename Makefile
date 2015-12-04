OCB=ocamlbuild

all: lib/phat_pure.cma lib/phat_pure.cmxa lib/phat_async_unix.cma \
     lib/phat_async_unix.cmxa app/run_ounit_tests.native \
     app/run_ounit_tests.byte

%.cma:
	$(OCB) $@

%.cmxa:
	$(OCB) $@

%.native:
	$(OCB) $@

%.byte:
	$(OCB) $@

test: app/run_ounit_tests.native
	./`basename $<`

clean:
	$(OCB) -clean

.PHONY: test clean all
