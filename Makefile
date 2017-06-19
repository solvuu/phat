.PHONY: all
all:
	jbuilder build @install -j 2

.PHONY: clean
clean:
	rm -rf _build
	rm -f phat-{base,async}.install
	rm -f app/.merlin lib/{base,async}/.merlin
