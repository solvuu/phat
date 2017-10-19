.PHONY: all
all:
	jbuilder build @install --dev

.PHONY: clean
clean:
	jbuilder clean
