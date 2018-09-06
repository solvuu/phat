.PHONY: all
all:
	dune build @install

.PHONY: clean
clean:
	dune clean

.PHONY: docker
docker:
	docker build -t phat .
