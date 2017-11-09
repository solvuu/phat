FROM ubuntu:14.04

RUN apt-get update && \
    apt-get install -yq \
      curl \
      build-essential \
      aspcud git m4 patch rsync unzip

ENV \
  OPAMYES=true \
  OCAML_VERSION=4.03.0 \
  OPAM_VERSION=1.2.2

RUN \
  curl -o /usr/local/bin/opam -sSL "https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-x86_64-Linux" && \
  chmod 755 /usr/local/bin/opam
RUN opam init --comp="$OCAML_VERSION" --no-setup
RUN opam install depext
ENV PATH="/root/.opam/$OCAML_VERSION/bin:$PATH"

COPY . /src/phat
RUN \
  opam pin add -n phat-base /src/phat && \
  opam pin add -n phat-async /src/phat

RUN opam depext phat-async && opam install phat-async

CMD ["/bin/bash"]
