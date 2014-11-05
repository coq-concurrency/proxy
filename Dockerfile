FROM ubuntu:14.10
MAINTAINER Guillaume Claret

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y gcc make git
RUN apt-get install -y curl m4 ruby

# OCaml
WORKDIR /root
RUN curl -L https://github.com/ocaml/ocaml/archive/4.02.1.tar.gz |tar -xz
WORKDIR /root/ocaml-4.02.1
RUN ./configure
RUN make world.opt
RUN make install

# Camlp4
WORKDIR /root
RUN curl -L https://github.com/ocaml/camlp4/archive/4.02.1+1.tar.gz |tar -xz
WORKDIR /root/camlp4-4.02.1-1
RUN ./configure
RUN make all
RUN make install

# OPAM
WORKDIR /root
RUN curl -L https://github.com/ocaml/opam/archive/1.2.0.tar.gz |tar -xz
WORKDIR opam-1.2.0
RUN ./configure
RUN make lib-ext
RUN make
RUN make install

# Initialize OPAM
RUN opam init
ENV OPAMJOBS 4

# Dependencies
RUN opam install -y lwt base64

# Compile
ADD . /root/proxy
WORKDIR /root/proxy
RUN eval `opam config env`; make -j && make install

# Continuous build
CMD eval `opam config env`; make
