FROM ubuntu
MAINTAINER Guillaume Claret

RUN apt-get update
RUN apt-get install -y gcc make git

# Opam
RUN apt-get install -y opam
RUN opam init

# Dependencies
RUN apt-get install -y m4
RUN opam install -y lwt base64