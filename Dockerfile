FROM ubuntu
MAINTAINER Guillaume Claret

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y gcc make git

# Opam
RUN apt-get install -y opam
RUN opam init

# OCaml 4.02.0
RUN opam switch 4.02.0

# Dependencies
RUN apt-get install -y m4
RUN opam install -y lwt base64

# Setup a working environment
# RUN apt-get install -y rlwrap screen telnet htop nano inotify-tools
# RUN chmod 777 /var/run/screen

# Compile
ADD . /root/coq-concurrency-extraction
WORKDIR /root/coq-concurrency-extraction
RUN eval `opam config env`; make

# Run the web server
ADD website /root/website
CMD eval `opam config env`; ocaml test.ml httpServer.ml /root/website