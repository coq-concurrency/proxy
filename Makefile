ifndef PREFIX
  PREFIX := /usr/local
endif

default:
	ocamlbuild coqConcurrencyProxy.native coqConcurrencyProxy.byte -use-ocamlfind -package base64,lwt,lwt.unix,num,str

install:
	mkdir -p ${PREFIX}/bin
	install coqConcurrencyProxy.native coqConcurrencyProxy.byte ${PREFIX}/bin/

clean:
	ocamlbuild -clean

helloWorld:
	cd tests && ocamlbuild helloWorld.native -use-ocamlfind -package base64,str,unix
	tests/helloWorld.native