default:
	ocamlbuild coqConcurrencyProxy.native coqConcurrencyProxy.byte -use-ocamlfind -package base64,lwt,lwt.unix,num,str

install:
	ocamlfind install coq-concurrency-proxy META coqConcurrencyProxy.native coqConcurrencyProxy.byte

clean:
	ocamlbuild -clean

helloWorld:
	cd tests && ocamlbuild helloWorld.native -use-ocamlfind -package base64,str,unix
	tests/helloWorld.native