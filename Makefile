default:
	ocamlbuild systemProxy.native -use-ocamlfind -package base64,lwt,lwt.unix,num,str

clean:
	ocamlbuild -clean

helloWorld:
	cd tests && ocamlbuild helloWorld.native -use-ocamlfind -package base64,str,unix
	tests/helloWorld.native