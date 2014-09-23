default:
	ocamlbuild systemProxy.native -use-ocamlfind -package lwt,lwt.unix

clean:
	ocamlbuild -clean