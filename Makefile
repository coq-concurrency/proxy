default:
	ocamlbuild systemProxy.native -use-ocamlfind -package lwt,lwt.unix,str

clean:
	ocamlbuild -clean