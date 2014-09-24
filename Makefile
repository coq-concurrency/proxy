default:
	ocamlbuild systemProxy.native -use-ocamlfind -package base64,lwt,lwt.unix,str

clean:
	ocamlbuild -clean