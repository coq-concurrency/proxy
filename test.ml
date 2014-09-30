(** Run the tests. *)

let run (command : string) : unit =
  let code = Sys.command command in
  if code <> 0 then
    exit code

let do_test (test : string) =
  Sys.chdir "tests/";
  let native = Filename.chop_extension test ^ ".native" in
  let compile = "ocamlbuild " ^ native ^ " -use-ocamlfind -package base64,num,str,unix" in
  run compile;
  Sys.chdir "../";
  run ("tests/" ^ native)

let main () = 
  if Array.length Sys.argv = 2 then
    do_test Sys.argv.(1)
  else
    prerr_endline "One argument was expected (the name of the test)."

;;main ()