(** Run the tests. *)

let run (command : string) : unit =
  let code = Sys.command command in
  if code <> 0 then
    exit code

let do_test (test : string) (arguments : string list) =
  Sys.chdir "tests/";
  let native = Filename.chop_extension test ^ ".native" in
  let compile = "ocamlbuild " ^ native ^ " -use-ocamlfind -package base64,num,str,unix" in
  run compile;
  Sys.chdir "../";
  let arguments = List.fold_left (fun s1 s2 -> s1 ^ " " ^ s2) "" arguments in
  run ("tests/" ^ native ^ " " ^ arguments)

let main () =
  match Array.to_list Sys.argv with
  | _ :: test :: arguments ->
    do_test test arguments
  | _ -> prerr_endline "At least argument was expected (the name of the test)."

;;main ()