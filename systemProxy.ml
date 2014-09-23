let main () : unit Lwt.t =
  Lwt_io.printl "Hello world!"

;;Lwt_main.run (main ())