let handle (message : string) : unit Lwt.t =
  Lwt_io.printl message

let rec main () : unit Lwt.t =
  Lwt.bind (Lwt_io.read_line Lwt_io.stdin) (fun message ->
  Lwt.join [handle message; main ()])

;;Lwt_main.run (main ())