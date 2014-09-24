module Log = struct
  let write (arguments : string list) : unit Lwt.t =
    match arguments with
    | [message] -> Lwt_io.printl message
    | _ -> failwith "wrong number of arguments"
end

module File = struct
  let read (arguments : string list) : unit Lwt.t =
    match arguments with
    | [file_name] ->
      Lwt.bind (Lwt_io.open_file Lwt_io.Input file_name) (fun file ->
      Lwt.bind (Lwt_io.read file) (fun content ->
      Lwt_io.print content))
    | _ -> failwith "wrong number of arguments"
end

let handle (message : string) : unit Lwt.t =
  match Str.split (Str.regexp_string " ") message with
  | [] -> failwith "message empty"
  | command :: arguments ->
    match command with
    | "Log.write" -> Log.write arguments
    | "File.read" -> File.read arguments
    | _ -> failwith "unknown command"

let rec main () : unit Lwt.t =
  Lwt.bind (Lwt_io.read_line Lwt_io.stdin) (fun message ->
  Lwt.join [handle message; main ()])

;;Lwt_main.run (main ())