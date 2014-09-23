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
      let lines = Lwt_io.lines_of_file file_name in
      Lwt.bind (Lwt_stream.fold (fun s1 s2 -> s1 ^ "\n" ^ s2) lines "") (fun content ->
      Lwt_io.print content)
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