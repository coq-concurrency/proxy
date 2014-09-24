(* module Client = struct
  type t = Lwt_io.input_channel * Lwt_io.output_channel
end *)

module State = struct
  let servers : Lwt_unix.file_descr Heap.t ref = ref Heap.empty

  let clients : Lwt_unix.file_descr Heap.t ref = ref Heap.empty
end

module Log = struct
  let write (arguments : string list) : unit Lwt.t =
    match arguments with
    | [message] -> Lwt_io.printl message
    | _ -> failwith "one argument was expected"
end

module File = struct
  let read (arguments : string list) : unit Lwt.t =
    match arguments with
    | [file_name] ->
      Lwt.bind (Lwt_io.open_file Lwt_io.Input file_name) (fun file ->
      Lwt.bind (Lwt_io.read file) (fun content ->
      let content = Base64.encode content in
      Lwt_io.printl ("File.read" ^ " " ^ content)))
    | _ -> failwith "one argument was expected"
end

module TCPClientSocket = struct
  let new_client (client : Lwt_unix.file_descr) : unit Lwt.t =
    let (id, clients) = Heap.add !State.clients client in
    State.clients := clients;
    Lwt_io.printl ("TCPClientSocket.accepted" ^ " " ^ Heap.Id.to_string id)
end

module TCPServerSocket = struct
  let rec accept_loop (server : Lwt_unix.file_descr) : unit Lwt.t =
    Lwt.bind (Lwt_unix.accept server) (fun (client, _) ->
    Lwt.join [ TCPClientSocket.new_client client; accept_loop server ])
  
  let bind (arguments : string list) : unit Lwt.t =
    match arguments with
    | [port] ->
      (match int_of_string port with
      | exception Failure "int_of_string" ->
        failwith "the port number should be an integer"
      | port ->
        let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        let address = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
        Lwt_unix.bind socket address;
        Lwt_unix.listen socket 5;
        let (id, servers) = Heap.add !State.servers socket in
        State.servers := servers;
        Lwt.join [
          Lwt_io.printl ("TCPServerSocket.bound" ^ " " ^ Heap.Id.to_string id);
          accept_loop socket])
    | _ -> failwith "one argument was expected"
end

let handle (message : string) : unit Lwt.t =
  match Str.split (Str.regexp_string " ") message with
  | [] -> failwith "message empty"
  | command :: arguments ->
    match command with
    | "Log.write" -> Log.write arguments
    | "File.read" -> File.read arguments
    | "TCPServerSocket.bind" -> TCPServerSocket.bind arguments
    | _ -> failwith "unknown command"

let rec main () : unit Lwt.t =
  let _ = Heap.empty in (* TODO: remove *)
  Lwt.bind (Lwt_io.read_line Lwt_io.stdin) (fun message ->
  Lwt.join [handle message; main ()])

;;Lwt_main.run (main ())