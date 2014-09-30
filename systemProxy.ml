module State = struct
  let servers : Lwt_unix.file_descr Heap.t ref = ref Heap.empty
  let clients : Lwt_unix.file_descr Heap.t ref = ref Heap.empty
end

module Log = struct
  let write (arguments : string list) : unit Lwt.t =
    match arguments with
    | [message] ->
      let message = Base64.decode message in
      Lwt_io.write_line Lwt_io.stderr message
    | _ -> failwith "one argument was expected"
end

module File = struct
  let read (arguments : string list) : unit Lwt.t =
    match arguments with
    | [file_name] ->
      Lwt.bind (Lwt_io.open_file Lwt_io.Input (Base64.decode file_name)) (fun file ->
      Lwt.bind (Lwt_io.read file) (fun content ->
      let content = Base64.encode content in
      Lwt_io.printl ("File.Read" ^ " " ^ file_name ^ " " ^ content)))
    | _ -> failwith "one argument was expected"
end

module TCPClientSocket = struct
  let rec recv_loop (id : Heap.Id.t) (client : Lwt_unix.file_descr)
    : unit Lwt.t =
    let buffer_size = 1024 in
    let buffer = String.create buffer_size in
    Lwt.bind (Lwt_unix.recv client buffer 0 buffer_size []) (fun bytes ->
    let message = Base64.encode (String.sub buffer 0 bytes) in
    Lwt.join [
      Lwt_io.printl ("TCPClientSocket.Read" ^ " " ^ Heap.Id.to_string id ^ " " ^
        message);
      recv_loop id client ])

  let new_client (client : Lwt_unix.file_descr) : unit Lwt.t =
    let (id, clients) = Heap.add !State.clients client in
    State.clients := clients;
    Lwt.join [
      Lwt_io.printl ("TCPClientSocket.Accepted" ^ " " ^ Heap.Id.to_string id);
      recv_loop id client ]

  let write (arguments : string list) : unit Lwt.t =
    match arguments with
    | [id; message] ->
      (match Heap.find !State.clients (Heap.Id.of_string id) with
      | None -> Lwt.return ()
      | Some client ->
        let message = Base64.decode message in
        let length = String.length message in
        Lwt.bind (Lwt_unix.send client message 0 length []) (fun _ ->
        Lwt.return ()))
    | _ -> failwith "two arguments were expected"

  let close (arguments : string list) : unit Lwt.t =
    match arguments with
    | [id] ->
      let id = Heap.Id.of_string id in
      (match Heap.find !State.clients id with
      | None -> Lwt.return ()
      | Some client ->
        State.clients := Heap.remove !State.clients id;
        Lwt_unix.close client)
    | _ -> failwith "one argument was expected"
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
        let address = Unix.ADDR_INET (Unix.inet_addr_any, port) in
        Lwt_unix.bind socket address;
        Lwt_unix.listen socket 5;
        let (id, servers) = Heap.add !State.servers socket in
        State.servers := servers;
        Lwt.join [
          Lwt_io.printl ("TCPServerSocket.Bound" ^ " " ^ Heap.Id.to_string id);
          accept_loop socket ])
    | _ -> failwith "one argument was expected"

  let close (arguments : string list) : unit Lwt.t =
    match arguments with
    | [id] ->
      let id = Heap.Id.of_string id in
      (match Heap.find !State.servers id with
      | None -> Lwt.return ()
      | Some server ->
        State.servers := Heap.remove !State.servers id;
        Lwt_unix.close server)
    | _ -> failwith "one argument was expected"
end

let handle (message : string) : unit Lwt.t =
  match Str.split (Str.regexp_string " ") message with
  | [] -> failwith "message empty"
  | command :: arguments ->
    (match command with
    | "Log.Write" -> Log.write arguments
    | "File.Read" -> File.read arguments
    | "TCPClientSocket.Write" -> TCPClientSocket.write arguments
    | "TCPClientSocket.Close" -> TCPClientSocket.close arguments
    | "TCPServerSocket.Bind" -> TCPServerSocket.bind arguments
    | "TCPServerSocket.Close" -> TCPServerSocket.close arguments
    | _ -> failwith "unknown command")

let rec loop_on_inputs () : unit Lwt.t =
  Lwt.catch (fun () ->
    Lwt.bind (Lwt_io.read_line Lwt_io.stdin) (fun message ->
    Lwt.join [handle message; loop_on_inputs ()]))
    (function
      | End_of_file -> Lwt.return ()
      | e -> raise e)

let rec main () : unit Lwt.t =
  loop_on_inputs ()

;;Lwt_main.run (main ())