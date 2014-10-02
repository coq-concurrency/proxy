module State = struct
  let clients : Lwt_unix.file_descr Heap.t ref = ref Heap.empty
end

module Log = struct
  let write (id : string) (arguments : string list) : unit Lwt.t =
    match arguments with
    | [message] ->
      let message = Base64.decode message in
      Lwt.catch (fun _ ->
        Lwt.bind (Lwt_io.write_line Lwt_io.stderr message) (fun _ ->
        Lwt_io.printl ("Log " ^ id ^ " true")))
        (fun _ -> Lwt_io.printl ("Log " ^ id ^ " false"))
    | _ -> failwith "one argument was expected"
end

module File = struct
  let read (id : string) (arguments : string list) : unit Lwt.t =
    match arguments with
    | [file_name] ->
      Lwt.catch (fun _ -> 
        Lwt.bind (Lwt_io.open_file Lwt_io.Input (Base64.decode file_name)) (fun file ->
        Lwt.bind (Lwt_io.read file) (fun content ->
        let content = Base64.encode content in
        Lwt_io.printl ("FileRead " ^ id ^ " " ^ content))))
        (fun _ -> Lwt_io.printl ("FileRead " ^ id ^ " "))
    | _ -> failwith "one argument was expected"
end

module ClientSocket = struct
  let read (id : string) (arguments : string list) : unit Lwt.t =
    match arguments with
    | [client_id] ->
      Lwt.catch (fun _ ->
        (match Heap.find !State.clients (Heap.Id.of_string client_id) with
        | None -> failwith "Client socket not found."
        | Some client ->
            let buffer_size = 1024 in
            let buffer = String.create buffer_size in
            Lwt.bind (Lwt_unix.recv client buffer 0 buffer_size []) (fun bytes ->
            let message = Base64.encode (String.sub buffer 0 bytes) in
            Lwt_io.printl ("ClientSocketRead " ^ id ^ " " ^ message))))
        (fun _ -> Lwt_io.printl ("ClientSocketRead " ^ id ^ " "))
    | _ -> failwith "one argument was expected"

  let write (id : string) (arguments : string list) : unit Lwt.t =
    match arguments with
    | [client_id; message] ->
      Lwt.catch (fun _ ->
        (match Heap.find !State.clients (Heap.Id.of_string client_id) with
        | None -> failwith "Client socket not found."
        | Some client ->
          let message = Base64.decode message in
          let length = String.length message in
          Lwt.bind (Lwt_unix.send client message 0 length []) (fun _ ->
          Lwt_io.printl ("ClientSocketWrite " ^ id ^ " true"))))
        (fun _ -> Lwt_io.printl ("ClientSocketWrite " ^ id ^ " false"))
    | _ -> failwith "two arguments were expected"
end

module ServerSocket = struct
  let rec accept_loop (id : string) (server : Lwt_unix.file_descr) : unit Lwt.t =
    Lwt.bind (Lwt_unix.accept server) (fun (client, _) ->
    let (client_id, clients) = Heap.add !State.clients client in
    State.clients := clients;
    Lwt.join [
      Lwt_io.printl ("ServerSocketBind " ^ id ^ " " ^ Heap.Id.to_string client_id);
      accept_loop id server ])

  let bind (id : string) (arguments : string list) : unit Lwt.t =
    match arguments with
    | [port] ->
      (match Big_int.big_int_of_string port with
      | exception Failure "int_of_string" ->
        failwith "the port number should be an integer"
      | port ->
        Lwt.catch (fun _ ->
          (match Big_int.int_of_big_int port with
          | exception Failure "int_of_big_int" ->
            failwith "the port number is too large to fit in an int"
          | port ->
            let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
            let address = Unix.ADDR_INET (Unix.inet_addr_any, port) in
            Lwt_unix.bind socket address;
            Lwt_unix.listen socket 5;
            accept_loop id socket))
          (fun _ -> Lwt_io.printl ("ServerSocketBind " ^ id ^ " ")))
    | _ -> failwith "one argument was expected"
end

let handle (message : string) : unit Lwt.t =
  match Str.split (Str.regexp_string " ") message with
  | command :: id :: arguments ->
    (match command with
    | "Log" -> Log.write id arguments
    | "FileRead" -> File.read id arguments
    | "ServerSocketBind" -> ServerSocket.bind id arguments
    | "ClientSocketRead" -> ClientSocket.read id arguments
    | "ClientSocketWrite" -> ClientSocket.write id arguments
    | _ -> failwith "unknown command")
  | _ -> failwith "message too short"

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