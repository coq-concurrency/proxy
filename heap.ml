module Id = struct
  type t = Make of int

  type t' = t
  module Map = Map.Make (struct type t = t' let compare = compare end)

  let to_string (id : t) : string =
    match id with
    | Make id -> string_of_int id
end

type 'a t = 'a Id.Map.t

let empty : 'a t =
  Id.Map.empty

(** Find an id not present in the heap (linear time). *)
let fresh_id (heap : 'a t) : Id.t =
  let rec aux (n : int) : Id.t =
    let id = Id.Make n in
    if not (Id.Map.mem id heap) then
      id
    else
      aux (n + 1) in
  aux 0

let add (heap : 'a t) (x : 'a) : (Id.t * 'a t) =
  let id = fresh_id heap in
  (id, Id.Map.add id x heap)

let find (heap : 'a t) (id : Id.t) : 'a option =
  match Id.Map.find id heap with
  | x -> Some x
  | exception Not_found -> None