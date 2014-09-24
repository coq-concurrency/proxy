module Id = struct
  type t = Make of int

  type t' = t
  module Map = Map.Make (struct type t = t' let compare = compare end)

  let to_string (id : t) : string =
    match id with
    | Make id -> string_of_int id

  let of_string (id : string) : t =
    match int_of_string id with
    | exception Failure "int_of_string" ->
      failwith "the id should be an integer"
    | id -> Make id

  let first : t =
    Make 0

  let next (id : t) : t =
    match id with
    | Make id -> Make (id + 1)
end

type 'a t = {
  map : 'a Id.Map.t;
  next_id : Id.t }

let empty : 'a t =
  { map = Id.Map.empty; next_id = Id.first }

let add (heap : 'a t) (x : 'a) : (Id.t * 'a t) =
  let id = heap.next_id in
  (id, { map = Id.Map.add id x heap.map; next_id = Id.next id })

let remove (heap : 'a t) (id : Id.t) : 'a t =
  { heap with map = Id.Map.remove id heap.map }

let find (heap : 'a t) (id : Id.t) : 'a option =
  match Id.Map.find id heap.map with
  | x -> Some x
  | exception Not_found -> None