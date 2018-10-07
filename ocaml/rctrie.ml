open Core

module RCTrie : sig
  type t
  val from_string : string -> t
  val add_string  : t -> string -> t
  val is_entry    : t -> string -> t
end = struct
  type node = char * node list
  type t = node list

  let t    = ('t', [])
  let at   = ('a', [t])
  let cat  = ('c', [at])
  let trie = [cat]

  let char_is_child l ch = List.exists l ~f:(fun (c, _) -> c = ch)

  (* Can make tail recursive? *)
  let from_string s =
    let rec aux l =
      match l with
      | []      -> assert false
      | [c]     -> (c, [])
      | c :: cs -> (c, [aux cs])
    in
    match String.to_list s with
    | [] -> []
    | ls -> [aux ls]

  let add_string  t s = failwith "unin"
  let is_entry    t s = failwith "unin"
end
