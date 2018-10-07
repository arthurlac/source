open Core

(*module Trie : sig
    type ('e, 'v) t

    val create   : key :'a list -> data : 'b -> ('a, 'b) t
  val add      : ('a, 'b) t -> key :'a list -> data : 'b -> ('a, 'b) t
  val get      : ('a, 'b) t -> key :'a list -> 'b option
  val is_entry : ('a, 'b) t -> key :'a list -> bool

    module Str : sig
        type nonrec 'a t = (char, 'a) t

        val create   : key : string -> data : 'a -> 'a t
      val add      : 'a t -> key : string -> data : 'a -> 'a t
      val get      : 'a t -> key : string -> 'a option
      val is_entry : 'a t -> key : string -> bool
    end
end = struct
    *)

  type ('e, 'v) t = Trie of 'v option * (('e * ('e, 'v) t) list)

  let create ~key ~data =
      let rec aux = function
          | []      -> Trie (Some data, [])
          | c :: cs -> Trie (None, [(c, aux cs)])
      in aux key

  let val_extract      (Trie (v, _)) = v
  let children_extract (Trie (_, c)) = c

  let key_extract   (k, _) = k
  let child_extract (_, c) = c

  let find_child trie c =
      let cl = children_extract trie in
      let rec aux = function
          | [] -> None
      | t :: ts ->
              if c = (key_extract t)
              then Some (child_extract t)
              else aux ts
      in aux cl

  let get t ~key =
      let open Option.Monad_infix in
      let rec search chars t =
          match chars with
        | []      -> None
        | [c]     -> find_child t c >>= val_extract
        | c :: cs -> bind_search (find_child t c) cs
    (* Because find child is opt we want to short circuit sometimes *)
    and bind_search ot chars = ot >>= search chars
    in search key t

  let is_entry t ~key = get t ~key |> Option.is_some

  (* This function is find child but also returns other children seperately. *)
  let seperate_child children key =
      let rec aux fwd bwd = match fwd with
      | [] -> None
      | n :: ns ->
              if (key_extract n) = key
        then Some ((child_extract n), ns @ bwd)
        else aux ns (n :: bwd)
      in aux children []

  let add t ~key ~data =
      let rec aux t chars =
          match chars with
      | [] -> (* Set the data *)
              let Trie (_, children) = t in
              Trie (Some data, children)
      | c :: cs -> (* Descend trie via current char c *)
              let Trie (v, children) = t in
              Trie (v, (descend children c cs))
          and descend children ch cs =
              match seperate_child children ch with
      | None                 -> (ch, (create ~key:cs ~data)) :: children (* Create new child *)
      | Some (child, others) -> (ch, (aux child cs))         :: others   (* Descend child *)
              in aux t key

  module Str = struct
      type nonrec 'a t = (char, 'a) t
    let to_list = String.to_list
    let create     ~key ~data = to_list key |> (fun x -> create     ~key:x ~data)
    let add      t ~key ~data = to_list key |> (fun x -> add      t ~key:x ~data)
    let get      t ~key       = to_list key |> (fun x -> get      t ~key:x)
    let is_entry t ~key       = to_list key |> (fun x -> is_entry t ~key:x)
  end

  (*end*)
