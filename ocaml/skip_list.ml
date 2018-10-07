(* I have a hunch this could be improved with GADTs, pls revisit *)
(* This whole module most likely has terrible memory characteristics *)
open Core

module Skip_list : sig
  type ('k, 'v) t

  val create : 'k -> 'v -> ('k, 'v) t

  val find   : ('k, 'v) t -> 'k       -> cmp:('k -> 'k -> [< `Eq | `Gt | `Lt ]) -> ('k * 'v) option
  val rm     : ('k, 'v) t -> 'k       -> cmp:('k -> 'k -> [< `Eq | `Gt | `Lt ])                 -> unit
  val add    : ('k, 'v) t -> 'k -> 'v -> cmp:('k -> 'k -> [< `Eq | `Gt | `Lt ]) -> replace:bool -> unit

  (* This makes find faster, TODO expl *)
  val recalibrate : ('k, 'v) t -> ('k, 'v) t

end = struct
  type ('k, 'v) t =
    | Empty
    | Head of
        { mutable next : ('k, 'v) t
        ; mutable down : ('k, 'v) t
        ; mutable up   : ('k, 'v) t
        }
    | Node of
        { key  : 'k
        ; mutable valu : 'v
        ; mutable next : ('k, 'v) t
        ; mutable down : ('k, 'v) t
        }

  let create key valu =
    let n = Node { key ; valu ; down = Empty ; next = Empty } in
    Head { next = n ; down = Empty ; up = Empty }

  (* UGLY CODE *)(* UGLY CODE *)(* UGLY CODE *)(* UGLY CODE *)
  let rec highest = function
    | Empty -> Empty
    | Node n -> assert false (* XXX *)
    | Head n -> if n.up = Empty then (Head n) else highest n.up
  let rec lowest = function
    | Empty -> Empty
    | Head n -> if n.down = Empty then (Head n) else lowest n.down
    | Node n -> if n.down = Empty then (Node n) else lowest n.down

  let recalibrate n = highest n

  let find sl k ~cmp =
    let rec aux t = match t with
      | Empty -> None
      | Head n -> aux n.next
      | Node { key ; valu ; next ; down} ->
        match cmp key k with
        | `Eq -> Some (key, valu)
        | `Gt -> aux down
        | `Lt -> aux next
    in aux (highest sl) (* We need to ensure we search from the root *)

  let set_next node new_next =
    match node with
    | Empty -> assert false (* XXX Invar why?? *)
    | Head n -> n.next <- new_next
    | Node n -> n.next <- new_next

  (* Could go down to top and stop when level has no node? *)
  let rm sl k ~cmp =
    let rec aux lvl =
      match lvl with
      | Empty -> ()
      | Head n -> del_in_lvl (Head n) Empty; aux n.down
      | Node n -> failwith "Improper construction" (* XXX *)
    and del_in_lvl node prev =
      match node with
      | Empty -> ()
      | Head n -> del_in_lvl n.next (Head n)
      | Node n ->
        match (cmp n.key k) with
        | `Eq -> set_next prev n.next
        | `Gt -> ()
        | `Lt -> del_in_lvl n.next (Node n) (* 4wd *)
    in aux (highest sl)

  let promote = Random.bool

  (* TODO *)
  let create_node key valu ~down ~next =
    Node { key ; valu ; down ; next }

  (* XXX INvar *)
  let set_val new_val = function
    | Empty  -> assert false
    | Head _ -> assert false
    | Node n -> n.valu <- new_val

  let make_new_lvl ~down ~next = Head { up = Empty; next; down }

  (* Disgusted with how over complicated this feels. *)
  let add sl k v ~cmp ~replace =
    (* TODO EXPL *)
    let ins ~down ~prev ~next =
      let new_n = create_node k v down next in
      set_next prev new_n;
      new_n
    in
    (* Okay so, we first set up a pointer to keep track of which level
     * we are at. We begin at the lowest level *)
    let cur_lvl = ref (lowest sl) in
    (* ins_in_lvl finds the right place for our new node,
     * we then take the inserted node and pass it to maybe_promote,
     * if we do promote then node we pass to maybe_promote will be the
     * value in down.
     * *)
    let rec ins_in_lvl ~cur ~down ~prev =
      match cur with
      (* We've reached end of the level, append and maybe promote *)
      | Empty  -> maybe_promote (ins ~down ~prev ~next:Empty)
      (* Begining of the level, keep going. *)
      | Head n -> ins_in_lvl ~cur:n.next ~down ~prev:(Head n)
      (* Check value of node to see if we need to keep going or insert *)
      | Node n ->
        match (cmp n.key k) with
        | `Eq -> if replace then set_val (Node n)             (* XXX, we have to on all levels *)
        | `Gt -> maybe_promote (ins ~down ~prev ~next:n.next) (* Insert the node and maybe promote *)
        | `Lt -> ins_in_lvl ~cur:n.next ~down ~prev:(Node n)  (* Forward thru the list *)
    (* Once we have added the node in the current level we may promote it,
     * meaning we also make it present in the level above. *)
    and maybe_promote new_node_below =
      if promote () then (* Promote or return unit *)
        (* The value of cur_lvl is ALWAYS a head node, maintained by invariant create *)
        match !cur_lvl with Empty -> assert false | Node _ -> assert false | Head h ->
          begin
            (* If the level above us is empty we create a new_level, insert the node into
             * that level and then we stop. If we have more levels above us then we call
             * ins_in_lvl on that level. This may lead us back here. *)
            match h.up with
            | Node _ -> assert false (* Head nodes only ever go to head nodes or empty *)
            | Empty  -> (* If we are top level then mk new level and stop *)
              let first  = create_node k v ~down:new_node_below ~next:Empty in
              let new_hd = make_new_lvl ~down:!cur_lvl ~next:first in
              h.up <- new_hd
            | Head _ -> (* We know there is a level above us so set that to be the current level *)
              cur_lvl := h.up;
              ins_in_lvl (Head h) new_node_below Empty
          end
    in
    (* Ok so, we MUST ins into bottom level. From there we may promote to a potential new top level*)
      match sl with
      | Head _ -> ins_in_lvl !cur_lvl Empty Empty
      | Node _ -> assert false (* Node can't leak *)
      | Empty  -> assert false (* Empty can't leak *)

end
