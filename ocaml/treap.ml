open Core

module Treap : sig

end = struct
  type 'k t =
    | Nil
    | Node of { mutable par : 'k t
              ; mutable lef : 'k t
              ; mutable rig : 'k t
              ; pri : int
              ; key : 'k
              }

  let pri () = Random.int Int.max_value

  let create k =
    Node { pri = pri (); key = k; par = Nil; lef = Nil; rig = Nil }

  (*
  let right_rot = function
    | Node (q, ((Node (p, (a, b)), c))) ->
      Node (p, (a, (Node (q, (b, c)))))
    | _ -> assert false (* Can't right_rot this node *)
  let left_rot = function
    | Node (p, (a, Node (q, (b, c)))) ->
      Node (q, (Node (p, (a, b)), c))
    | _ -> assert false (* Can't left rot this node *)
 *)

  let right_rot = function
    | Node { lef = (Node l); } as n ->
      begin
        failwith "ui"
      end

  let left_rot n = failwith "ui"

  let rec fix_heap = function Nil -> () | Node n ->
  match n.par with
  | Nil -> () (* Root node there4 for heap must b true *)
  | Node p ->
    if p.pri < n.pri then
      if p.lef = (Node n) then
        begin
          right_rot (Node p);
          fix_heap (Node n)
        end
      else
        begin
          left_rot (Node p);
          fix_heap (Node n)
        end

  let set_child par dir new_n = begin
    match par with
    | Nil -> assert false
    | Node p -> begin
        if dir = `L then
          p.lef <- new_n
        else
          p.rig <- new_n;
        fix_heap new_n
      end
  end

  let add t k ~cmp =
    let new_n par = Node { pri = pri (); key = k; par; lef = Nil; rig = Nil } in
    let rec aux par dir = function
      | Nil -> set_child par dir (new_n par)
      | Node { pri; key; par; lef; rig } as n ->
        match cmp key k with
        | `Eq -> ()
        | `Gt -> aux n `R rig
        | `Lt -> aux n `L lef
    in aux Nil t

end
