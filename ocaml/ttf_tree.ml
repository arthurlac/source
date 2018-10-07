  type 'k t =
    | Nil
    | Two  of 'k             * ('k t * 'k t)
    | Thr  of ('k * 'k)      * ('k t * 'k t * 'k t)
    | Four of ('k * 'k * 'k) * ('k t * 'k t * 'k t * 'k t)

  let car   n = fst n
  let caar  n = fst n |> fst
  let caaar n = fst n |> fst |> fst

  let find t k ~cmp =

  (* If the current node is a 4-node:
   * Remove and save the middle value to get a 3-node.
   * Split the remaining 3-node up into a pair of 2-nodes (the now missing middle value is handled in the next step).
   * If this is the root node (which thus has no parent):
   * the middle value becomes the new root 2-node and the tree height increases by 1. Ascend into the root.
   * Otherwise, push the middle value up into the parent node. Ascend into the parent node.
   * Find the child whose interval contains the value to be inserted.
   * If that child is a leaf, insert the value into the child node and finish.
   * Otherwise, descend into the child and repeat from step 1
   *)

  let add t k =
    let rec aux = function
      | Nil    -> Two (k, (Nil, Nil))

      | Four ((k1, k2, k3), cn) ->
        if k = k1 then







      | Two t  ->
        match cmp (car t) k with
        | `Eq ->
        | `Gt ->
        | `Lt ->
      | Thr t  ->

  let rm t k ~cmp =
