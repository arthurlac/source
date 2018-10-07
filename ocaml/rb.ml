open Core
open Option.Monad_infix
open Option.Let_syntax

module Rb : sig 
  type ('k, 'v) t

  val empty  : ('k, 'v) t
  val add    : ('k, 'v) t -> replace:bool -> compare:('k -> 'k -> int) -> key:'k -> data:'v -> ('k, 'v) t
  val first  : ('k, 'v) t -> ('k * 'v) option
  val last   : ('k, 'v) t -> ('k * 'v) option
  val mem    : ('k, 'v) t -> compare:('k -> 'k -> int) -> 'k -> bool
  val remove : ('k, 'v) t -> removed:bool ref -> compare:('k -> 'k -> int) -> 'k -> ('k, 'v) t
end = struct
  type color = Red | Black
  type 'k t =
    | Empty
    | Node of { mutable color  : color
              ; key    : 'k
              ; mutable left   : 'k t
              ; mutable right  : 'k t
              ; mutable parent : 'k t
              }

  let empty = Empty

  let some_exn = function Some t -> t | None -> raise Not_found

  (* I HATE REPEATED CODE *)
  let rec first = function
    | Empty -> None
    | Node t -> if t.left = Empty then Some t.key else first t.left

  let rec last = function
    | Empty -> None
    | Node t -> if t.right = Empty then Some t.key else first t.right

  let of_cmp compare key k =
    let res = compare key k in
         if res >= 1  then `Gt
    else if res =  0  then `Eq
    else                   `Lt

  let mem t ~compare key =
    let rec aux = function
      | Empty -> false
      | Node t ->
        match (of_cmp compare key t.key) with
        | `Eq -> true
        | `Gt -> aux t.right
        | `Lt -> aux t.left
    in aux t

  let remove = failwith ""

  let create key parent =
    Node { color = Red; key; left = Empty; right = Empty; parent }

  let set node data = failwith ""

  let fix_up n = failwith ""

  let add t key data ~replace ~cmp = failwith "ui"

end
