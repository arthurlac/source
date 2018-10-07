module Queue' : sig
  type 'a t
  val create  : 'a -> 'a t
  val enqueue : 'a t -> 'a -> 'a t
  val dequeue : 'a t -> ('a t * 'a) option
  val length  : 'a t -> int
end = struct
  type 'a t = 'a list

  let create a = [a]

  let enqueue q a = a :: q

  (* This is O(n), bad *)
  let dequeue q =
    let rec aux b f = match f with
      | [] -> None
      | x :: [] -> Some ((List.rev b), x)
      | x :: xs -> aux (x :: b) xs
    in aux [] q

  let length t = List.length t
end

module Queue : sig
  type 'a t
  val create  : 'a -> 'a t
  val enqueue : 'a t -> 'a -> 'a t
  val dequeue : 'a t -> ('a t * 'a) option
  val length  : 'a t -> int
end = struct
  type 'a t = 'a list * 'a list

  let create a = ([a], [])

  let enqueue (i, o) a = (a :: i, o)

  (* Amortised O(1) (I think, TODO Check) *)
  let dequeue (i, o) =
    match o with
    | x :: xs -> Some ((i, xs), x)
    | [] ->
      match List.rev i with
      | [] -> None
      | x :: xs -> Some (([], xs), x)

  let length (i, o) = List.length i + List.length o
end
