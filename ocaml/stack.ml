module Stack : sig
  type 'a t

  val pop  : 'a t -> ('a * 'a t) option
  val push : 'a t -> 'a -> 'a t
end = struct
  type 'a t = 'a list

  let pop t = match t with
    | hd :: tl -> Some (hd, tl)
    | [] -> None (* Empty stack *)

  let push t e = e :: t
end

module Mutable_stack : sig
  type 'a t

  val pop  : 'a t -> 'a option
  val push : 'a t -> 'a -> unit
end = struct
  type 'a t = 'a list ref

  let pop t = match !t with
    | hd :: tl -> begin
        t := tl;
        Some hd
      end
    | [] -> None

  let push t a = t := a :: (!t)
end
