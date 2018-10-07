open Core

module Heap : sig
  type 'a t
  val from_arr : 'a array -> unit
  val size : 'a t -> int
  val insert : 'a t -> 'a -> unit
  val max : 'a t -> 'a
  val extract_max : 'a t -> 'a
  val inc : 'a t -> 'a -> unit
end = struct
  type 'a t = 'a array

  let size t = Array.length t

  let parent t i = t.(i / 2)
  let left   t i = t.(2 * i)
  let right  t i = t.((2 * i) + 1)

  let lr_ix i = (2 * i, (2 * i) + 1)

  let heapify t i =
    let s = size t in
    let largest i =
      let l, r = lr_ix i in
      let lgst = ref i in
      if l < s && t.(l) > t.(i)     then lgst := l;
      if r < s && t.(r) > t.(!lgst) then lgst := r;
      !lgst
    in
    let rec aux ix =
      let lx = largest ix in
      if lx <> ix then
        begin
          Array.swap t ix lx;
          aux lx
        end
      else
        ()
    in aux i

  let from_arr a =
    let rec aux cur =
      match cur with
      | 0 ->  heapify a 0;  ()
      | ix -> heapify a ix; aux (cur - 1)
    in aux ((Array.length a) / 2)

  let insert t v = failwith "un"

  let max t =
    if size t < 1 then None else Some (t.(0))

  let extract_max t =
    if size t < 1
    then None
    else begin
      (* rm t.(0) *)
      Some (t.(0))
    end

  let inc t i = failwith "un"

end
