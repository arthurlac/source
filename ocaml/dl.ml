open Core

type 'a aux =
  | Nil
  | Cons of { mutable prev : 'a aux
            ; dat : 'a
            ; mutable next : 'a aux
            }

type 'a t = 'a aux ref

let create a = ref (Cons {prev=Nil; dat=a; next=Nil})

let add t a = match !t with
  | Nil ->
    t := Cons { prev=Nil; dat=a; next=Nil }
  | Cons oldh as next -> begin
      let newh = Cons { prev=Nil; dat=a; next } in
      oldh.prev <- newh;
      t := newh
    end

let rm t a =
  let rec aux = function
    | Nil -> `Not_found
    | Cons cur ->
      if cur.dat = a then
        match cur.prev, cur.next with
        | Nil, Nil       -> t := Nil;                           `Removed
        | Cons prev, Nil -> prev.next <- Nil;                   `Removed
        | Nil, Cons next -> next.prev <- Nil; t := (Cons next); `Removed
        | Cons prev, Cons next ->
            prev.next <- cur.next;
            next.prev <- cur.prev;
            `Removed
      else aux cur.next
  in aux !t

let rec equal t1 t2 =
  match t1, t2 with
  | Nil, Nil    -> true
  | Cons _, Nil -> false
  | Nil, Cons _ -> false
  | Cons p1, Cons p2 ->
    if p1.dat <> p2.dat then false
    else equal p1.next p2.next

