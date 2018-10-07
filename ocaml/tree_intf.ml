module type S = sig
  type ('k, 'v) t

  val empty  : ('k, 'v) t
  val add    : ('k, 'v) t -> replace:bool -> compare:('k -> 'k -> int) -> added:bool ref -> key:'k -> data:'v -> ('k, 'v) t
  val first  : ('k, 'v) t -> ('k * 'v) option
  val last   : ('k, 'v) t -> ('k * 'v) option
  val find   : ('k, 'v) t -> compare:('k -> 'k -> int) -> 'k -> 'v option
  val mem    : ('k, 'v) t -> compare:('k -> 'k -> int) -> 'k -> bool
  val remove : ('k, 'v) t -> removed:bool ref -> compare:('k -> 'k -> int) -> 'k -> ('k, 'v) t
end
