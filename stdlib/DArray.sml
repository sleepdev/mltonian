

structure DArray : sig
    type 'a darray

    val new : unit -> 'a darray
    val set : 'a darray -> (int * 'a) -> unit
    val get : 'a mlist -> int -> 'a
    val pop : 'a list -> int -> 'a
    val contains : 'a list -> 'a -> bool
    val len : 'a list ->  int

    val toList : ('a, 'b) dict -> ('a * 'b) list
    val fromList : ('a * 'b) list -> ('a, 'b) dict

    val app : ('a, 'b) dict -> (('a * 'b) -> unit) -> unit
    val map : ('a, 'b) dict -> (('a * 'b) -> 'c) -> ('a, 'c) dict
    val filter : ('a, 'b) dict -> (('a * 'b) -> bool) -> unit

    val copy : ('a, 'b) dict -> ('a, 'b) dict
    val slice : 
  end = struct
    exception IndexError

    justify (i: int) = if i>=0 then i else 

  end
