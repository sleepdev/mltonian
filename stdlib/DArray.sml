

structure DArray : sig
    type 'a darray

    val new : unit -> 'a darray
    val len : 'a darray ->  int

    val set : 'a darray -> int -> 'a -> unit
    val get : 'a darray -> int -> 'a
    (* val pop : 'a darray -> int -> 'a *)
    val has : 'a darray -> 'a -> bool
    val append : 'a darray -> 'a -> unit

    (*
    val toList : ('a, 'b) dict -> ('a * 'b) list
    val fromList : ('a * 'b) list -> ('a, 'b) dict
    
    val app: 'a darray -> (int * 'a -> unit) -> unit
    val map : 'a darray -> ('a -> 'b) -> 'b darray
    val filter : 'a darray -> ('a -> bool) -> unit

    val copy : 'a darray -> 'a darray
    val slice : 'a darray -> (int * int) -> 'a darray
    *)
  end = struct

    type 'a darray = (int * ('a option) Array.array) ref
    exception IndexError

    fun accommodate (r as ref(l, a)) n =
      if Array.length a >= (n + 1)
         then ()
      else
         let 
            fun nextpower x = 
                if x >= (n + 1) then x
                else nextpower (x * 2)
            val ns = nextpower (Array.length a)
            val na = Array.tabulate(ns,
                (fn i =>
                    if i < l
                       then Array.sub(a, i)
                    else NONE))
         in
            r := (l, na)
         end
    

    fun new () = ref (0, Array.array(16, NONE))
    fun len (ref (s,_)) = s

    (* justify index if below zero *)
    fun justify r i = if i>=0 then i else (len r) + i

    fun set r n v =
        let 
          val i = justify r n
          val _ = accommodate r i
          val (l, a) = !r
        in
          Array.update(a, i, SOME v);
          if i >= l
             then r := (i + 1, a)
          else ()
        end

    fun get (r as ref (s, a)) n =
      let 
        val i = justify r n
      in
        if n < s 
          then (case Array.sub(a, i) of
                    NONE => raise IndexError
                  | SOME z => z)
        else raise IndexError
      end

    fun has (ref (used, a)) n = raise IndexError


    fun append (r as ref(n, _)) x =
      let
         val _ = accommodate r (n + 1)
         val (_, a) = !r
      in
         Array.update(a, n, SOME x);
         r := (n + 1, a)
      end
  end




