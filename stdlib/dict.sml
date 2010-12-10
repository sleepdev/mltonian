

(* hash-table-rep.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This is the internal representation of hash tables, along with some
 * utility functions.  It is used in both the polymorphic and functor
 * hash table implementations.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure DictRep : sig

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    (* internal methods *)
    val alloc : int -> ('a, 'b) table
    val growTable : (('a, 'b) table * int) -> ('a, 'b) table
    val growTableIfNeeded : (('a, 'b) table ref * int) -> bool

    val toList : (('a, 'b) table * int ref) -> ('a * 'b) list

    val app : ('a * 'b -> 'c) -> ('a, 'b) table -> unit
    val map : ('a * 'b -> 'c) -> ('a, 'b) table -> ('a, 'c) table
    val filter : ('a * 'b -> bool) -> ('a, 'b) table -> int

    val copy : ('a, 'b) table -> ('a, 'b) table

  end = struct

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))

    (* find smallest power of 2 (>= 32) that is >= n *)
    fun roundUp n = let
	  fun f i = if (i >= n) then i else f(i * 2)
	  in
	    f 32
	  end

    (* Create a new table; the int is a size hint and the exception
     * is to be raised by find.
     *)
    fun alloc sizeHint = Array.array(roundUp sizeHint, NIL)

    (* grow a table to the specified size *)
    fun growTable (table, newSz) = let
	  val newArr = Array.array (newSz, NIL)
	  fun copy NIL = ()
	    | copy (B(h, key, v, rest)) = let
		val indx = index (h, newSz)
		in
		  Array.update (newArr, indx,
		    B(h, key, v, Array.sub(newArr, indx)));
		  copy rest
		end
	  in
	    Array.app copy table;
	    newArr
	  end

    (* conditionally grow a table; return true if it grew. *)
    fun growTableIfNeeded (table, nItems) = let
	    val arr = !table
	    val sz = Array.length arr
	    in
	      if (nItems >= sz)
		then (table := growTable (arr, sz+sz); true)
		else false
	    end

    fun toList (table, nItems) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, (k, v)::l, n-1)
		in
		  g (Array.sub(table, i), l, n)
		end
	  in
	    f ((Array.length table) - 1, [], !nItems)
	  end (* listItems *)

    fun app f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f (key, item); appF rest)
	  in
	    Array.app appF table
	  end (* appi *)

    fun map f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) =
		B(hash, key, f (key, item), mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* transform *)

    fun filter pred table = let
	  val nItems = ref 0
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred(key, item))
		then (
		  nItems := !nItems+1;
		  B(hash, key, item, filterP rest))
		else filterP rest
	  in
	    Array.modify filterP table;
	    !nItems
	  end (* filteri *)

    fun copy table =
	  Array.tabulate (Array.length table, fn i => Array.sub(table, i));

  end (* DictRep *)






















(* hash-table.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * Polymorphic hash tables.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

structure Dict : sig
    type ('a, 'b) dict

    val new : unit -> ('a,'b) dict
    val set : ('a, 'b) dict -> ('a * 'b) -> unit
    val get : ('a, 'b) dict -> 'a -> 'b
    val pop : ('a, 'b) dict -> 'a -> 'b
    val contains : ('a, 'b) dict -> 'a -> bool
    val len : ('a, 'b) dict ->  int

    val toList : ('a, 'b) dict -> ('a * 'b) list
    val fromList : ('a * 'b) list -> ('a, 'b) dict

    val app : ('a, 'b) dict -> (('a * 'b) -> unit) -> unit
    val map : ('a, 'b) dict -> (('a * 'b) -> 'c) -> ('a, 'c) dict
    val filter : ('a, 'b) dict -> (('a * 'b) -> bool) -> unit
    val copy : ('a, 'b) dict -> ('a, 'b) dict
  end = struct

    exception KeyError

    structure HTRep = DictRep

    datatype ('a, 'b) dict = HT of {
	    hash_fn : 'a -> word,
	    eq_pred : ('a * 'a) -> bool,
	    not_found : exn,
	    table : ('a, 'b) HTRep.table ref,
	    n_items : int ref
    }

    (* internal methods *)
    fun index (i, sz) = Word.toIntX(Word.andb(i, Word.fromInt sz - 0w1))
    fun roundUp n = let
	  fun f i = if (i >= n) then i else f(i * 2)
	  in
	    f 32
	  end
    fun mkTable (hash, eq) (sizeHint, notFound) = HT{
	    hash_fn = hash,
	    eq_pred = eq,
	    not_found = notFound,
	    table = ref (HTRep.alloc sizeHint),
	    n_items = ref 0
	  }


    fun new () = mkTable (MLton.hash,MLton.equal) (11,KeyError)


    fun set (tbl as HT{hash_fn, eq_pred, table, n_items, ...}) (key, item) = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = (
		Array.update(arr, indx,
		  HTRep.B(hash, key, item, Array.sub(arr, indx)));
		n_items := !n_items + 1;
		HTRep.growTableIfNeeded (table, !n_items);
		HTRep.NIL)
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then HTRep.B(hash, key, item, r)
		else (case (look r)
		   of HTRep.NIL => HTRep.NIL
		    | rest => HTRep.B(h, k, v, rest)
		  (* end case *))
	  in
	    case (look (Array.sub (arr, indx)))
	     of HTRep.NIL => ()
	      | b => Array.update(arr, indx, b)
	  end

    fun get (HT{hash_fn, eq_pred, table, not_found, ...}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

    fun pop (HT{hash_fn, eq_pred, not_found, table, n_items}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = raise not_found
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then (v, r)
		else let val (item, r') = look r in (item, HTRep.B(h, k, v, r')) end
	  val (item, bucket) = look (Array.sub (arr, indx))
	  in
	    Array.update (arr, indx, bucket);
	    n_items := !n_items - 1;
	    item
	  end (* remove *)

    fun contains (HT{hash_fn, eq_pred, table, ...}) key = let
	  val arr = !table
	  val hash = hash_fn key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = false
	    | look (HTRep.B(h, k, v, r)) = 
		((hash = h) andalso eq_pred(key, k)) orelse look r
	  in
	    look (Array.sub (arr, indx))
	  end    

    fun len (HT{n_items, ...}) = !n_items

    fun toList (HT{table = ref arr, n_items, ...}) =
	  HTRep.toList (arr, n_items)

    fun fromList l = let
      val d = new () 
      in
        List.app (fn (k,v) => set d (k,v)) l;
        d
      end

    fun app (HT{table, ...}) f = HTRep.app f (! table)

    fun map (HT{hash_fn, eq_pred, table, n_items, not_found}) f = HT{
	    hash_fn = hash_fn, eq_pred = eq_pred,
	    table = ref(HTRep.map f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

    fun filter (HT{table, n_items, ...}) pred =
	  n_items := HTRep.filter pred (! table)

    fun copy (HT{hash_fn, eq_pred, table, n_items, not_found}) =HT{
	    hash_fn = hash_fn, eq_pred = eq_pred,
	    table = ref(HTRep.copy (! table)), n_items = ref(!n_items),
	    not_found = not_found
	  }

  end (* Dict *)




