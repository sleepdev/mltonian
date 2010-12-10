(* hash-table-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * The signature of the polymorphic hash table structure.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature HASH_TABLE =
  sig

    type ('a, 'b) hash_table
	(* type of hash table mapping 'a to 'b *)

    val mkTable : (('a -> word) * (('a * 'a) -> bool)) -> (int * exn)
	  -> ('a,'b) hash_table
	(* Given a hashing function and an equality predicate, create a new table;
	 * the int is a size hint and the exception is to be raised by find.
	 *)

    val new : unit -> ('a,'b) hash_table

    val clear : ('a, 'b) hash_table -> unit
	(* remove all elements from the table *)

    val insert : ('a, 'b) hash_table -> ('a * 'b) -> unit
	(* Insert an item.  If the key already has an item associated with it,
	 * then the old item is discarded.
	 *)

    val inDomain : ('a, 'b) hash_table -> 'a -> bool
	(* return true, if the key is in the domain of the table *)

    val lookup : ('a, 'b) hash_table -> 'a -> 'b
	(* Find an item, the table's exception is raised if the item doesn't exist *)

    val find : ('a, 'b) hash_table -> 'a -> 'b option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove : ('a, 'b) hash_table -> 'a -> 'b
	(* Remove an item, returning the item.  The table's exception is raised if
	 * the item doesn't exist.
	 *)

    val numItems : ('a, 'b) hash_table ->  int
	(* Return the number of items in the table *)

    val listItems  : ('a, 'b) hash_table -> 'b list
    val listItemsi : ('a, 'b) hash_table -> ('a * 'b) list
	(* Return a list of the items (and their keys) in the table *)

    val app  : ('b -> unit) -> ('a, 'b) hash_table -> unit
    val appi : (('a * 'b) -> unit) -> ('a, 'b) hash_table -> unit
	(* Apply a function to the entries of the table *)

    val map  : ('b -> 'c) -> ('a, 'b) hash_table -> ('a, 'c) hash_table
    val mapi : (('a * 'b) -> 'c) -> ('a, 'b) hash_table -> ('a, 'c) hash_table
	(* Map a table to a new table that has the same keys *)

    val fold  : (('b *'c) -> 'c) -> 'c -> ('a, 'b) hash_table -> 'c
    val foldi : (('a * 'b * 'c) -> 'c) -> 'c -> ('a, 'b) hash_table -> 'c
	(* Fold a function over the elements of a table *)

    val modify  : ('b -> 'b) -> ('a, 'b) hash_table -> unit
    val modifyi : (('a * 'b) -> 'b) -> ('a, 'b) hash_table -> unit
	(* modify the hash-table items in place *)

    val filter  : ('b -> bool) -> ('a, 'b) hash_table -> unit
    val filteri : (('a * 'b) -> bool) -> ('a, 'b) hash_table -> unit
	(* remove any hash table items that do not satisfy the given
	 * predicate.
	 *)

    val copy : ('a, 'b) hash_table -> ('a, 'b) hash_table
	(* Create a copy of a hash table *)

    val bucketSizes : ('a, 'b) hash_table -> int list
	(* returns a list of the sizes of the various buckets.  This is to
	 * allow users to gauge the quality of their hashing function.
	 *)

  end (* HASH_TABLE *)




















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

structure HashTableRep : sig

    datatype ('a, 'b) bucket
      = NIL
      | B of (word * 'a * 'b * ('a, 'b) bucket)

    type ('a, 'b) table = ('a, 'b) bucket array

    val alloc : int -> ('a, 'b) table
	(* allocate a table of at least the given size *)

    val growTable : (('a, 'b) table * int) -> ('a, 'b) table
	(* grow a table to the specified size *)

    val growTableIfNeeded : (('a, 'b) table ref * int) -> bool
	(* conditionally grow a table; the second argument is the number
	 * of items currently in the table.
	 *)

    val clear : ('a, 'b) table -> unit
	(* remove all items *)

    val listItems  : (('a, 'b) table * int ref) -> 'b list
    val listItemsi : (('a, 'b) table * int ref) -> ('a * 'b) list


    val appi : ('a * 'b -> 'c) -> ('a, 'b) table -> unit
    val app : ('a -> 'b) -> ('c, 'a) table -> unit

    val mapi : ('a * 'b -> 'c) -> ('a, 'b) table -> ('a, 'c) table
    val map : ('a -> 'b) -> ('c, 'a) table -> ('c, 'b) table

    val foldi : ('a * 'b * 'c -> 'c) -> 'c -> ('a, 'b) table -> 'c
    val fold : ('a * 'b -> 'b) -> 'b -> ('c, 'a) table -> 'b

    val modify  : ('b -> 'b) -> ('a, 'b) table -> unit
    val modifyi : (('a * 'b) -> 'b) -> ('a, 'b) table -> unit

    val filteri : ('a * 'b -> bool) -> ('a, 'b) table -> int
    val filter : ('a -> bool) -> ('b,'a) table -> int

    val copy : ('a, 'b) table -> ('a, 'b) table

    val bucketSizes : ('a, 'b) table -> int list

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

  (* remove all items *)
    fun clear table = Array.modify (fn _ => NIL) table

  (* return a list of the items in the table *)
    fun listItems (table, nItems) = let
	  fun f (_, l, 0) = l
	    | f (~1, l, _) = l
	    | f (i, l, n) = let
		fun g (NIL, l, n) = f (i-1, l, n)
		  | g (B(_, k, v, r), l, n) = g(r, v::l, n-1)
		in
		  g (Array.sub(table, i), l, n)
		end
	  in
	    f ((Array.length table) - 1, [], !nItems)
	  end (* listItems *)
    fun listItemsi (table, nItems) = let
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

  (* Apply a function to the entries of the table *)
    fun appi f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f (key, item); appF rest)
	  in
	    Array.app appF table
	  end (* appi *)
    fun app f table = let
	  fun appF NIL = ()
	    | appF (B(_, key, item, rest)) = (f item; appF rest)
	  in
	    Array.app appF table
	  end (* app *)

  (* Map a table to a new table that has the same keys *)
    fun mapi f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) =
		B(hash, key, f (key, item), mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* transform *)

  (* Map a table to a new table that has the same keys *)
    fun map f table = let
	  fun mapF NIL = NIL
	    | mapF (B(hash, key, item, rest)) = B(hash, key, f item, mapF rest)
	  val newTbl = Array.tabulate (
		Array.length table,
		fn i => mapF (Array.sub(table, i)))
	  in
	    newTbl
	  end (* map *)

    fun foldi f init table = let
	  fun foldF (NIL, accum) = accum
	    | foldF (B(hash, key, item, rest), accum) =
		foldF(rest, f(key, item, accum))
	  in
	    Array.foldl foldF init table
	  end
    fun fold f init table = let
	  fun foldF (NIL, accum) = accum
	    | foldF (B(hash, key, item, rest), accum) =
		foldF(rest, f(item, accum))
	  in
	    Array.foldl foldF init table
	  end

  (* modify the hash-table items in place *)
    fun modify f table = let
	  fun modifyF NIL = NIL
	    | modifyF (B(hash, key, item, rest)) = B(hash, key, f item, modifyF rest)
	  in
	    Array.modify modifyF table
	  end
    fun modifyi f table = let
	  fun modifyF NIL = NIL
	    | modifyF (B(hash, key, item, rest)) =
		B(hash, key, f(key, item), modifyF rest)
	  in
	    Array.modify modifyF table
	  end

  (* remove any hash table items that do not satisfy the given
   * predicate.  Return the number of items left in the table.
   *)
    fun filteri pred table = let
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
    fun filter pred table = let
	  val nItems = ref 0
	  fun filterP NIL = NIL
	    | filterP (B(hash, key, item, rest)) = if (pred item)
		then (
		  nItems := !nItems+1;
		  B(hash, key, item, filterP rest))
		else filterP rest
	  in
	    Array.modify filterP table;
	    !nItems
	  end (* filter *)

  (* Create a copy of a hash table *)
    fun copy table =
	  Array.tabulate (Array.length table, fn i => Array.sub(table, i));

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes table = let
	  fun len (NIL, n) = n
	    | len (B(_, _, _, r), n) = len(r, n+1)
	  in
	    Array.foldr (fn (b, l) => len(b, 0) :: l) [] table
	  end

  end (* HashTableRep *)






















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

structure HashTable : HASH_TABLE =
  struct

    exception KeyError

    structure HTRep = HashTableRep

    datatype ('a, 'b) hash_table = HT of {
	    hash_fn : 'a -> word,
	    eq_pred : ('a * 'a) -> bool,
	    not_found : exn,
	    table : ('a, 'b) HTRep.table ref,
	    n_items : int ref
    }

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
    fun mkTable (hash, eq) (sizeHint, notFound) = HT{
	    hash_fn = hash,
	    eq_pred = eq,
	    not_found = notFound,
	    table = ref (HTRep.alloc sizeHint),
	    n_items = ref 0
	  }

    fun new () = mkTable (MLton.hash,MLton.equal) (11,KeyError)

    (* remove all elements from the table *)
    fun clear (HT{table, n_items, ...}) = (HTRep.clear(!table); n_items := 0)

    (* Insert an item.  If the key already has an item associated with it,
     * then the old item is discarded.
     *)
    fun insert (tbl as HT{hash_fn, eq_pred, table, n_items, ...}) (key, item) = let
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

  (* return true, if the key is in the domain of the table *)
    fun inDomain (HT{hash_fn, eq_pred, table, ...}) key = let
	  val arr = !table
	  val hash = hash_fn key
	  val indx = index (hash, Array.length arr)
	  fun look HTRep.NIL = false
	    | look (HTRep.B(h, k, v, r)) = 
		((hash = h) andalso eq_pred(key, k)) orelse look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* find an item, the table's exception is raised if the item doesn't exist *)
    fun lookup (HT{hash_fn, eq_pred, table, not_found, ...}) key = let
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

  (* look for an item, return NONE if the item doesn't exist *)
    fun find (HT{hash_fn, eq_pred, table, ...}) key = let
	  val arr = !table
	  val sz = Array.length arr
	  val hash = hash_fn key
	  val indx = index (hash, sz)
	  fun look HTRep.NIL = NONE
	    | look (HTRep.B(h, k, v, r)) = if ((hash = h) andalso eq_pred(key, k))
		then SOME v
		else look r
	  in
	    look (Array.sub (arr, indx))
	  end

  (* Remove an item.  The table's exception is raised if
   * the item doesn't exist.
   *)
    fun remove (HT{hash_fn, eq_pred, not_found, table, n_items}) key = let
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

  (* Return the number of items in the table *)
   fun numItems (HT{n_items, ...}) = !n_items

  (* return a list of the items in the table *)
    fun listItems (HT{table = ref arr, n_items, ...}) =
	  HTRep.listItems (arr, n_items)
    fun listItemsi (HT{table = ref arr, n_items, ...}) =
	  HTRep.listItemsi (arr, n_items)

  (* Apply a function to the entries of the table *)
    fun appi f (HT{table, ...}) = HTRep.appi f (! table)
    fun app f (HT{table, ...}) = HTRep.app f (! table)

  (* Map a table to a new table that has the same keys and exception *)
    fun mapi f (HT{hash_fn, eq_pred, table, n_items, not_found}) = HT{
	    hash_fn = hash_fn, eq_pred = eq_pred,
	    table = ref(HTRep.mapi f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* Map a table to a new table that has the same keys and exception *)
    fun map f (HT{hash_fn, eq_pred, table, n_items, not_found}) = HT{
	    hash_fn = hash_fn, eq_pred = eq_pred,
	    table = ref(HTRep.map f (! table)),
	    n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* Fold a function over the entries of the table *)
    fun foldi f init (HT{table, ...}) = HTRep.foldi f init (! table)
    fun fold f init (HT{table, ...}) = HTRep.fold f init (! table)

  (* modify the hash-table items in place *)
    fun modifyi f (HT{table, ...}) = HTRep.modifyi f (!table)
    fun modify f (HT{table, ...}) = HTRep.modify f (!table)

  (* remove any hash table items that do not satisfy the given
   * predicate.
   *)
    fun filteri pred (HT{table, n_items, ...}) =
	  n_items := HTRep.filteri pred (! table)
    fun filter pred (HT{table, n_items, ...}) = 
	  n_items := HTRep.filter pred (! table)

  (* Create a copy of a hash table *)
    fun copy (HT{hash_fn, eq_pred, table, n_items, not_found}) =HT{
	    hash_fn = hash_fn, eq_pred = eq_pred,
	    table = ref(HTRep.copy (! table)), n_items = ref(!n_items),
	    not_found = not_found
	  }

  (* returns a list of the sizes of the various buckets.  This is to
   * allow users to gauge the quality of their hashing function.
   *)
    fun bucketSizes (HT{table, ...}) = HTRep.bucketSizes(! table)

  end (* HashTable *)






