

(* basic datastructures for sml *)


(*
structure DICT: sig eqtype ('k,'v) map end = 
struct 
    type ('k,'v) map = (('k * 'v) list) ref;
    val set     : ((('k,'v) map) * 'k * 'v) -> unit = ;
    val get     : ((('k,'v) map) * 'k) -> 'v
end


exception MapKeyError of string;
type ('k,'v) map = ;


fun map_get (m : ('k,'v) map) (key:'k) = case !m of
    [] => raise KeyError key
 |  (k,v) :: ms => if key=k then v else map_get ms key;

fun map_set (m : 'k 'v map) key value = 
    m := (key,value) :: !m;
*)
