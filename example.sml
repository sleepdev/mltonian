
(* SIMPLE polymorphic hashtables *)
use "stdlib/hashtable.sml";

val d = HashTable.new () : (string,string) HashTable.hash_table;
HashTable.set d ("a","b");
print (HashTable.get d "a");
