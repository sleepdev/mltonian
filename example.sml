
(* SIMPLE polymorphic hashtables *)
use "stdlib/hashtable.sml";

val d = HashTable.new () : (string,string) HashTable.hash_table;
HashTable.insert d ("a","b");
print (HashTable.lookup d "a");
