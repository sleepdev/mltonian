

(* hashtables *)

val ht = HashTable.new () : (string,string) HashTable.hashtable;
HashTable.set ht ("a","b");
print (HashTable.get ht "c");

(* Dynamic Arrays *)

val da = DArray.new () : (string) DArray;
DArray.append da "a";



(* regular expressions *)
