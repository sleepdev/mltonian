

(* hashtables *)

val ht = HashTable.new () : (string,string) HashTable.hashtable;
HashTable.set ht ("a","b");
print (HashTable.get ht "a");

(* Dynamic Arrays *)


val da = DArray.new () : (string) DArray.darray;
DArray.append da "a";
DArray.append da "b";
DArray.append da "c";
print (DArray.get da ~1);

(* regular expressions *)
