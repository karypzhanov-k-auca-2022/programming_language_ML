(* Problem 1*)
fun mynull [] = true
  | mynull _  = false

(* Problem 2*)

fun reverse xs = foldl (fn (x, acc) => x::acc) [] xs

(* Problem 2.2*)
fun minlist [] = raise Match
  | minlist (x::xs) = foldl Int.min x xs