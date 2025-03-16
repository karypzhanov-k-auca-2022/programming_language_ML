(* Problem 1*)
fun mynull [] = true
  | mynull _  = false

(* Problem 2*)
fun reverse xs = foldl (fn (x, acc) => x::acc) [] xs

(* Problem 2.2*)
fun minlist [] = raise Match
  | minlist (x::xs) = foldl Int.min x xs

(* Test functions *)
val test_mynull = 
  if mynull [] = true andalso mynull [43, 99 , 1000] = false 
  then "mynull: passed" else "mynull: failed"

val test_reverse = 
  if reverse [832, 33, 0, 5] = [5, 0, 33 ,832] andalso reverse [] = [] 
  then "reverse: passed" else "reverse: failed"

val test_minlist = 
  if (minlist [823, 322, 1] = 1) handle Match => true 
  then "minlist: passed" else "minlist: failed"

val _ = print (test_mynull ^ "\n" ^ test_reverse ^ "\n" ^ test_minlist ^ "\n")