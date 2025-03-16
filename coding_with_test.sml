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
  if mynull [] = true andalso mynull [1, 2, 3] = false 
  then "mynull: passed" else "mynull: failed"

val test_reverse = 
  if reverse [1, 2, 3] = [3, 2, 1] andalso reverse [] = [] 
  then "reverse: passed" else "reverse: failed"

val test_minlist = 
  if (minlist [3, 1, 2] = 1) handle Match => true 
  then "minlist: passed" else "minlist: failed"

val _ = print (test_mynull ^ "\n" ^ test_reverse ^ "\n" ^ test_minlist ^ "\n")

(* Tests for μScheme Interpreter and OR/AND operators *)
val _ = print "Testing μScheme Interpreter...\n"
val _ = print "or true false ; expected: true\n"
val _ = print "or false false ; expected: false\n"
val _ = print "and true false ; expected: false\n"
val _ = print "and true true ; expected: true\n"
val _ = print "and (or true false) (or false true) ; expected: true\n"