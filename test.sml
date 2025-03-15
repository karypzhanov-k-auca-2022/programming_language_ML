(* Определения функций *)
fun mynull [] = true
  | mynull _  = false;

fun reverse xs = foldl (fn (x, acc) => x::acc) [] xs;

fun minlist [] = raise Match
  | minlist (x::xs) = foldl Int.min x xs;

(* Тесты в виде объявлений значений *)
val test_mynull_empty : bool = mynull [];
val test_mynull_nonempty : bool = mynull [1,2,3];

val test_reverse_empty : int list = reverse [];
val test_reverse_list : int list = reverse [1,2,3];

val test_minlist_multiple : int = minlist [3,1,4,2];
val test_minlist_single : int = minlist [5];

(* Тест на исключение нужно обрабатывать особым образом *)
val test_minlist_empty : string = 
  (minlist []; "No exception raised") 
  handle Match => "Exception Match raised as expected";

(* Вывод результатов *)
val _ = print "Test results:\n";
val _ = print ("mynull [] = " ^ Bool.toString test_mynull_empty ^ "\n");
val _ = print ("mynull [1,2,3] = " ^ Bool.toString test_mynull_nonempty ^ "\n");
val _ = print ("reverse [] = [" ^ 
       String.concatWith ", " (List.map Int.toString test_reverse_empty) ^ "]\n");
val _ = print ("reverse [1,2,3] = [" ^ 
       String.concatWith ", " (List.map Int.toString test_reverse_list) ^ "]\n");
val _ = print ("minlist [3,1,4,2] = " ^ Int.toString test_minlist_multiple ^ "\n");
val _ = print ("minlist [5] = " ^ Int.toString test_minlist_single ^ "\n");
val _ = print ("minlist [] test: " ^ test_minlist_empty ^ "\n");