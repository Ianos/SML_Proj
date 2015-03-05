(* Problem: Create a function that returns the sum of the elements of a list *)

(* v1: The imperative way *)
fun sum l =
  if (l = []) then 0
  else let
         val h = hd l;
         val t = tl l
       in
         h + sum t
       end

(* v2: Use pattern-matching *)
fun sum2 [] = 0
  | sum2 (h :: t) = h + sum2 t

(*
  The actual function calls
  
  sum2 [1,2,3] =>
  1 + sum2 [2,3] =>
  1 + (2 + sum2 [3]) =>
  1 + (2 + (3 + sum2 [])) =>
  1 + (2 + (3 + 0)) =>
  6
*)

(* v3: Use tail recursion and an auxiliary function *)
fun mySum l = 
  let
    fun sum3 [] acc = acc
      | sum3 (h :: t) acc = sum3 t (acc + h)
  in
    sum3 l 0
  end
 
(*
  The actual function calls
  
  mySum [1,2,3] =>
  sum3 [1,2,3] 0 =>
  sum3 [2,3] 1 =>
  sum3 [3] 3 =>
  sum3 [] 6 =>
  6
*)


(* You can use pattern matching for tuples too! *)

fun sum3 [] acc = acc
  | sum3 (h :: t) acc = sum3 t (acc + h)

fun sum4 ([], acc) = acc
  | sum4 ((h :: t), acc) = sum4 (t, (acc + h))


(* Other examples *)


(* Use case of datatypes *)
datatype btree = Empty | Leaf | Node of btree * btree

fun cntleaves Empty = 0
  | cntleaves Leaf  = 1
  | cntleaves (Node (ltree, rtree)) =
     cntleaves ltree + cntleaves rtree

(* val powerset = fn : int list -> (int list) list *)
fun powerset [] = [ [] ]
  | powerset (h :: t) =
      let
        fun cons h t = h :: t
        fun kollato h [] = []
          | kollato h (h2::t2) = (h::h2) :: kollato h t2
        val pst = powerset t
      in
        (kollato h pst) @ pst
      end
















