
type height = int
datatype 'a avltree = Empty | Node of height * 'a * 'a avltree * 'a avltree


fun height(Empty) = 0
  | height(Node(h,_,_,_)) = h;

fun bal_factor(Empty) = 0
  | bal_factor(Node(_,_,l,r)) = (height l) - (height r);

fun node(v, l, r) =
  Node(1+Int.max(height l, height r), v, l, r);

fun rotate_left(t) =
  case t 
    of Node(_, x, a, Node(_, y, b, c)) => node(y, node(x, a, b), c)
     | _ => t;

fun rotate_right(t) =
  case t
    of Node(_, x, Node(_, y, a, b), c) => node(y, a, node(x, b, c))
     | _ => t;

(* Returns: an AVL tree containing the same values as n.
 * Requires: The children of n satisfy the AVL invariant, and
 *           their heights differ by at most 2. *)
fun balance(n as Node(h, v, l, r))  =
  case (bal_factor n, bal_factor l, bal_factor r) 
    of ( 2, ~1, _) => rotate_right(node(v, rotate_left l, r))
     | ( 2, _, _)  => rotate_right(n)
     | (~2, _, 1)  => rotate_left (node(v, l, rotate_right r))
     | (~2, _, _)  => rotate_left (n)
     | _ => n
;





fun add1 (t, n as (c,d)) =
  case t
    of Empty => node(n , Empty, Empty)
     | Node(h, v as (a,b), l, r) =>
         case String.compare (d, b)
           of EQUAL => (case String.compare(c,a)
						of EQUAL => t
						| LESS => balance(node(v, add1(l, n), r))
						| GREATER => balance(node(v, l, add1(r, n)))
		   
							)
		   
		   
		   
		   
            | LESS => balance(node(v, add1(l, n), r))
            | GREATER => balance(node(v, l, add1(r, n)))	;	
			
			
			

			
			
fun lookup Empty (x,a) =false
  | lookup (Node(_,(y,b),left,right)) (x,a) =
	case String.compare (x,y)
           of EQUAL => (case String.compare(a,b)
						of EQUAL => true
						| LESS => lookup left (x,a)
						| GREATER => lookup right (x,a)
		   
							)
		    | LESS => lookup left (x,a)
            | GREATER => lookup right (x,a)   
			
			
		
	fun insert(sta as(l1,buff,l2,seq,premove),que,already)=
			let
			val s1=case l1 of nil => "" | l => implode(l)
			val s2=case l2 of nil => "" | li => implode(li)
				in
			if lookup already (s1,s2) then (que,already)
			else
				let
				val _=Queue.enqueue(que,sta)
				val already=add1(already,(s1,s2))
				in
				(que,already)
				end
			end
	
	
	

	
	
	
	
	fun pick ((nil,NONE,l2,seq,premove),que,already)=(que,already)
	  | pick ((nil,SOME a,l2 as y::ys,seq,premove),que,already)=
		if premove = "12" then
		let
	(*02*) val(que,already)=insert((nil,NONE,a::l2,"02"::seq,"02"),que,already)
	    in 
		(que,already)
		end
		else
		let
	(*21*) val(que,already)=insert(([y],SOME a,ys,"21"::seq,"21"),que,already)
	(*02*) val(que,already)=insert((nil,NONE,a::l2,"02"::seq,"02"),que,already)
	    in
		(que,already)
		end
	  | pick ((l1 as x::xs,NONE,nil,seq,premove),que,already)=
	    if premove = "21" then
		let
	(*10*) val (que,already)=insert((xs,SOME x,nil,"10"::seq,"10"),que,already)
	    in
		(que,already)
		end
		else
		let
	(*12*) val (que,already)=insert((xs,NONE,[x],"12"::seq,"12"),que,already)
	(*10*) val (que,already)=insert((xs,SOME x,nil,"10"::seq,"10"),que,already)
	    in
		(que,already)
		end
	  | pick ((l1 as x::xs,SOME a,nil,seq,premove),que,already)=
	    if premove= "21" then
		let
	(*02*) val (que,already)=insert((l1,NONE,[a],"02"::seq,"02"),que,already)
		in
		(que,already)
		end
		else
		let
	(*02*) val (que,already)=insert((l1,NONE,[a],"12"::seq,"12"),que,already)
	(*12*) val (que,already)=insert((xs,SOME a,[x],"12"::seq,"12"),que,already)
	    in
		(que,already)
		end 
		| pick ((l1 as x::xs,NONE,l2 as y::ys,seq,premove),que,already)=
	    if premove ="21" then
		let
	(*21*) val (que,already)=insert((y::l1,NONE,ys,"21"::seq,"21"),que,already)
	    in
		(que,already)
		end
		else if premove ="12" then
		let
	(*12*)val (que,already) =insert((xs,NONE,x::l2,"12"::seq,"12"),que,already)
	(*10*)val (que,already) =insert((xs,SOME x,l2,"10"::seq,"10"),que,already)
		in
		(que,already)
		end
	(*02*)else
		let 
	(*12*)val (que,already) =insert((xs,NONE,x::l2,"12"::seq,"12"),que,already)
	(*10*)val (que,already) =insert((xs,SOME x,l2,"10"::seq,"10"),que,already)
		in
		(que,already)
		end
		| pick((l1 as x::xs,SOME a,l2 as y::ys,seq,premove),que,already)=
		if premove ="21" then
		let
	(*21*)val(que,already)=insert((y::l1,SOME a,ys,"21"::seq,"21"),que,already)
	(*02*)val(que,already)=insert((l1,NONE,a::l2,"02"::seq,"02"),que,already)
		in
		(que,already)
		end
		else if premove="12" then
		let
	(*12*)val (que,already)=insert((xs,SOME a,x::l2,"12"::seq,"12"),que,already)
	(*02*)val (que,already)=insert((l1,NONE,a::l2,"02"::seq,"02"),que,already)
		in
		(que,already)
		end
	(*10*)else
		let
	(*21*)val (que,already)=insert((y::l1,SOME a,ys,"21"::seq,"21"),que,already)
	(*12*)val (que,already)=insert((xs,SOME a,x::l2,"12"::seq,"12"),que,already)
		in
		(que,already)
		end
		
	
		
		
	fun solver (que,goal,already)=
	let
	val st as (l1,buff,l2,seq,premove)=Queue.dequeue(que)
	in
	if l2 = goal then 
	String.concatWith "-" (rev seq)
	else
	let
	val (que,already)=pick(st,que,already)
	in  
	solver(que,goal,already)
	end 
	end
	
	
	
	fun anagrams (start,goal)=
	let 
	val (x::xs)=rev(explode(start))
	val goal=rev(explode(goal))
	val que =Queue.mkQueue():(char list * char option * char list * string list * string) Queue.queue
	
	val state =(xs,SOME x,nil,["10"],"10")
	val _=Queue.enqueue(que,state)
	val already=add1(Empty,(implode(xs),""))
	
	val state =(xs,NONE,[x],["12"],"12")
	val _=Queue.enqueue(que,state)
	val already=add1(already,(implode(xs),"x"))
	
	in
	solver(que,goal,already)
	
	end