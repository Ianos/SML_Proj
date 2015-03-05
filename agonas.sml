(* datatype 'a btree = Empty | Node of 'a * 'a btree * 'a btree;

val tree = Node(6,Node(2,Node(1,Empty,Empty),Node(4,Node(3,Empty,Empty),Node(5
,Node(100,Empty,Empty),Empty))),Node(7,Empty,Node(9,Node(8,Empty,Empty),Node(10,Empty,Empty))));

*)

fun printList f ls =
    print ("[" ^ String.concatWith "," (map f ls) ^ "]\n"); 

type height = int
datatype 'a avltree = Empty | Node of height * 'a * 'a avltree * 'a avltree

exception ma
fun max (Node(_,a,_,Empty))= a
  | max (Node(h,a,b,c))= max (c)
  | max (_)= raise ma
  
fun min (Node(h,a,Empty,_))= a
  | min (Node(h,a,b,c))= min (b)
  | min (_) = raise ma
  
  exception pre

fun previous (Node(_,a,b,c),i,parent)=
	if a = i then
		if b = Empty then parent
		else
		max (b)
	else	if a < i then previous ( c ,i ,a)
			else previous (b, i ,parent)
			| previous (_,_,_) =raise pre
			
			
			
fun next (Node(_,a,b,c),i,parent)=
	if a = i then
		if c = Empty then parent
		else 
		min(c)
	else	if a < i then next(c,i,parent)
			else next (b,i,a);

	exception EmptyTree;		

(*
			
 Following functions come directly from J.Ullmans Elements of ML Programming*)
			
fun deletemin(Empty) = raise EmptyTree
  | deletemin(Node(h,y,Empty,right))=(y,right)
  | deletemin(Node(h,w,left,right))=
	let 
		val (y,L) = deletemin(left)
		in
		(y,Node(h,w,L,right))
		end;
	(*		
fun delete lt Empty x = Empty
  | delete lt (Node(y,left,right)) x =
		if lt(x,y) then Node(y,(delete lt left x),right)
		else if lt(y,x) then Node(y,left,(delete lt right x))
		else
			case(left,right) of
			(Empty,r) => r |
			(l,Empty) => l |
			(l,r) =>
				let 
				val(z,r1) = deletemin (r)
				in
				Node(z,l,r1)
				end;
				*)
				
fun lookup lt Empty x =false
  | lookup lt (Node(_,y,left,right)) x =
	if lt(x,y) then lookup lt left x
	else if lt(y,x) then lookup lt right x
	else true;
	(*
	
fun insert lt Empty x =Node(x,Empty,Empty)
  | insert lt (T as Node(y,left,right)) x =
		if lt(x,y) then Node(y,(insert lt left x),right)
		else if lt(y,x) then Node(y,left,(insert lt right x))
		else T;
		*)

		
		
	(*Apo edo o kodikas lifthike apo http://www.cs.cornell.edu/courses/cs312/2007sp/lectures/lec15.html*)	
		
		


(* Rep Invariant:
 * For each node Node(h, v, l, r):
 * (1) BST invariant: v is greater than all values in l,
 *                    and less than all values in r.
 * (2) h is the height of the node.
 * (3) Each node is balanced, i.e., abs(l.h - r.h) <= 1
 *)

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
fun add (t, n) =
  case t
    of Empty => node(n, Empty, Empty)
     | Node(h, v, l, r) =>
         case Int.compare (n, v)
           of EQUAL => t
            | LESS => balance(node(v, add(l, n), r))
            | GREATER => balance(node(v, l, add(r, n)));

fun add1 (t, n as (c,d)) =
  case t
    of Empty => node(n , Empty, Empty)
     | Node(h, v as (a,b), l, r) =>
         case Real.compare (d, b)
           of EQUAL => (case Int.compare(c,a)
						of EQUAL => t
						| LESS => balance(node(v, add1(l, n), r))
						| GREATER => balance(node(v, l, add1(r, n)))
		   
							)
		   
		   
		   
		   
            | LESS => balance(node(v, add1(l, n), r))
            | GREATER => balance(node(v, l, add1(r, n)))	;		
			
fun remove(t, n: int) =
  let
    fun removeSuccessor(t) =
      case t
        of Empty => raise Fail "impossible"
         | Node(_, v, Empty, r) => (r, v)
         | Node(_, v, l, r) => let val (l', v') = removeSuccessor(l)
                               in (balance(node(v, l', r)), v') end
  in
    case t
      of Empty => raise Fail "value not in the tree"
       | Node (_, v, l, r) =>
           case Int.compare(n, v)
             of LESS => balance(node(v, remove(l, n), r))
              | GREATER => balance(node(v, l, remove(r, n)))
              | EQUAL => case (l, r)
                           of (_, Empty) => l
                            | (Empty, _) => r
                            | _ => let val (r', v') = removeSuccessor(r)
                                   in balance(node(v', l, r')) end
  end;
  
  
  fun remove1(t,x as(k:int,n: real)) =
  let
    fun removeSuccessor(t) =
      case t
        of Empty => raise Fail "impossible"
         | Node(_, v, Empty, r) => (r, v)
         | Node(_, v, l, r) => let val (l', v') = removeSuccessor(l)
                               in (balance(node(v, l', r)), v') end
  in
    case t
      of Empty => raise Fail "value not in the tree"
       | Node (_, v as(a,b), l, r) =>
           case Real.compare(n, b)
             of LESS => balance(node(v, remove1(l, x), r))
              | GREATER => balance(node(v, l, remove1(r, x)))
              | EQUAL => ( case Int.compare(k, a)
             of LESS => balance(node(v, remove1(l, x), r))
              | GREATER => balance(node(v, l, remove1(r, x)))
              | EQUAL => case (l, r)
                           of (_, Empty) => l
                            | (Empty, _) => r
                            | _ => let val (r', v') = removeSuccessor(r)
                                   in balance(node(v', l, r'))  end )
  end;
  (*Mehri edo itan kodikas tou http://www.cs.cornell.edu/courses/cs312/2007sp/lectures/lec15.html*)
  
 

 fun gemise ([(a,b,c)],(d,e,f),mikos,dentro,akomamesa)=
	let
val time=(mikos+d-a)/(b-e)
	in
	if b>e then (add1(dentro,(f,time)),add(akomamesa,c))
	else
	(dentro,add(akomamesa,c))
	end
   | gemise ((a,b,c)::(d,e,f)::tail,head,mikos,dentro,akomamesa)=
   let 
   val time=(d-a)/(b-e)
   in
   if b>e then
   gemise ((d,e,f)::tail,head,mikos,add1(dentro,(f,time)),add(akomamesa,c))
   else
   gemise ((d,e,f)::tail,head,mikos,dentro,add(akomamesa,c))
   end
   
   (*
val f =gemise([(0.0,1.75,1),(30.0,0.8,2),(60.0,0.5,3),(70.0,1.0,4),(120.0,0.1,
5),(140.0,0.9,6)],(0.0,1.75,1),150.0,Empty);
*)


fun howmanymax (nil,num,max)=num
  | howmanymax ((a,b:real,c)::tail,num,max)=
	if b>max then howmanymax(tail,1,b)
	else if b>=max andalso b<=max
	then howmanymax(tail,num+1,b)
	else
	howmanymax(tail,num,max);
	
	
	fun printTree (t:'a avltree) (p: 'a -> unit): unit = 
  let
    fun spaces (n: int): string = if n = 0 then "" else " " ^ (spaces (n - 1))
    fun helper (t: 'a avltree) (p: 'a -> unit) (n: int): unit =
      case t of
        Empty => print ((spaces n) ^ "Empty\n")
      | Node(_,k, l, r) => (helper r p (n + 2);   
                          print (spaces n); p k; print "\n";
                          helper l p (n + 2))
   in
     helper t p 0
end
	
(***************************************************************************
  Project     : Programming Languages 1 - Assignment 1 - Exercise 2
  Author(s)   : Nick Korasidis (renelvon@gmail.com)
  Date        : April 25, 2013
  -----------
  National Technical University of Athens
  School of Electrical and Computer Engineering.
*)

(* Input parse code by Stavros Aronis, modified by Nick Korasidis,extra modified by Ian Papatheodorou *)
fun parse file =
 	let
		(* A function to read an integer from specified input. *)
        fun readInt input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
		(*fun readReal input = Option.valOf (TextIO.scanStream (Real.scan StringCvt.DEC) input)*)
		fun poso (i,SOME #"\n",filestream)=i
		  | poso ( i,_,filestream)=
			poso(i^TextIO.inputN(filestream,1),TextIO.lookahead(filestream),filestream);
    		
		(* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Read an integer (number of players). *)
		val N = readInt inStream
		
        (* Read another integer (number of species). *)
		val L = real(readInt inStream)

        (* A function to read N pairs of integers from the open file. *)
		fun	readInfo i acc =
            let
                val position = real(readInt inStream)
				val _ = TextIO.input1  inStream
                val velocity = case Real.fromString(poso("",TextIO.lookahead(inStream),inStream)) of SOME x => x
				val _ = TextIO.inputLine inStream
            in
			if i = N then rev((position,velocity,i)::acc)
			else readInfo (i+1) ((position,velocity,i)::acc)
            end
  	in
   	    (N, L, readInfo 1 [])
  	end
	
	
	
	
	fun (* mundial(Empty,_,_,_,_,sol,_,_)=rev sol 
	| *) mundial(veltree,remtree,mikos,dedomena,i(*counter termatismou*),solution,arithmos)=
	if i<arithmos then
		let
	(*	val _ = print("edo eskase"^"\n") *)
		val (ari,tah) = min(veltree)
		val newvel =remove1(veltree,(ari,tah))
	(*	val _ = print (Int.toString(i)^"\n") *)
		val search = lookup op< remtree ari
		in
			if search then
			let
			val d = i + 1
			val prev = if ari = min(remtree) then max(remtree) 
			else previous(remtree,ari,~1)
			val epom = if ari = max(remtree) then min(remtree)
			else next(remtree,ari,~1)
			in
				let
				(*  val (_,newvel) = deletemin(veltree) *)
				val newrem = remove(remtree,ari)
				val (pos1,vel1,_)=Array.sub(dedomena,prev-1)
				val (pos2,vel2,thesis)=Array.sub(dedomena,epom-1)
				in
					if vel1>vel2 then
					let
					val time = if pos1 < pos2 then (pos2 - pos1)/(vel1 - vel2)
					else (mikos + pos2 - pos1)/(vel1 - vel2)
					in
						mundial(add1(newvel,(thesis,time)),newrem,mikos,dedomena,d,ari::solution,arithmos)
					end
					
					else
						mundial(newvel,newrem,mikos,dedomena,d,ari::solution,arithmos)
				end
			end
			
			else
			(*
			let
			val (_,newvel) = deletemin(veltree)
			in
			*)
			mundial(newvel,remtree,mikos,dedomena,i,solution,arithmos)
		(*	end  *)
		end
			
	else
		
	(* *)rev(solution)    (*,veltree,remtree) *) 


fun agonas file =
  let 
  val (athletes,mikos,list) = parse file
  val (s,t)=gemise(list,hd(list),mikos,Empty,Empty)
  val  tosoi=howmanymax(list,0,0.0)
  in
   mundial(s,t,mikos,Array.fromList(list),0,[],athletes-tosoi )
  end	
	
fun grafto (list) =
	let	
	val out = TextIO.openOut("ianos.txt")
	fun conList f ls =
    (String.concatWith " " (map f ls))
		in
		( TextIO.output(out,conList Int.toString list);
		 TextIO.closeOut(out) )
		 end
	
	