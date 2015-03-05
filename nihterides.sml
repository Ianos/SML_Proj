(* Kodikas dentrou gia eisagogi mikroteris apostasis*)

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
  
 

 fun gemise (tree,counter)=
 if counter = 0 then tree
 else
 gemise(add1(tree,(counter,Real.posInf)),counter-1)
   
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










(*Telos kodika dentrou*)


(*O parakato kodikas lifthike apo to forum : https://shmmy.ntua.gr/forum/memberlist.php?mode=viewprofile&u=9698 *)

fun parse file =
    let
    (* a function to read an integer from an input stream *)
        fun next_int input =
        Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
    (* a function to read a real that spans till the end of line *)
        fun next_char input =
        Option.valOf (TextIO.scanStream (Char.scan ) input)
    (* open input file and read the two integers in the first line *)
        val stream = TextIO.openIn file
        val N = next_int stream
        val M = next_int stream
        val K = next_int stream
    val _ = TextIO.inputLine stream
    (* a function to read the pair of integer & real in subsequent lines *)
        fun scanner 0 bacc wacc goal = (bacc,wacc,goal)
          | scanner i bacc wacc goal =
            let
                val x = real(next_int stream)
                val y = real(next_int stream)
                val _ = next_char stream
                val z = next_char stream
                val _ = TextIO.inputLine stream
            in
			(*Make something different return 2 lists consisting of bats and walls and spiders pos*)
				if (* z = #"B" *) ord(z)=66 then 
                scanner (i - 1) ((x, y) :: bacc) wacc goal
				else 
				if (* z = #"-" *) ord(z)=45 then
				scanner (i - 1)  bacc ((x,y) :: wacc) goal
				else
				scanner (i - 1) ((x, y)  :: bacc) wacc (x,y)   (*allagi*)
				
            end
    in
		let
		val  (a,b,c)=scanner K [] [] (0.0,0.0)
		val bats=rev(a)
		val walls=rev(b)
		in
        (*(N, M, K,  *)(bats,walls,c)
		end
    end

(*Mehri edo *)


fun inter((x1,y1),(x2,y2),(wx,wy))=
	let 
	val (xTR,yTR)=(wx+0.5,wy+0.5)
	val (xTL,yTL)=(xTR-1.0,yTR)
	val (xBL,yBL)=(xTR-1.0,yTR-1.0)
	val (xBR,yBR)=(xTR,yTR-1.0)
	fun lineq (x,y)=(y2-y1)*x+(x1-x2)*y+(x2*y1-x1*y2)
	in
	if ( lineq(xTR,yTR)>0.0 andalso lineq(xTL,yTL)>0.0 andalso lineq(xBL,yBL)>0.0 andalso lineq(xBR,yBR)>0.0 )
	orelse ( lineq(xTR,yTR)<0.0 andalso lineq(xTL,yTL)<0.0 andalso lineq(xBL,yBL)<0.0 andalso lineq(xBR,yBR)<0.0 )
	then false (*no intersection*)
	else 
	if (x1 > xTR andalso x2 > xTR) orelse (x1 < xBL andalso x2 < xBL) orelse (y1 > yTR andalso y2 > yTR)
	orelse (y1 < yBL andalso y2 < yBL) then false
	else true
	end
	
	
	
	fun roundup(x)=
	let 
	val SOME n=Real.fromString(Real.fmt (StringCvt.FIX(SOME 2)) x) 
	in
	if floor((n*100.0)-(real(floor x)*100.0)) mod 10 = 0 then
	(Real.fmt (StringCvt.FIX(SOME 1)) n)
	else
	Real.toString(n)
	end
	
	
	(* kaleitai os see(kom1,kom2,pinakas_toihon,Array.length(pinakas_toihon)-1) *)
	fun see(node1,node2,walls,~1)=true (*simanei oti den iparhoun walls*) 
	   |see(node1,node2,walls,0)=if inter(node1,node2,Array.sub(walls,0))
	   then false
	   else
	   true
	   | see(node1,node2,walls,counter)=
	  if inter(node1,node2,Array.sub(walls,counter))
	  then false
	  else
	  see(node1,node2,walls,counter-1)
	  
	  (*false NLOS,true LOS*)
	  
	  
	  
	 fun euclid((x1,y1),(x2,y2))=
	 Math.sqrt(Math.pow((x2-x1),2.0)+Math.pow((y2-y1),2.0))
	 
	 
	 
	 fun check (_,_,_,_,dist,newtree,~1)=(newtree,dist)
	   | check (minbat,bats,visited,walls,dist,newtree,counter)=
			if minbat <> counter andalso Array.sub(visited,counter) <> 1 then 
				if see(Array.sub(bats,minbat),Array.sub(bats,counter),walls,Array.length(walls)-1) then
					let val apo =Array.sub(dist,minbat)+euclid(Array.sub(bats,minbat),Array.sub(bats,counter))
					in 
					if apo < Array.sub(dist,counter)
						then 
							let
						(*	val _=print("edo skaei") *)
							val newtree=remove1(newtree,(counter,Array.sub(dist,counter)))
						(*	val _=print("edo skaei") *)
							val _ =Array.update(dist,counter,apo)
							val newtree=add1(newtree,(counter,apo))
							in
							check(minbat,bats,visited,walls,dist,newtree,counter-1)
							end
					else check(minbat,bats,visited,walls,dist,newtree,counter-1)
					end
				else check(minbat,bats,visited,walls,dist,newtree,counter-1)
            else check(minbat,bats,visited,walls,dist,newtree,counter-1)				
					
	
	fun tup((a,b),(c,d))= floor a= floor c andalso floor b=floor d
					
    fun olatalefta (bats,visited,distance,walls,goal,tree)=
		let 
		val (minbat,mindist)=min(tree)
		val newtree =remove1(tree,(minbat,mindist))
		val _=Array.update(visited,minbat,1)
		in
		if (* see(Array.sub(bats,minbat),goal,walls,Array.length(walls)-1) *) tup(Array.sub(bats,minbat),goal) then 
		(* Array.sub(distance,minbat) + euclid(Array.sub(bats,minbat),goal) *) Array.sub(distance,minbat)
		else
		let
		val (newtree,dist)=check(minbat,bats,visited,walls,distance,newtree,Array.length(bats)-1)
		in
		olatalefta(bats,visited,dist,walls,goal,newtree)
		end
		end
	
	
	fun bats file =
	let
	val (bats,walls,goal)=parse file
	val bats=Array.fromList(bats)
	val walls=Array.fromList(walls)
	val distance=Array.array(Array.length(bats),Real.posInf)
	val _=Array.update(distance,0,0.0)
	val visited=Array.array(Array.length(bats),0)
	in
	roundup( olatalefta( bats , visited , distance , walls , goal , gemise( add1(Empty,(0,0.0)), Array.length(bats)-1 ) ) )
	end
	
	