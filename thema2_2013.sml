  
   
 fun geti (list,0,acc)=(list,rev acc)
  | geti (x::xs,i,acc)=geti(xs,i-1,x::acc);
 
 
  
 fun check(nil,i,pref)=true
   | check(list,i,pref)=
 let
 val (rem,ch)=geti(list,i,[])
 in
 if ch=pref then check(rem,i,pref)
 else false
 end;
 
fun solve (pref,list,lislen,0)=pref
   | solve (pref,list,lislen,v)=
       if lislen mod v = 0
               then
               let
               val (rem,canpref)=geti(list,v,[])
               in
               if check (rem,v,canpref) then solve (canpref,list,lislen,v-1)
 else
 solve(pref,list,lislen,v-1)
 end
 else
 solve(pref,list,lislen,v-1);
 
 fun srp nil = "den edoses eisodo ilithie"
   | srp (list) = 
		let
		val v= length(list) div 2
		in 
			implode(solve(list,list,length(list),v))
		end
		
		
		
		
		
		
		
		(*Thema ML diskolo 2012-2013 kanoniki)
fun inclSeqs list=
	let
	fun adder(nil,_,acc)=rev acc
  | adder(x::xs,previous,acc as list::tail)=
if x = previous + 1 then adder(xs,x,(x::list)::tail)
else adder(xs,x,[x]::(rev list)::tail)
	in
	adder (ListMergeSort.sort op> list,0,[[]])
	end
	
	
	
	(*2013 epanaliptiki palouki*)
	

fun add_list f n = n :: f;

(*fun keep_searching (l::s) [] = ((l::s),[])
  | keep_searching (l::s) (h::t)=
   if l=h-1 then keep_searching (h::l::s) t else ((l::s),(h::t));*)
   
 (*Diorthomeni*)
 
fun keep_searching (l::s) [] acc = (rev (l::s),acc)
  | keep_searching (l::s) (h::t) acc=
   if l=h-1 then keep_searching (h::l::s) t acc else keep_searching (l::s) t (h
:: acc);
   
   
   
   
fun search (l::ls) fl =
let
val (nl,rest)=keep_searching [l] ls []
in if null rest then add_list fl nl
else search (ListMergeSort.sort op> rest) (add_list fl nl)
end;


fun inclSeqs [] = []
  | inclSeqs (l::ls)=List.rev (search (ListMergeSort.sort op> (l::ls)) [] );
  
  
   (*H perifimi Windows 2011 Kanoniki *)
   

   fun windows k [] acc = acc 
     | windows k list acc =
	 if length(list)=k then rev (list::acc)
	 else if length(list)<k then []
	 else
	 let
	 fun geti (list,0,acc)=(list,rev acc)
  | geti (x::xs,i,acc)=geti(xs,i-1,x::acc)
  val (h::ipo)=list
  val (_,sol) =geti(list,k,[])
  in
  windows k ipo (sol::acc)
  end
  
  
  
  fun Prod (l1, l2) = let
      fun mkPair a b = (a,b)
      in
        List.foldr (fn (a,l) => (map (mkPair a) l2)@l) [] l1
      end
  
   fun mapAllpairs f list = map f (foldr (fn (a,l)=>(map (fn x =>(a,x)) list)@ l ) [] list)
   
   (*Kanoniki tou 2010*)
   
  fun dupl list= foldr (fn (a,l)=> a::a::l) [] list;
  
  
  datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree;
  
  
  fun add a Empty = Node (Empty,a,Empty)
  | add a (*ΠΟΛΥ ΠΡΟΣΟΧΗ ΤΡΑΓΙΚΟ ΛΑΘΟΣ ΝΑ ΞΕΧΑΣΩ ΠΑΡΕΝΘΕΣΕΙΣ ΓΥΡΩ ΑΠΟ ΤΟ Node *) (Node(left,x,right))= if a<x then Node(add a left,x,right)
else Node(left,x,add a right);


fun makeBST list f =
		let 
		fun add a Empty = Node (Empty,a,Empty)
  | add a (*ΠΟΛΥ ΠΡΟΣΟΧΗ ΤΡΑΓΙΚΟ ΛΑΘΟΣ ΝΑ ΞΕΧΑΣΩ ΠΑΡΕΝΘΕΣΕΙΣ ΓΥΡΩ ΑΠΟ ΤΟ Node *) (Node(left,x,right))= if f(a,x) then Node(add a left,x,right)
else Node(left,x,add a right)
		fun create [] acc = acc
		  | create (x::xs) acc =create xs (add x acc)
		 in
		 create list Empty
		end

		
		
(*Kanoniki 2012 *)

fun munge(f,g,list)=
	let 
	fun create [] _ acc = rev acc
	  | create (x::ftail) (y::gtail) acc = create ftail gtail (g(y)::f(x)::acc)
	 in
	 create list (rev list) []
	 end

exception bourda
fun check list = map (fn list => case list of nil => raise bourda | (x::xs) => x) list
handle bourda => [1,2,3,4]

 fun checkrem list = map (fn list => case list of nil => raise bourda | (x::xs)
=> xs) list

(*To perifio zipmany*)

exception bourda
fun zipmany list=
	let
    fun check list = map (fn list => case list of nil => raise bourda | (x::xs) => x) list
    fun checkrem list = map (fn list => case list of nil => raise bourda | (x::xs) => xs ) list
	fun solve list acc=
	let
	val sub = check list
	val rem =checkrem list
	in
	solve rem (sub::acc)
	end
	handle bourda => rev acc
	in
	solve list []
	end
	
(*Kanoniki 2010*)
fun my_map f list = foldr (fn(a,l)=>f(a)::l) [] list;
