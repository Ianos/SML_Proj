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

(* Dummy solver & requested interface *)


(* Uncomment the following lines ONLY for MLton submissions.
val _ =
    let
        val res = basket (hd (CommandLine.arguments()))
    in
        print (Int.toString res ^ "\n") 
    end
*)

(*fun smaller ([(a,b:real,c:int)],(d,e,f),t,t_p) =
let
val t_min = ( d - a + 150.0 ) / ( e - b )
(*val _=print "ola kala"*)
in
  if (t < 0.0 ) then
  (t_min,f)
  else
	if (t_min > 0.0) andalso ( t_min < t) then
	( t_min, f) else (t,t_p)
	end
	 | smaller ( (a,b,c)::(d,e,f)::tail,head,t,t_p) =
  let
  val t_min = (  d  -  a )  / ( b - e )
  (*val _=print (Int.toString(d))*)
  in
 if (t < 0.0 ) then
smaller((d,e,f)::tail,head,t_min,f)
  else
	if (t_min > 0.0) andalso ( t_min < t) then
	smaller((d,e,f)::tail,head,t_min, f) else smaller((d,e,f)::tail,head,t,t_p)
	end
 *)
 
 fun smaller ([(a,b:real,c:int)],(d,e,f),t,t_p,mikos) =
let
val t_min = ( d - a + mikos ) / ( b - e ) (* an a > d *)
val t_min1 = ( d - a ) / ( b - e ) (* an a < d *)
(*val _=print "ola kala"*)
in
	if b > e then (*B*)
	
	if a > d then (* A *)
	
	if t_min < t then 
   (t_min,f)
   else
   (t,t_p)
   
   else (*A*)
   
   if t_min1 < t then 
   (t_min1,f)
   else
   (t,t_p)
   
	else (*B*)
	(t,t_p)
	end
	 | smaller ( (a,b,c)::(d,e,f)::tail,head,t,t_p,mikos) =
  let
  val t_min = ( d - a + mikos ) / ( b - e ) (* an a > d *)
  val t_min1 = ( d - a ) / ( b - e ) (* an a < d *)
  (* val _=print (Real.toString(t_min)) *)
  in
    
	if b > e then (*B*)
	
	if a > d then (* A *)
	
	if t_min < t then 
   smaller((d,e,f)::tail,head,t_min, f,mikos)
   else
   smaller((d,e,f)::tail,head,t,t_p,mikos)
   
   else (*A*)
   
   if t_min1 < t then 
   smaller((d,e,f)::tail,head,t_min1,f,mikos)
   else
   smaller((d,e,f)::tail,head,t,t_p,mikos)
   
	else (*B*)
	smaller((d,e,f)::tail,head,t,t_p,mikos)
	end
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  (*
	if b > e then
	if t_min < t then
	smaller((d,e,f)::tail,head,t_min, f)
	else
	smaller((d,e,f)::tail,head,t,t_p)
	else
	smaller((d,e,f)::tail,head,t,t_p)
	end
	*)
 
 
 
 
 
  (*Edo paei i update kai eimai kala*)

 fun update ([],el_dem,mikos,time,l_acc,d_acc)=(rev(l_acc),d_acc)
   | update ((a,b,c:int)::list_tail,el_dem,mikos,time,l_acc,d_acc)=
   if c=el_dem then update(list_tail,el_dem,mikos,time,l_acc,el_dem::d_acc)
   else
   let
   val new_pos=a+b*time
   val new_pos2=Real.rem(new_pos,mikos)
   in
   if new_pos > mikos then update (list_tail,el_dem,mikos,time,(new_pos2,b,c)::l_acc,d_acc)
	else
	update (list_tail,el_dem,mikos,time,(new_pos,b,c)::l_acc,d_acc)
	end
  
  
 fun solve ([a],acc,_)=rev acc
   | solve (list,acc,mikos)=
   let
   val ( time,thesi)=smaller(list,hd(list),Real.posInf,0,mikos)
   in
   if time>=Real.posInf andalso time <= Real.posInf then rev acc
   else
   let
   val (rem_list,losers) = update (list,thesi,mikos,time,[],acc)
   (*val _ = List.app (fn x =>print(Int.toString(x)^" ")) losers
   val _ =  List.app (fn (a,b,c) =>print("menoun"^Real.toString(a)^"+"^Real.toString(b)^"+"^Int.toString(c)^"!")) rem_list*)
   in
   solve (rem_list,losers,mikos)
   end
   end
   
   
  fun agonas file =
  let 
  val (athletes,mikos,list) = parse file
  in
  solve (list,[],mikos)
  end
  
  