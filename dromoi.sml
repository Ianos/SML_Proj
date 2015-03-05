(***************************************************************************
  Project     : Programming Languages 1 - Assignment 1 - Exercise 2
  Author(s)   : Ianos Papatheodorou (ianospapatheodorou@yahoo.gr)
  Date        : April 25, 2013
  -----------
  National Technical University of Athens
  School of Electrical and Computer Engineering.
*)

(* Input parse code by Stavros Aronis, modified by Nick Korasidis. *)

datatype place = start|finish

fun parse file =
 	let
		(* A function to read an integer from specified input. *)
        fun readInt input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
    		
		(* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Diavase arithmo imeron *)
		val days = readInt inStream
		
        (* Diavase sinoliko  mikos*)
		val length = readInt inStream
		
		(* Diavase mikos sinehomenou aftiahtou dromou *)
		
		val problem = readInt inStream
		

        (* A function to read N pairs of integers from the open file. *)
	fun	readInfo i acc =
            let
                val arhi = readInt inStream
                val telos = readInt inStream
            in
			if i = days then ((arhi,i,start)::(telos,i,finish)::acc)
			else readInfo (i+1) ((arhi,i,start)::(telos,i,finish)::acc)
            end
  	in
   	    (days, length,problem,readInfo 1 [])
  	end
	
	(* Following code has been taken directly from http://www.it.uu.se/edu/course/homepage/pkd/ht12/program/sorting.sml *)
	
	
	fun split L =
    let
	val t = (length L) div 2
    in
	(List.take (L, t), List.drop (L, t))
    end
	
	fun merge ([], M) = M
  | merge (L, []) = L
  | merge (L as ((a,b,c)::xs), M as ((d,e,f)::ys)) =
    if a > d then
        (d,e,f) :: merge (L, ys)
    else
        (a,b,c) :: merge (xs, M)
		
		
	fun sort [] = []
  | sort [x] = [x]
  | sort xs =
    let
        val (ys, zs) = split xs
    in
        merge (sort ys, sort zs)
    end
	
	(* Here ends the code that was taken directly from domain mentioned above*)
	
	
	fun check_days ( _ ,_, true, _ , _ , _ , _ )=true
	  | check_days (last,_,false,nil,wanted,days,mikos)=
	         if mikos - last > wanted then true else false
	  | check_days (last,0,false,list as (a,b,c)::tail,wanted,days,mikos)=
			if b <=days andalso c = start then
			if a - last > wanted then
			check_days (last,0,true,tail,wanted,days,mikos)
			else
			check_days (last,1,false,tail,wanted,days,mikos)
			else
			check_days (last,0,false,tail,wanted,days,mikos)
	  | check_days (last,counter,false,list as (a,b,c)::tail,wanted,days,mikos)=
			if b<=days andalso c = finish then
			check_days (a,counter-1,false,tail,wanted,days,mikos)
			else if b <=days andalso c = start then
			check_days (last,counter+1,false,tail,wanted,days,mikos)
			else
			check_days (last,counter,false,tail,wanted,days,mikos)
					
					
					
	fun binsearch (1,1,list,problem,tlength) =
			if check_days(0,0,false,list,problem,1,tlength) then ~1 else 1
	  | binsearch (arhi,telos,list,problem,tlength) =
	        if check_days(0,0,false,list,problem,arhi,tlength) andalso check_days(0,0,false,list,problem,arhi+1,tlength)=false
			then arhi + 1
			else
				if arhi=telos-1 then
					if check_days(0,0,false,list,problem,arhi,tlength) andalso check_days(0,0,false,list,problem,telos,tlength)=false
					then telos
					else
					if check_days(0,0,false,list,problem,arhi,tlength)=false andalso check_days(0,0,false,list,problem,telos,tlength)=false
					then arhi
				    else ~1
				else
				let
				val mid = (arhi + telos)div 2
				in
				if check_days(0,0,false,list,problem,mid,tlength)=true then
				binsearch(mid,telos,list,problem,tlength)
				else
				binsearch(arhi,mid,list,problem,tlength)
				end
				
	fun dromoi file =
	let
	val (meres,mikos,problem,list)= parse file
	val slist=sort list
	in
	if mikos = problem then 0
	else
	binsearch (1,meres,slist,problem,mikos)
	end