(***************************************************************************
  Project     : Programming Languages 1 - Assignment 1 - Exercise 1
  Author(s)   : Ian Papatheodorou (ianospapatheodorou@yahoo.gr)
  Date        : May 30, 2014
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
		val L = readInt inStream

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