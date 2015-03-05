(***************************************************************************
  Project     : Programming Languages 1 - Assignment 1 - Exercise 2
  Author(s)   : Nick Korasidis (renelvon@gmail.com)
  Date        : April 25, 2013
  -----------
  National Technical University of Athens
  School of Electrical and Computer Engineering.
*)

(* Input parse code by Stavros Aronis, modified by Nick Korasidis. *)
fun parse file =
 	let
		(* A function to read an integer from specified input. *)
        fun readInt input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
    		
		(* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Read an integer (number of players). *)
		val n = readInt inStream
		
        (* Read another integer (number of species). *)
		val k = readInt inStream

        (* A function to read N pairs of integers from the open file. *)
		fun	readInts 0 acc = acc (* Replace with 'rev acc' for proper order. *)
		  |	readInts i acc =
            let
                val species = readInt inStream
                val height  = readInt inStream
            in
                readInts (i - 1) ((species, height) :: acc)
            end
  	in
   	    (n, k, readInts n [])
  	end

(* Dummy solver & requested interface *)
fun solve (n, k, l) = 42
fun basket fileName = solve (parse fileName)

(* Uncomment the following lines ONLY for MLton submissions.
val _ =
    let
        val res = basket (hd (CommandLine.arguments()))
    in
        print (Int.toString res ^ "\n") 
    end
*)
