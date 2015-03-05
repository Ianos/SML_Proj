fun parse file =
 	let
		(* A function to read an integer from specified input. *)
           		
		(* Open input file. *)
    	val inStream = TextIO.openIn file
		val velocity = case Real.fromString(TextIO.inputN(inStream,4)) of
				SOME x => x
				val _ = TextIO.inputLine inStream
        (* Read an integer (number of players). *)
	
        (* Read another integer (number of species). *)
            in
		velocity	
            end
