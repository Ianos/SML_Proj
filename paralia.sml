(*έχεις 4 περιπτώσεις

αν ένα από τα 2 έχει μέσα του C τότε απλά επιστρέφεις τον accumulator
C;

το ζητούμενο

αλλιώς αν είναι άδειο το k

κάνεις 0k

αλλιώς αν είναι γεμάτο το k' κάνεις k'0

αλλιώς κάνεις kk'

κάνεις το ίδιο και για τα k=1 και για k=2

και επιστρέφεις το μικρότερο *)

fun gcd (a,0)=a 
  | gcd (a,b)=gcd(b,a mod b);



	fun solve1 (k(*1*),kt(*2*),v1,v2,wanted,acc)=
	if k=wanted orelse kt=wanted
	then rev acc 
	else if k=0 then solve1 (v1,kt,v1,v2,wanted,"01"::acc)
	else if kt=v2 then solve1 (k,0,v1,v2,wanted,"20"::acc)
	else solve1(k-Int.min(k,v2-kt),kt+Int.min(k,v2-kt),v1,v2,wanted,"12"::acc)
	
	fun solve2 (k(*2*),kt(*1*),v1,v2,wanted,acc)=
	if k=wanted orelse kt=wanted
	then rev acc 
	else if k=0 then solve2 (v2,kt,v1,v2,wanted,"02"::acc)
	else if kt=v1 then solve2 (k,0,v1,v2,wanted,"10"::acc)
	else solve2(k-Int.min(k,v1-kt),kt+Int.min(k,v1-kt),v1,v2,wanted,"21"::acc)
	

fun kouvadakia v1 v2 vg =
	let
	val d = gcd(v1,v2)
	in
	if vg <= v1 orelse vg <=v2
	then
	
	
	
	if vg mod d <> 0
	then 
	"impossible"
	else
		let
		val n=0
		val m=0
		val N=v1 div d 
		val M=v2 div d
		val wanted=vg div d 
		val s1=solve1(N,0,N,M,wanted,["01"])
		val s2=solve2(M,0,N,M,wanted,["02"])
		in
		if length(s1)>length(s2) then String.concatWith "-" s2 else String.concatWith "-" s1
		end
	else "impossible"
	end
	
	fun grafto (list) =
	let	
	val out = TextIO.openOut("ianos.txt")
	fun conList f ls =
    (String.concatWith " " (map f ls))
		in
		( TextIO.output(out,conList str list);
		 TextIO.closeOut(out) )
		 end
	
	
	
	
	
	
	