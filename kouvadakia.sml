
(* To olo concept epilisis ehei lifthei apo edo http://codesgeek.com/2013/10/water-jug-problem-source-code/ *)

fun nmtoi(n,m,M)=n*(M+1)+m;

fun iton(i,M)=i div (M+1);
fun itom(i,M)=i mod (M+1);

fun gcd (a,0)=a 
  | gcd (a,b)=gcd(b,a mod b);

fun moves (i,previous,sequence,answer)=if Array.sub(previous,i) = 0 then Array.sub(sequence,i)^answer 
else moves(Array.sub(previous,i),previous,sequence,"-"^Array.sub(sequence,i)^answer)

fun test(n,m,n1,m1,N,M,q,distance,previous,sequence,seq)=
	if n1 < 0 orelse n1 > N orelse m1 < 0 orelse m1>M
	then q
	else
		let 
		val i1=nmtoi(n1,m1,M)
		in
			if Array.sub(distance,i1) <> 0 then q
			else
				let
				val i=nmtoi(n,m,M)
				val _=Array.update(distance,i1,1)
				val _=Array.update(previous,i1,i)
				val _=Array.update(sequence,i1,seq)
				val _=Queue.enqueue(q,i1)
				in
				q
				end
		end

		
fun spasimo (q,N,M,goal,distance,previous,sequence)=
				let
				val i =Queue.dequeue(q)
				val n =iton(i,M)
				val m =itom(i,M)
					in 
						if n=goal orelse m=goal then
						moves(i,previous,sequence,"")
						else
							let
							val q=test(n,m,0,m,N,M,q,distance,previous,sequence,"10")
							val _=test(n,m,n,0,N,M,q,distance,previous,sequence,"20")
							val _=test(n,m,N,m,N,M,q,distance,previous,sequence,"01")
							val _=test(n,m,n,M,N,M,q,distance,previous,sequence,"02")
							val _=test(n,m,0,n+m,N,M,q,distance,previous,sequence,"12")
							val _=test(n,m,n+m,0,N,M,q,distance,previous,sequence,"21")
							val _=test(n,m,n-M+m,M,N,M,q,distance,previous,sequence,"12")
							val	_=test(n,m,N,m-N+n,N,M,q,distance,previous,sequence,"21")
							in
							spasimo(q,N,M,goal,distance,previous,sequence)
							end
					end		

		
		

		

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
		val k=vg div d 
		val maxsize=(N+1)*(M+1)
		val distance =Array.array(maxsize,0)
		val previous =Array.array(maxsize,0)
		val sequence =Array.array(maxsize,"")
		val _=Array.update(distance,0,1)
		val q=Queue.mkQueue():int Queue.queue
		val _=Queue.enqueue(q,0)
		in
		spasimo(q,N,M,k,distance,previous,sequence)
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
	
	
	
	
	
	
	
	
	
	
	
	
	
