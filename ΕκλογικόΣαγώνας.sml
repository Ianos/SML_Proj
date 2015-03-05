Standard ML of New Jersey v110.76 [built: Sun Jul 14 09:59:19 2013]
 fun sagonas (a,b,xa,xb:real) = (xb-xa)/(a-b);
val sagonas = fn : real * real * real * real -> real
 fun minus (a,b)=
= if a>b then b-a+150 else b-a;
val minus = fn : int * int -> int
 minus (10,140);
val it = 130 : int
 minus (140,10);
val it = 20 : int
 minus (140,0);
val it = 10 : int
 fun minus (a,b:real)=
= if a>b then b-a+150 else b-a;
stdIn:8.13-8.20 Error: operator and operand don't agree [literal]
  operator domain: real * real
  operand:         real * int
  in expression:
    b - a + 150
 fun minus (a,b)=
= ;
= ;
= ;
= ;
stdIn:9.1-11.2 Error: syntax error: deleting  SEMICOLON SEMICOLON SEMICOLON
 fun minus (a,b:real)=
 if a>b then b-a+150.0 else b-a;

 fun sagonas (a,b,xa,xb:real) = minus(xb-xa)/(a-b);
stdIn:14.32-14.50 Error: operator and operand don't agree [tycon mismatch]
  operator domain: real * real
  operand:         real
  in expression:
    minus (xb - xa)
 fun sagonas (a,b,xa,xb:real) = minus(xa,xb)/(a-b);
val sagonas = fn : real * real * real * real -> real
 sagonas(140,0,0.9,1,75);
stdIn:15.1-15.24 Error: operator and operand don't agree [tycon mismatch]
  operator domain: real * real * real * real
  operand:         int * int * real * int * int
  in expression:
    sagonas (140,0,0.9,1,75)
 sagonas(140.0,0.0,0.0.9.0,1.0,75.0);
stdIn:11.1 Error: syntax error found at DOT
 sagonas(140.0,0.0,0.0,9.0,1.0,75.0);
stdIn:1.2-13.14 Error: operator and operand don't agree [tycon mismatch]
  operator domain: real * real * real * real
  operand:         real * real * real * real * real * real
  in expression:
    sagonas (140.0,0.0,0.0,9.0,1.0,75.0)
 sagonas(140.0,0.0,9.0,1,75);
stdIn:1.2-13.6 Error: operator and operand don't agree [tycon mismatch]
  operator domain: real * real * real * real
  operand:         real * real * real * int * int
  in expression:
    sagonas (140.0,0.0,9.0,1,75)
 sagonas(140.0,0.0,9.0,1.75);
val it = 0.0517857142857 : real
 sagonas(140.0,0.0,0.9,1.75);
val it = 1.06535714286 : real
 sagonas(0.9,1.75,140.0,0.0);
val it = ~164.705882353 : real
 sagonas(1.75,0.8,0.0,30.0);
val it = 126.315789474 : real
 sagonas(0.8,0.5,30.0,60.0);
val it = 400.0 : real
 sagonas(0.5,0.1,60.0,70.0);
val it = 350.0 : real
 sagonas(0.5,1,60.0,70.0);
stdIn:21.1-21.25 Error: operator and operand don't agree [literal]
  operator domain: real * real * real * real
  operand:         real * int * real * real
  in expression:
    sagonas (0.5,1,60.0,70.0)
 sagonas(0.5,1.0,60.0,70.0);
val it = ~280.0 : real
 sagonas(1.0,0.1,70.0,120.0);
val it = 111.111111111 : real
 sagonas(0.9,0.1,120.0,140.0);
val it = 162.5 : real
 sagonas(0.1,0.9,120.0,140.0);
val it = ~162.5 : real
 fun update (d,v,t) = d + v * t;
val update = fn : int * int * int -> int
 fun update (d,v,t:real) = d + v * t;
val update = fn : real * real * real -> real
 update(0.0,1.75,126);
stdIn:27.1-27.21 Error: operator and operand don't agree [literal]
  operator domain: real * real * real
  operand:         real * real * int
  in expression:
    update (0.0,1.75,126)
 update(0.0,1.75,126).0;
= ;
stdIn:10.2 Error: syntax error found at DOT
 update(0.0,1.75,126.0);
val it = 220.5 : real
 fun update (d,v,t:real) = let val new_pos=d + v * t
in
if new_pos > 150 then new_pos mod 150
else new_pos
end;
stdIn:31.1-32.13 Error: operator and operand don't agree [literal]
  operator domain: real * real
  operand:         real * int
  in expression:
    new_pos > 150
stdIn:31.23-31.38 Error: operator and operand don't agree [literal]
  operator domain: real * real
  operand:         real * int
  in expression:
    new_pos mod 150
stdIn:31.31-31.34 Error: overloaded variable not defined at type
  symbol: mod
  type: real
 fun update (d,v,t:real) = let val new_pos=d + v * t
= in
= if new_pos > 150 then new_pos mod 150.0
= else new_pos
= end;
stdIn:35.1-36.13 Error: operator and operand don't agree [literal]
  operator domain: real * real
  operand:         real * int
  in expression:
    new_pos > 150
stdIn:35.31-35.34 Error: overloaded variable not defined at type
  symbol: mod
  type: real
 fun update (d,v,t:real) = let val new_pos=d + v * t
= in
= if new_pos > 150.0 then new_pos mod 150.0
= else new_pos
= end;
stdIn:39.33-39.36 Error: overloaded variable not defined at type
  symbol: mod
  type: real

