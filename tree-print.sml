  fun spaces(n) =
    if n > 0 then " " ^ spaces(n-1) else
      if n <= 0 then "" else
        raise Fail "negative space!"
  fun rdiag(n): string list =
    if n = 0 then []
    else rdiag(n-1) @ [spaces(n-1) ^ "\\"]
  fun ldiag(n) =
    if n = 0 then []
    else  [spaces(n-1) ^ "/"] @ ldiag(n-1)

  fun indent(sl,n) = let val ws = spaces(n) in
    map (fn(s) => ws^s) sl
  end
  fun pad(sl, n) = map (fn(s) => s ^ spaces(n - String.size(s))) sl
  fun hsplice(h1::t1,h2::t2,w1,w2) =
    (h1^h2) :: hsplice(t1,t2,w1,w2)
    | hsplice(sl1: string list, nil,w1,w2) = pad(sl1,w1+w2)
    | hsplice(nil, sl2,w1,w2) = indent(sl2, w1)
  (* toStrings(t) is (sl,w,h,r) where "sl" is a list of h
   * strings of length w representing a drawing of "t",
   * where the root of the tree is positioned in the
   * first string at offset "r" *)
  fun toStrings(t: tree): (string list)*int*int*int =
    case t of
      Empty => ([], 0, 0, 0)
    | Node {left=L,value=V,right=R,...} => let
        val vs = node_toString(t)
        val vl = String.size(vs)
        val (sl1,w1,h1,r1) = toStrings(L)
        val (sl2,w2,h2,r2) = toStrings(R)
        val padding = case r2 + w1 - r1
          of 0 => 2
           | 1 => 1
           | 2 => 0
           | diff => if diff mod 2 = 0 then 0 else 1
        val w = Int.max(w1 + w2 + padding,vl)
        val diagsize = (r2 + w1 - r1 + padding) div 2
        val leftarc = case L of
          Empty => []
          | _ => ldiag(diagsize)
        val rightarc = case R of
          Empty => []
        | _ => rdiag(diagsize)
        val sl = pad(indent([vs],
                            r1 + diagsize - (vl div 2)), w) @
          pad(indent(hsplice(pad(leftarc, diagsize+1),
                             pad(rightarc, diagsize),
                             diagsize+1, diagsize),
                     r1), w) @
          hsplice(sl1, indent(sl2, padding), w1, w2+padding)
      in
        (sl, w, Int.max(h1,h2)+diagsize+1, diagsize+r1)
      end
  fun print(tr) =
    let val (sl,w,h,r) = toStrings(tr) in
      List.app (fn(s:string) => TextIO.print (s^"\n")) sl
    end

(* Useful routines for creating and querying trees *)

  fun iota(n: int): int list =
    let
      fun upto(m: int) =
        if m = n then [n] else m::upto(m+1)
    in
      upto(1)
    end

  fun addall(l: int list) =
    foldl (fn (x,s) => add(s,x)) Empty l

  fun height(t) =
    case t
      of Empty => 1
       | Node {left,right,...} => 1+Int.max(height(left),
                                       height(right))
