 fun ektypo n =
       let
               fun plist [x] = print(Int.toString(x)^"]"^"\n")
   | plist (x::xs) =( print(Int.toString(x)^","); plist xs)
               fun forlst n acc =
       if n< 0 then acc
 else forlst (n-1) ((n*n)::acc)
 val x=print("[")
 in
 plist(forlst n [])
 end;
 
 DFS
 
 fun leafListTR (Empty, acc) = acc
  | leafListTR (Node (x, Empty, Empty), acc) = x :: acc
  | leafListTR (Node (x, treeL, treeR), acc) =
      x:: leafListTR (treeL, leafListTR(treeR,acc));