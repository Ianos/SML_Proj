datatype('a,'b)tree = empty | node of ('a,'b) tree * ('a*'b) * ('a,'b)tree;
  (* 
   fun inorder empty s = s
    |  inorder (node(l,n,r)) s = inorder l (n :: (inorder r s))
    ;
   *)
   local
     fun max (x:int) y = if x > y then x else y
    in
    fun height empty = 0
     |  height (node(l,_,r)) = 1 + max (height l) (height r)
  end;
 
  fun isfull empty = true
   |  isfull (node(l,_,r)) = height l = height r  andalso isfull l andalso isfull r
   ;
  
  fun is_complete empty = true
   |  is_complete (t as node(l,_,r)) =
          isfull t orelse 
          ((height l) = (height r) + 1 andalso isfull r andalso is_complete l) orelse
          ((height l) = (height r) andalso isfull l andalso is_complete r)
   ;
  (*
  fun max n empty = true
   |  max n (node (l,m,r)) = n>=m andalso (is_prioritized l) andalso (is_prioritized r)
  and
      is_prioritized (empty) = true
   |  is_prioritized (node (l,n,r)) = (max n l) andalso (max n r)
   ;
  
  fun isheap t = is_complete t andalso is_prioritized t
  *)
  
  fun CBTinsert item empty = node (empty,item,empty)
   |  CBTinsert item (t as node(l,n,r)) =
        if isfull t orelse 
            (height l = height r + 1 andalso isfull r andalso is_complete l andalso not (isfull l))
         then node (CBTinsert item l,n,r)
         else node (l,n,CBTinsert item r)
   ;
  
  fun LISTtoCBT [] t = t
   |   LISTtoCBT (x::xs) t = LISTtoCBT xs (CBTinsert x t)
   ;
  
 
  fun heapify empty = empty
   |  heapify (t as node(empty,n,r)) = t   (* assumes r=empty! *)
   |  heapify (t as node(node(l,m as(a,b:real),r),n as (c,d),empty)) =   (* assumes l,r=empty! *)
        if d<=b then t else node (node (empty,n,empty),m,empty)
   |  heapify (t as node(l as node(l1,m as(a,b),r1),n as(c,d),r as node(l2,q as(e,f),r2))) = 
       if d<=b andalso d<=f then t
        else if b<=d andalso b<=f then node (heapify (node(l1,n,r1)),m,r)
        else node (l,q,heapify(node(l2,n,r2)))
   ;
  
  fun CBTtoHeap empty = empty
   |  CBTtoHeap (node(l,n,r)) = heapify (node (CBTtoHeap l, n, CBTtoHeap r));
  
  
  

  
  fun HEAPinsert item t = CBTtoHeap (CBTinsert item t);
  
  (*
  fun HEAPmerge empty empty = empty
   |  HEAPmerge empty t = t
   |  HEAPmerge t empty = t
   |  HEAPmerge (l as node(l1,m,r1)) (r as node(l2,q,r2)) = 
        if m>=q then node (heapify (node(l1,q,r1)),m,r)
        else node (l,q,heapify(node(l2,m,r2)))
   ;
  *)
  
  (*
  fun HEAPmerge (t) s = CBTtoHeap (LISTtoCBT (inorder t []) s);

  fun HEAPsort empty = []
   |  HEAPsort (node(l,n,r)) = n :: HEAPsort (HEAPmerge l r);
    HEAPsort h;
	*)
	
	 
  fun gemise ((a,b,c),(d,e,f),dentro)=
	let
val time=(mikos+d-a)/(b-e)
	in
	if b>e then HEAPinsert dentro (f,time)
	else
	dentro
	