(* This is a correct version, written by a student *)

fun split [] = ([], [])
  | split [x] = ([x], [])
  | split (x::xs) = 
    let
        fun help (y::ys) zs = 
            if length (y::ys) > length zs then
               help ys (y::zs)
            else
               (rev zs, y::ys);
    in
        help (x::xs) []
    end;

(* This should be a simpler version, written by nickie, only it's wrong!
 * Find the bug and fix it!  There may be many!
 *)

fun split2 xs =
  let
    val n = length xs
    fun walk i acc [] = ([], [])
      | walk i acc (y :: ys) =
          if 2*i <= n then walk (i+1) (y :: acc) ys
                      else (rev acc, y)
  in
    walk 0 [] xs
  end;

(* The machinery to test a split function *)

fun test_split f_split =
  let val testcases = [
            ("empty", [], ([], [])),
            ("one",   [1], ([1], [])),
            ("two",   [1,2], ([1], [2])),
            ("six",   [1,2,3,4,5,6], ([1,2,3], [4,5,6])),
            ("seven", [1,2,3,4,5,6,7], ([1,2,3,4], [5,6,7]))
          ]
      fun runtests [] = ()
        | runtests ((name, input, output) :: testcases) = (
            print ("Testcase " ^ name ^ ": ");
            if f_split input = output then
              print "OK\n"
            else
              print "FAILED!!!\n";
            runtests testcases
          )
  in runtests testcases
  end;
