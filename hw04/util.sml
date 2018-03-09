fun run_test f check test =
   let
      val (test_name, test_input, test_output) = test
      val _ = print (concat ["Testing ", test_name, " ...\n"])
   in
      (if check (f test_input, test_output)
          then print (concat ["Test ", test_name, " PASSED.\n"])
          else print (concat ["Test ", test_name, " FAILED.\n"]))
      handle exn => print (concat ["Test ", test_name, " ERRORED. [", exnMessage exn, "]\n"])
   end 
fun run_tests f check tests = List.app (run_test f check) tests

fun intEq (i1: int, i2: int) = i1 = i2
fun boolEq (b1: bool, b2: bool) = b1 = b2
fun stringEq (s1: string, s2: string) = s1 = s2
fun pairEq (elem1Eq, elem2Eq) (p1, p2) =
   case (p1, p2) of
      ((x1, y1), (x2, y2)) => elem1Eq (x1, x2) andalso elem2Eq (y1, y2)
fun optionEq elemEq (opt1, opt2) =
   case (opt1, opt2) of
      (NONE, NONE) => true
    | (SOME x1, SOME x2) => elemEq (x1, x2)
    | _ => false
fun listEq elemEq (l1, l2) =
   case (l1, l2) of
      ([], []) => true
    | (h1::t1, h2::t2) => elemEq (h1, h2) andalso listEq elemEq (t1, t2)
    | _ => false
fun listSetEq elemEq (l1, l2) =
   (List.length l1) = (List.length l2)
   andalso
   List.all (fn x1 => List.exists (fn x2 => elemEq (x1, x2)) l2) l1
   andalso
   List.all (fn x2 => List.exists (fn x1 => elemEq (x1, x2)) l1) l2

fun uncurry2 f (x, y) = f x y
fun uncurry3 f (x, y, z) = f x y z

fun btreeEq elemEq (bt1, bt2) =
   case (bt1, bt2) of
      (Leaf, Leaf) => true
    | (Node (l1, x1, r1), Node (l2, x2, r2)) => btreeEq elemEq (l1, l2) andalso elemEq (x1, x2) andalso btreeEq elemEq (r1, r2)
    | _ => false
datatype 'a findRes = FR_Found of 'a | FR_NotFound of string
fun wrapFind find (name, rho) =
   (FR_Found (find (name, rho))) handle NotFound n => FR_NotFound n
fun findResEq elemEq (fr1, fr2) =
   case (fr1, fr2) of
      (FR_Found d1, FR_Found d2) => elemEq (d1, d2)
    | (FR_NotFound n1, FR_NotFound n2) => stringEq (n1, n2)
    | _ => false
val tenvEx01 = Leaf
val tenvEx02 = Node (Node (Node (Leaf, ("a", 107), Leaf), ("c", 417), Node (Leaf, ("e", ~151), Node (Leaf, ("o", ~499), Leaf))), ("s", 35), Node (Leaf, ("u", ~387), Node (Leaf, ("y", 263), Leaf)))
val tenvEx03 = Node (Node (Leaf, ("d", ~422), Node (Node (Node (Node (Leaf, ("f", ~464), Leaf), ("j", 158), Leaf), ("n", ~362), Leaf), ("t", 424), Leaf)), ("v", 354), Node (Leaf, ("z", ~224), Leaf))
val tenvEx04 = Node (Node (Node (Leaf, ("c", ~169), Leaf), ("g", 15), Node (Node (Leaf, ("i", 149), Node (Node (Leaf, ("m", ~313), Leaf), ("o", 115), Leaf)), ("q", ~159), Node (Leaf, ("s", 297), Leaf))), ("u", ~327), Node (Leaf, ("w", ~163), Leaf))
val tenvEx05 = Node (Leaf, ("b", 56), Node (Node (Leaf, ("d", 192), Leaf), ("f", ~404), Node (Node (Node (Node (Leaf, ("h", 258), Node (Leaf, ("j", ~486), Leaf)), ("l", 468), Node (Leaf, ("n", 76), Leaf)), ("p", 94), Node (Node (Leaf, ("r", 348), Leaf), ("t", ~424), Leaf)), ("v", 62), Node (Leaf, ("x", ~184), Node (Leaf, ("z", 360), Leaf)))))
val tenvEx06 = Node (Node (Node (Node (Leaf, ("a", 427), Leaf), ("c", ~257), Leaf), ("e", ~297), Node (Node (Node (Leaf, ("g", ~455), Leaf), ("i", 31), Node (Leaf, ("k", 21), Leaf)), ("m", ~401), Node (Node (Node (Leaf, ("o", ~441), Leaf), ("q", ~511), Leaf), ("s", ~113), Node (Leaf, ("u", 169), Leaf)))), ("w", 245), Node (Leaf, ("y", ~291), Leaf))
val tenvEx07 = Node (Node (Node (Leaf, ("b", 378), Leaf), ("d", ~72), Node (Leaf, ("f", 64), Node (Leaf, ("h", 82), Leaf))), ("j", 362), Node (Node (Leaf, ("l", ~58), Leaf), ("n", 340), Node (Node (Node (Node (Leaf, ("p", ~52), Leaf), ("r", 114), Leaf), ("t", 130), Leaf), ("x", ~66), Node (Leaf, ("z", ~312), Leaf))))
val tenvEx08 = Node (Node (Node (Node (Leaf, ("a", 341), Leaf), ("c", ~81), Node (Leaf, ("e", ~121), Leaf)), ("g", ~191), Node (Node (Leaf, ("i", 207), Leaf), ("k", 489), Leaf)), ("m", ~459), Node (Node (Node (Leaf, ("o", 379), Leaf), ("q", ~277), Leaf), ("s", 63), Node (Node (Leaf, ("u", 23), Leaf), ("w", ~135), Node (Leaf, ("y", 351), Leaf))))
val tenvEx09 = Node (Node (Leaf, ("b", 174), Node (Node (Leaf, ("d", ~100), Leaf), ("f", ~142), Leaf)), ("h", ~328), Node (Node (Leaf, ("j", ~192), Node (Leaf, ("l", ~88), Node (Leaf, ("n", 106), Node (Leaf, ("p", ~460), Leaf)))), ("r", ~354), Node (Node (Leaf, ("t", ~190), Node (Leaf, ("v", 90), Leaf)), ("x", 20), Leaf)))
val tenvEx10 = Node (Leaf, ("a", ~391), Node (Node (Node (Leaf, ("c", 95), Leaf), ("e", ~3), Node (Leaf, ("g", ~337), Leaf)), ("i", ~27), Node (Node (Node (Node (Leaf, ("k", ~447), Leaf), ("m", ~49), Node (Leaf, ("o", 85), Leaf)), ("q", 309), Node (Leaf, ("s", ~317), Node (Leaf, ("u", 491), Node (Leaf, ("w", ~223), Leaf)))), ("y", ~233), Leaf)))
val tenvEx11 = Node (Node (Node (Leaf, ("b", ~236), Node (Node (Node (Leaf, ("d", 396), Leaf), ("f", ~462), Leaf), ("h", ~182), Leaf)), ("l", ~146), Node (Leaf, ("n", 136), Node (Leaf, ("p", ~198), Leaf))), ("r", 376), Node (Leaf, ("t", ~104), Node (Node (Leaf, ("v", 382), Node (Leaf, ("x", ~214), Leaf)), ("z", 390), Leaf)))
val tenvEx12 = Node (Node (Leaf, ("a", 165), Leaf), ("c", ~255), Node (Node (Leaf, ("e", 143), Leaf), ("g", 277), Node (Node (Leaf, ("i", 501), Node (Node (Leaf, ("k", ~125), Leaf), ("m", 243), Leaf)), ("o", ~31), Node (Node (Node (Leaf, ("q", 425), Leaf), ("s", ~199), Leaf), ("u", 287), Node (Leaf, ("w", ~75), Node (Leaf, ("y", ~145), Leaf))))))
val tenvEx13 = Node (Leaf, ("b", ~470), Node (Node (Node (Leaf, ("d", ~394), Node (Node (Leaf, ("f", ~404), Leaf), ("h", 256), Node (Leaf, ("j", ~486), Node (Leaf, ("l", 468), Leaf)))), ("n", 76), Leaf), ("p", ~402), Node (Node (Node (Leaf, ("r", 260), Node (Leaf, ("t", 306), Node (Leaf, ("v", 120), Leaf))), ("x", ~184), Leaf), ("z", 360), Leaf)))
val tenvEx14 = Node (Node (Leaf, ("a", 311), Node (Leaf, ("c", 299), Node (Leaf, ("e", ~385), Leaf))), ("g", ~425), Node (Node (Node (Leaf, ("i", 441), Leaf), ("k", ~359), Leaf), ("m", ~107), Node (Node (Node (Node (Leaf, ("o", 495), Leaf), ("q", 455), Node (Leaf, ("s", ~141), Leaf)), ("u", ~241), Node (Leaf, ("w", 41), Leaf)), ("y", 117), Leaf)))
val tenvEx15 = Node (Node (Node (Leaf, ("b", 174), Node (Leaf, ("d", 456), Node (Node (Leaf, ("f", 122), Leaf), ("h", ~36), Leaf))), ("j", 216), Node (Node (Node (Leaf, ("l", 206), Leaf), ("n", 106), Leaf), ("p", ~314), Leaf)), ("r", 84), Node (Node (Leaf, ("t", ~308), Node (Leaf, ("v", 90), Leaf)), ("x", 138), Node (Leaf, ("z", 10), Leaf)))
val tenvEx16 = Node (Node (Node (Node (Leaf, ("a", ~127), Leaf), ("c", 329), Node (Leaf, ("e", 231), Leaf)), ("g", ~395), Node (Node (Node (Leaf, ("i", ~465), Node (Leaf, ("k", ~505), Leaf)), ("m", 69), Leaf), ("o", 291), Leaf)), ("q", 105), Node (Leaf, ("s", 181), Node (Leaf, ("u", 375), Node (Leaf, ("w", ~17), Node (Leaf, ("y", ~351), Leaf)))))


fun alistEq elemEq (l1, l2) =
   case (l1, l2) of
      (Nil, Nil) => true
    | (NonNil l1, NonNil l2) =>
         let
            fun alistNNEq (l1, l2) =
               case (l1, l2) of
                  (Sing x1, Sing x2) => elemEq (x1, x2)
                | (Append (ys1, zs1), Append (ys2, zs2)) => alistNNEq (ys1, ys2) andalso alistNNEq (zs1, zs2)
                | _ => false
         in
            alistNNEq (l1, l2)
         end
    | _ => false
val alistEx1 = Nil
val alistEx2 = NonNil (Sing 42)
val alistEx3 = NonNil (Append (Sing 23, Sing 24))
val alistEx4 = NonNil (Append (Append (Sing 17, Sing 18), Sing 19))
val alistEx5 = NonNil (Append (Sing 39, Append (Sing 38, Sing 37)))
val alistEx6 = NonNil (Append (Append (Append (Sing 27, Sing 28), Sing 29), Append (Sing 79, Append (Sing 78, Sing 77))))
val alistEx7 = NonNil (Append (Append (Sing 50, Sing 51), Append (Sing 61, Sing 60)))
val alistExs = [alistEx1, alistEx2, alistEx3, alistEx4, alistEx5, alistEx6, alistEx7]

val fmlaEx01 = F_And (F_Not (F_Not (F_Or (F_Or (F_Or (F_Not (F_And (F_And (F_Or (F_Or (F_Var "d", F_Var "h"), F_Not (F_Var "a")), F_Var "h"), F_And (F_Not (F_Var "e"), F_Var "b"))), F_Var "g"), F_And (F_Var "c", F_Var "b")), F_Or (F_And (F_Or (F_Var "h", F_Or (F_Or (F_Var "c", F_Var "a"), F_Var "d")), F_Var "h"), F_Or (F_And (F_And (F_Var "b", F_Var "c"), F_Var "f"), F_Var "a"))))), F_Var "b")
val fmlaEx02 = F_Or (F_And (F_Var "c", F_Or (F_And (F_Not (F_And (F_Var "g", F_And (F_Var "a", F_Or (F_Or (F_Or (F_Not (F_Var "a"), F_Or (F_Var "h", F_Var "b")), F_And (F_Var "d", F_Or (F_Var "a", F_Or (F_Var "c", F_Var "d")))), F_Not (F_Var "d"))))), F_Or (F_Var "a", F_Var "b")), F_Var "b")), F_Not (F_Var "b"))
val fmlaEx03 = F_And (F_Var "h", F_And (F_Or (F_Var "b", F_And (F_Or (F_Or (F_Or (F_Var "f", F_Not (F_Var "e")), F_Not (F_Not (F_And (F_Var "c", F_Not (F_Not (F_And (F_Var "f", F_Var "d"))))))), F_Var "e"), F_Not (F_Var "f"))), F_Var "b"))
val fmlaEx04 = F_Or (F_Var "a", F_Not (F_And (F_Var "f", F_Not (F_Or (F_Or (F_And (F_Or (F_Var "g", F_Var "g"), F_Var "a"), F_Or (F_Var "d", F_And (F_And (F_Not (F_Or (F_Var "g", F_And (F_Var "f", F_Not (F_Or (F_Or (F_Var "d", F_Not (F_Not (F_Var "b"))), F_And (F_Not (F_And (F_Var "b", F_And (F_Var "g", F_And (F_Not (F_Var "e"), F_Or (F_Or (F_Or (F_Var "d", F_Or (F_Var "a", F_And (F_And (F_Var "g", F_Var "f"), F_Var "b"))), F_Not (F_Var "c")), F_And (F_Or (F_And (F_And (F_Or (F_Not (F_Var "f"), F_Var "h"), F_Or (F_Var "f", F_And (F_And (F_Not (F_Or (F_And (F_Or (F_Not (F_Var "a"), F_Var "g"), F_Or (F_Not (F_And (F_And (F_And (F_Not (F_Or (F_And (F_Var "h", F_And (F_Var "b", F_Or (F_Not (F_Var "d"), F_Var "d"))), F_Var "a")), F_Not (F_Var "a")), F_Var "d"), F_Or (F_Var "a", F_Not (F_Var "d")))), F_And (F_Var "a", F_Var "f"))), F_Var "h")), F_And (F_Var "c", F_Var "d")), F_Or (F_Var "b", F_Or (F_Var "d", F_Var "f"))))), F_Not (F_Not (F_Var "b"))), F_Not (F_Var "h")), F_Var "d")))))), F_Or (F_Not (F_Var "e"), F_Not (F_Or (F_Var "f", F_Var "d"))))))))), F_Var "d"), F_Or (F_Var "g", F_And (F_Var "b", F_Var "e"))))), F_Var "f")))))
val fmlaEx05 = F_And (F_Var "a", F_And (F_Not (F_Or (F_Var "c", F_And (F_Var "f", F_Or (F_Var "e", F_And (F_And (F_And (F_Var "b", F_Not (F_And (F_Var "g", F_Var "e"))), F_Or (F_Or (F_Var "e", F_Var "f"), F_Or (F_Var "e", F_Var "f"))), F_Not (F_Var "f")))))), F_Not (F_Var "f")))
val fmlaEx06 = F_Not (F_And (F_Not (F_And (F_Var "d", F_Var "c")), F_And (F_Var "g", F_Not (F_Or (F_Var "d", F_Or (F_And (F_And (F_Var "d", F_And (F_Not (F_Not (F_Var "f")), F_Var "c")), F_Var "c"), F_Var "h"))))))
val fmlaEx07 = F_Not (F_Not (F_Not (F_Var "d")))
val fmlaEx08 = F_Or (F_Var "a", F_Or (F_Var "f", F_Not (F_Not (F_Not (F_Not (F_Or (F_Not (F_Var "f"), F_And (F_Var "c", F_Or (F_Not (F_Or (F_Var "d", F_Not (F_Not (F_Not (F_Or (F_And (F_Var "g", F_And (F_Or (F_Var "h", F_And (F_Not (F_Var "g"), F_And (F_Var "b", F_Not (F_Var "b")))), F_Var "d")), F_Var "b")))))), F_And (F_Var "b", F_Var "b"))))))))))
val fmlaEx09 = F_Or (F_Not (F_Var "f"), F_Not (F_Not (F_And (F_And (F_Not (F_Or (F_Or (F_Or (F_Or (F_Var "a", F_Not (F_Var "g")), F_Var "h"), F_Var "g"), F_And (F_Var "a", F_Var "f"))), F_Not (F_Var "g")), F_Var "a"))))
val fmlaEx10 = F_And (F_Not (F_And (F_Not (F_And (F_Var "d", F_And (F_Or (F_Var "c", F_Or (F_Not (F_And (F_Not (F_Var "a"), F_And (F_Var "a", F_Var "f"))), F_Var "b")), F_Var "a"))), F_Not (F_Or (F_And (F_Or (F_Var "c", F_And (F_Var "d", F_Not (F_Not (F_And (F_Or (F_And (F_Or (F_Or (F_Or (F_Var "a", F_And (F_Var "h", F_Var "e")), F_And (F_And (F_Var "g", F_Var "e"), F_Var "d")), F_And (F_Var "c", F_Var "c")), F_Var "a"), F_Var "h"), F_Not (F_Var "g")))))), F_Not (F_Var "f")), F_Not (F_Var "h"))))), F_Var "b")
val fmlaEx11 = F_Or (F_Or (F_Var "h", F_Var "e"), F_Or (F_Var "b", F_And (F_Var "c", F_Var "a")))
val fmlaEx12 = F_Not (F_And (F_And (F_Var "d", F_And (F_And (F_Var "e", F_Or (F_Not (F_Or (F_Var "g", F_Var "e")), F_Or (F_Var "d", F_Var "f"))), F_And (F_Var "c", F_And (F_Var "g", F_And (F_Var "c", F_Not (F_Var "d")))))), F_Var "c"))
val fmlaEx13 = F_Not (F_Not (F_Var "h"))
val fmlaEx14 = F_Or (F_Not (F_Var "d"), F_Not (F_Var "d"))
val fmlaEx15 = F_Not (F_And (F_Or (F_And (F_Not (F_Var "e"), F_Not (F_Or (F_Not (F_Var "f"), F_Or (F_Var "a", F_Or (F_Var "h", F_Var "d"))))), F_Var "g"), F_Var "g"))
val fmlaEx16 = F_Or (F_Var "h", F_Not (F_And (F_And (F_Not (F_Or (F_Var "g", F_And (F_Not (F_Var "c"), F_Not (F_Not (F_And (F_Or (F_And (F_Not (F_And (F_And (F_Var "g", F_Var "b"), F_Var "b")), F_Var "d"), F_Not (F_Or (F_And (F_Var "b", F_Or (F_And (F_Var "c", F_Var "b"), F_Not (F_And (F_Or (F_Var "c", F_Var "g"), F_Or (F_Or (F_And (F_Var "e", F_Or (F_Var "d", F_And (F_Var "b", F_Or (F_Not (F_Not (F_Not (F_Not (F_Var "b")))), F_And (F_Not (F_Not (F_Or (F_And (F_Not (F_And (F_Var "e", F_Var "b")), F_Not (F_Not (F_Var "h"))), F_Var "h"))), F_Var "g"))))), F_Var "e"), F_Or (F_And (F_Or (F_Not (F_Var "f"), F_And (F_Or (F_Var "c", F_Var "g"), F_Var "f")), F_Var "g"), F_And (F_Not (F_Var "d"), F_Var "d"))))))), F_Var "d"))), F_Var "h")))))), F_Var "f"), F_And (F_Not (F_Var "e"), F_Var "h"))))
val fmlaEx17 = F_Or (F_Not (F_Not (F_Not (F_Or (F_Or (F_And (F_Var "h", F_And (F_Not (F_Not (F_Var "d")), F_Var "b")), F_Or (F_Not (F_Var "b"), F_Var "f")), F_Or (F_Or (F_Var "h", F_Var "b"), F_And (F_Or (F_And (F_Not (F_Or (F_Var "a", F_Var "f")), F_Not (F_Or (F_Var "a", F_And (F_Var "h", F_Var "a")))), F_Var "d"), F_Not (F_Or (F_Not (F_Not (F_Or (F_And (F_And (F_And (F_Var "b", F_Or (F_And (F_Or (F_Var "c", F_Not (F_Var "f")), F_Or (F_And (F_Var "g", F_Not (F_Var "a")), F_Var "c")), F_Var "g")), F_Var "b"), F_Not (F_And (F_Var "h", F_Not (F_And (F_Var "c", F_Not (F_Not (F_And (F_Or (F_Var "d", F_Var "b"), F_Not (F_Not (F_Var "f")))))))))), F_Var "a"))), F_And (F_Not (F_Not (F_Var "h")), F_And (F_Var "b", F_Not (F_Var "d"))))))))))), F_Not (F_And (F_Var "e", F_Var "b")))
val fmlaEx18 = F_And (F_Or (F_Var "a", F_Or (F_Not (F_Or (F_And (F_Not (F_Not (F_Var "h")), F_And (F_Not (F_Var "d"), F_Or (F_And (F_Or (F_Or (F_Var "d", F_Not (F_Var "b")), F_Or (F_Not (F_Var "b"), F_And (F_And (F_Var "e", F_Var "e"), F_Or (F_Not (F_Var "d"), F_Var "c")))), F_Not (F_Or (F_And (F_Not (F_Var "d"), F_Not (F_Var "c")), F_Or (F_Var "g", F_And (F_Var "c", F_And (F_Var "f", F_Or (F_Var "a", F_Or (F_Or (F_Var "a", F_Not (F_Var "a")), F_Var "b")))))))), F_Var "e"))), F_Var "h")), F_And (F_Not (F_Var "h"), F_Var "e"))), F_Var "h")
val fmlaEx19 = F_Or (F_Not (F_Var "f"), F_Not (F_And (F_Var "g", F_Var "d")))
val fmlaEx20 = F_Or (F_Var "c", F_Or (F_Not (F_Var "a"), F_Var "a"))
val fmlaEx21 = F_Not (F_And (F_Not (F_Or (F_Var "d", F_And (F_Var "e", F_Or (F_Var "h", F_Var "e")))), F_Or (F_Var "a", F_And (F_Var "f", F_Var "b"))))
val fmlaEx22 = F_Not (F_And (F_Var "h", F_Or (F_Or (F_Var "b", F_Not (F_And (F_Not (F_And (F_Var "f", F_Not (F_Var "d"))), F_Or (F_Var "a", F_Not (F_Not (F_Not (F_And (F_Or (F_And (F_Or (F_Var "b", F_Var "h"), F_And (F_And (F_Not (F_Var "e"), F_And (F_Var "g", F_Var "h")), F_Or (F_Var "b", F_Or (F_Or (F_Var "b", F_Not (F_Var "h")), F_Var "b")))), F_Var "b"), F_And (F_Var "b", F_Var "b"))))))))), F_Var "b")))
val fmlaEx23 = F_Or (F_Not (F_Var "a"), F_And (F_Or (F_Not (F_Or (F_Or (F_Var "e", F_And (F_Var "h", F_Not (F_Var "g"))), F_Var "a")), F_Var "g"), F_Var "c"))
val fmlaEx24 = F_Or (F_Or (F_Or (F_Not (F_Var "d"), F_Not (F_Var "b")), F_And (F_Var "b", F_Var "e")), F_Var "d")
val fmlaEx25 = F_Or (F_Not (F_Or (F_And (F_Not (F_Not (F_Var "f")), F_Var "d"), F_Or (F_Or (F_Var "e", F_Not (F_Or (F_Or (F_And (F_Var "g", F_Var "g"), F_Not (F_And (F_Var "g", F_Not (F_Var "c")))), F_And (F_Var "c", F_Or (F_And (F_Not (F_Not (F_Not (F_Not (F_Not (F_Var "g"))))), F_Or (F_And (F_And (F_Or (F_And (F_Var "b", F_And (F_And (F_And (F_Var "h", F_Or (F_Var "g", F_Var "d")), F_Or (F_Var "c", F_Not (F_Var "e"))), F_Or (F_Var "g", F_Var "g"))), F_Var "a"), F_Not (F_And (F_Var "a", F_Var "e"))), F_Var "h"), F_Var "e")), F_Var "g"))))), F_Not (F_Or (F_Var "e", F_Not (F_Not (F_Var "h"))))))), F_Not (F_Var "e"))
val fmlaEx26 = F_Not (F_Var "d")
val fmlaEx27 = F_Or (F_Var "a", F_Not (F_Not (F_Not (F_And (F_Var "e", F_Var "h")))))
val fmlaEx28 = F_Or (F_Or (F_Not (F_Var "a"), F_Var "a"), F_Or (F_Or (F_Var "h", F_Not (F_And (F_Var "b", F_Or (F_And (F_And (F_And (F_And (F_Var "a", F_Not (F_Not (F_Or (F_Or (F_Var "b", F_Var "b"), F_Var "g")))), F_Var "e"), F_And (F_Or (F_Not (F_Not (F_And (F_Or (F_Var "d", F_Var "e"), F_Not (F_Var "e")))), F_Not (F_And (F_Var "e", F_Or (F_Or (F_Var "b", F_Var "a"), F_And (F_Var "f", F_Not (F_Or (F_Var "c", F_Var "e"))))))), F_Var "h")), F_Var "a"), F_And (F_Var "a", F_Var "f"))))), F_Or (F_Var "e", F_And (F_Or (F_And (F_Not (F_Var "g"), F_And (F_Var "g", F_Not (F_Or (F_Var "e", F_And (F_Or (F_Var "g", F_Var "c"), F_Var "g"))))), F_Var "f"), F_Var "a"))))
val fmlaEx29 = F_Or (F_Not (F_Or (F_Not (F_Not (F_Not (F_And (F_Var "b", F_Not (F_And (F_Not (F_Not (F_Var "b")), F_Or (F_Not (F_Or (F_Var "f", F_And (F_And (F_And (F_Not (F_Not (F_Var "g")), F_Var "g"), F_Not (F_Var "d")), F_Or (F_Or (F_Var "a", F_Or (F_Not (F_And (F_Var "h", F_Or (F_Var "b", F_Var "d"))), F_Var "e")), F_Not (F_Var "a"))))), F_Var "b"))))))), F_Not (F_Var "a"))), F_Var "e")
val fmlaEx30 = F_And (F_And (F_Var "f", F_Var "f"), F_Not (F_And (F_Not (F_Not (F_Var "a")), F_Var "h")))
val fmlaEx31 = F_Var "a"
val fmlaEx32 = F_Or (F_Or (F_Or (F_Var "a", F_Var "a"), F_Not (F_Not (F_Not (F_And (F_Or (F_Not (F_Not (F_Not (F_And (F_Not (F_Var "d"), F_Not (F_Var "d"))))), F_Var "c"), F_Not (F_Not (F_And (F_Not (F_Var "c"), F_And (F_Not (F_Var "c"), F_Not (F_Var "d")))))))))), F_Var "g")

val envEx01 = [("a", false), ("b", false), ("c", false), ("d", true), ("e", true), ("f", false), ("g", false), ("h", true)]
val envEx02 = [("a", false), ("b", true), ("c", false), ("d", false), ("e", false), ("f", true), ("g", false), ("h", false)]
val envEx03 = [("a", true), ("b", false), ("c", true), ("d", false), ("e", false), ("f", false), ("g", true), ("h", false)]
val envEx04 = [("a", true), ("b", true), ("c", true), ("d", true), ("e", true), ("f", true), ("g", false), ("h", false)]
