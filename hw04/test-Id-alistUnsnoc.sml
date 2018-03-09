val _ : 'a alist -> ('a alist * 'a) option = alistUnsnoc

fun run_alistUnsnoc_test elemEq test = run_test alistUnsnoc (optionEq (pairEq (alistEq elemEq, elemEq))) test

val _ = run_alistUnsnoc_test intEq ("alistUnsnoc_test01", alistEx1, NONE)
val _ = run_alistUnsnoc_test intEq ("alistUnsnoc_test02", alistEx2, SOME (Nil, 42))
val _ = run_alistUnsnoc_test intEq ("alistUnsnoc_test03", alistEx3, SOME (NonNil (Sing (23)), 24))
val _ = run_alistUnsnoc_test intEq ("alistUnsnoc_test04", alistEx4, SOME (NonNil (Append (Sing (17), Sing (18))), 19))
val _ = run_alistUnsnoc_test intEq ("alistUnsnoc_test05", alistEx5, SOME (NonNil (Append (Sing (39), Sing (38))), 37))
val _ = run_alistUnsnoc_test intEq ("alistUnsnoc_test06", alistEx6, SOME (NonNil (Append (Append (Append (Sing (27), Sing (28)), Sing (29)), Append (Sing (79), Sing (78)))), 77))
val _ = run_alistUnsnoc_test intEq ("alistUnsnoc_test07", alistEx7, SOME (NonNil (Append (Append (Sing (50), Sing (51)), Sing (61))), 60))
