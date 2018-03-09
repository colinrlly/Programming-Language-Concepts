val _ : 'a alist * 'a -> 'a alist = alistSnoc

fun run_alistSnoc_test elemEq test = run_test alistSnoc (alistEq elemEq) test

val _ = run_alistSnoc_test intEq ("alistSnoc_test01", (alistEx1, 99), NonNil (Sing (99)))
val _ = run_alistSnoc_test intEq ("alistSnoc_test02", (alistEx2, 99), NonNil (Append (Sing (42), Sing (99))))
val _ = run_alistSnoc_test intEq ("alistSnoc_test03", (alistEx3, 99), NonNil (Append (Append (Sing (23), Sing (24)), Sing (99))))
val _ = run_alistSnoc_test intEq ("alistSnoc_test04", (alistEx4, 99), NonNil (Append (Append (Append (Sing (17), Sing (18)), Sing (19)), Sing (99))))
val _ = run_alistSnoc_test intEq ("alistSnoc_test05", (alistEx5, 99), NonNil (Append (Append (Sing (39), Append (Sing (38), Sing (37))), Sing (99))))
val _ = run_alistSnoc_test intEq ("alistSnoc_test06", (alistEx6, 99), NonNil (Append (Append (Append (Append (Sing (27), Sing (28)), Sing (29)), Append (Sing (79), Append (Sing (78), Sing (77)))), Sing (99))))
val _ = run_alistSnoc_test intEq ("alistSnoc_test07", (alistEx7, 99), NonNil (Append (Append (Append (Sing (50), Sing (51)), Append (Sing (61), Sing (60))), Sing (99))))
