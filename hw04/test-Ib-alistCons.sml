val _ : 'a * 'a alist -> 'a alist = alistCons

fun run_alistCons_test elemEq test = run_test alistCons (alistEq elemEq) test

val _ = run_alistCons_test intEq ("alistCons_test01", (99, alistEx1), NonNil (Sing (99)))
val _ = run_alistCons_test intEq ("alistCons_test02", (99, alistEx2), NonNil (Append (Sing (99), Sing (42))))
val _ = run_alistCons_test intEq ("alistCons_test03", (99, alistEx3), NonNil (Append (Sing (99), Append (Sing (23), Sing (24)))))
val _ = run_alistCons_test intEq ("alistCons_test04", (99, alistEx4), NonNil (Append (Sing (99), Append (Append (Sing (17), Sing (18)), Sing (19)))))
val _ = run_alistCons_test intEq ("alistCons_test05", (99, alistEx5), NonNil (Append (Sing (99), Append (Sing (39), Append (Sing (38), Sing (37))))))
val _ = run_alistCons_test intEq ("alistCons_test06", (99, alistEx6), NonNil (Append (Sing (99), Append (Append (Append (Sing (27), Sing (28)), Sing (29)), Append (Sing (79), Append (Sing (78), Sing (77)))))))
val _ = run_alistCons_test intEq ("alistCons_test07", (99, alistEx7), NonNil (Append (Sing (99), Append (Append (Sing (50), Sing (51)), Append (Sing (61), Sing (60))))))
