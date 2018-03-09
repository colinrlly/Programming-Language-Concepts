val _ : int -> int -> int = exp

fun run_exp_test test = run_test (fn (b,e) => exp b e) intEq test

val _ = run_exp_test ("exp_test01", (3, 0), 1)
val _ = run_exp_test ("exp_test02", (3, 1), 3)
val _ = run_exp_test ("exp_test03", (3, 2), 9)
val _ = run_exp_test ("exp_test04", (3, 3), 27)
val _ = run_exp_test ("exp_test05", (2, 0), 1)
val _ = run_exp_test ("exp_test06", (2, 1), 2)
val _ = run_exp_test ("exp_test07", (2, 2), 4)
val _ = run_exp_test ("exp_test08", (2, 3), 8)
val _ = run_exp_test ("exp_test09", (2, 4), 16)
val _ = run_exp_test ("exp_test10", (~3, 0), 1)
val _ = run_exp_test ("exp_test11", (~3, 1), ~3)
val _ = run_exp_test ("exp_test12", (~3, 2), 9)
val _ = run_exp_test ("exp_test13", (~3, 3), ~27)
val _ = run_exp_test ("exp_test14", (~2, 0), 1)
val _ = run_exp_test ("exp_test15", (~2, 1), ~2)
val _ = run_exp_test ("exp_test16", (~2, 2), 4)
val _ = run_exp_test ("exp_test17", (~2, 3), ~8)
val _ = run_exp_test ("exp_test18", (~2, 4), 16)
