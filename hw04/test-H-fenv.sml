val _ : 'a fenv = fenvEmpty
val _ : string * 'a fenv -> 'a = fenvFind
val _ : string * 'a * 'a fenv -> 'a fenv = fenvBind

fun run_fenv_test test = run_test (wrapFind fenvFind) (findResEq intEq) test

val _ = run_fenv_test ("fenv_test01", ("a", fenvEmpty), FR_NotFound "a")
val _ = run_fenv_test ("fenv_test02", ("b", fenvEmpty), FR_NotFound "b")
val _ = run_fenv_test ("fenv_test03", ("c", fenvEmpty), FR_NotFound "c")
val _ = run_fenv_test ("fenv_test04", ("a", fenvBind ("a", 1, fenvEmpty)), FR_Found 1)
val _ = run_fenv_test ("fenv_test05", ("b", fenvBind ("a", 1, fenvEmpty)), FR_NotFound "b")
val _ = run_fenv_test ("fenv_test06", ("c", fenvBind ("a", 1, fenvEmpty)), FR_NotFound "c")
val _ = run_fenv_test ("fenv_test07", ("a", fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty))), FR_Found 1)
val _ = run_fenv_test ("fenv_test08", ("b", fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty))), FR_Found 2)
val _ = run_fenv_test ("fenv_test09", ("c", fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty))), FR_NotFound "c")
val _ = run_fenv_test ("fenv_test10", ("a", fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty)))), FR_Found 3)
val _ = run_fenv_test ("fenv_test11", ("b", fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty)))), FR_Found 2)
val _ = run_fenv_test ("fenv_test12", ("c", fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty)))), FR_NotFound "c")
val _ = run_fenv_test ("fenv_test13", ("a", fenvBind ("z", 4, fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty))))), FR_Found 3)
val _ = run_fenv_test ("fenv_test14", ("b", fenvBind ("z", 4, fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty))))), FR_Found 2)
val _ = run_fenv_test ("fenv_test15", ("c", fenvBind ("z", 4, fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty))))), FR_NotFound "c")
val _ = run_fenv_test ("fenv_test16", ("a", fenvBind ("c", 5, fenvBind ("z", 4, fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty)))))), FR_Found 3)
val _ = run_fenv_test ("fenv_test17", ("b", fenvBind ("c", 5, fenvBind ("z", 4, fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty)))))), FR_Found 2)
val _ = run_fenv_test ("fenv_test18", ("c", fenvBind ("c", 5, fenvBind ("z", 4, fenvBind ("a", 3, fenvBind ("b", 2, fenvBind ("a", 1, fenvEmpty)))))), FR_Found 5)
