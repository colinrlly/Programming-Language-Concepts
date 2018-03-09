val _ : int -> ('a -> 'a) -> 'a -> 'a = compound

fun run_compound_test check test = run_test (fn (n,f,x) => compound n f x) check test

val _ = run_compound_test intEq ("compound_test01", (0, fn x => x + 1, 10), 10)
val _ = run_compound_test intEq ("compound_test02", (5, fn x => x + 1, 10), 15)
val _ = run_compound_test intEq ("compound_test03", (10, fn x => x + 1, 10), 20)
val _ = run_compound_test intEq ("compound_test04", (0, fn x => x + x, 2), 2)
val _ = run_compound_test intEq ("compound_test05", (4, fn x => x + x, 2), 32)
val _ = run_compound_test stringEq ("compound_test06", (0, fn x => x ^ x, "*"), "*")
val _ = run_compound_test stringEq ("compound_test06", (3, fn x => x ^ x, "*"), "********")
