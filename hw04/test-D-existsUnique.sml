val _ : ('a -> bool) -> 'a list -> bool = existsUnique

fun run_existsUnique_test test = run_test (uncurry2 existsUnique) boolEq test

val _ = run_existsUnique_test ("existsUnique_test01", (fn x => x mod 2 = 1, []), false)
val _ = run_existsUnique_test ("existsUnique_test02", (fn x => x mod 2 = 1, [1]), true)
val _ = run_existsUnique_test ("existsUnique_test03", (fn x => x mod 2 = 1, [1,2]), true)
val _ = run_existsUnique_test ("existsUnique_test04", (fn x => x mod 2 = 1, [1,2,3]), false)
val _ = run_existsUnique_test ("existsUnique_test05", (fn x => x mod 2 = 1, [1,2,3,4,5]), false)
val _ = run_existsUnique_test ("existsUnique_test06", (fn x => x mod 2 = 1, [2,4,5,6,8]), true)
val _ = run_existsUnique_test ("existsUnique_test07", (fn x => size x = 4, ["antelope","bat","cat","dear","elephant"]), true)
