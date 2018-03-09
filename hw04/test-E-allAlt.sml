val _ : ('a -> bool) -> 'a list -> bool = allAlt

fun run_allAlt_test test = run_test (uncurry2 allAlt) boolEq test

val _ = run_allAlt_test ("allAlt_test01", (fn x => x mod 2 = 1, []), true)
val _ = run_allAlt_test ("allAlt_test02", (fn x => x mod 2 = 1, [1]), true)
val _ = run_allAlt_test ("allAlt_test03", (fn x => x mod 2 = 1, [1,2,3,4,5,6,7,8,9]), true)
val _ = run_allAlt_test ("allAlt_test04", (fn x => x mod 2 = 1, [1,2,3,4,6,7,8,9]), false)
val _ = run_allAlt_test ("allAlt_test05", (fn x => x < "monkey", ["ant","zebra","cat","yak"]), true)
val _ = run_allAlt_test ("allAlt_test06", (fn x => x < "monkey", ["ant","zebra","yak","cat"]), false)
val _ = run_allAlt_test ("allAlt_test07", (fn x => x mod 2 = 1, [1,1,1,1]), false)
