val _ : int * 'a * 'a list -> 'a list = separate

fun run_separate_test test = run_test separate (listEq intEq) test

val _ = run_separate_test ("separate_test01", (1, 0, []), [])
val _ = run_separate_test ("separate_test02", (1, 0, [1]), [1])
val _ = run_separate_test ("separate_test03", (1, 0, [1,2]), [1,0,2])
val _ = run_separate_test ("separate_test04", (1, 0, [1,2,3]), [1,0,2,0,3])
val _ = run_separate_test ("separate_test05", (1, 0, [1,2,3,4]), [1,0,2,0,3,0,4])
val _ = run_separate_test ("separate_test06", (1, 0, [1,2,3,4,5]), [1,0,2,0,3,0,4,0,5])
val _ = run_separate_test ("separate_test07", (1, 0, [1,2,3,4,5,6]), [1,0,2,0,3,0,4,0,5,0,6])
val _ = run_separate_test ("separate_test07", (1, 0, [1,2,3,4,5,6,7]), [1,0,2,0,3,0,4,0,5,0,6,0,7])
val _ = run_separate_test ("separate_test08", (2, 0, []), [])
val _ = run_separate_test ("separate_test09", (2, 0, [1]), [1])
val _ = run_separate_test ("separate_test10", (2, 0, [1,2]), [1,2])
val _ = run_separate_test ("separate_test11", (2, 0, [1,2,3]), [1,0,2,3])
val _ = run_separate_test ("separate_test12", (2, 0, [1,2,3,4]), [1,2,0,3,4])
val _ = run_separate_test ("separate_test13", (2, 0, [1,2,3,4,5]), [1,0,2,3,0,4,5])
val _ = run_separate_test ("separate_test14", (2, 0, [1,2,3,4,5,6]), [1,2,0,3,4,0,5,6])
val _ = run_separate_test ("separate_test15", (2, 0, [1,2,3,4,5,6,7]), [1,0,2,3,0,4,5,0,6,7])
val _ = run_separate_test ("separate_test16", (3, 0, []), [])
val _ = run_separate_test ("separate_test17", (3, 0, [1]), [1])
val _ = run_separate_test ("separate_test18", (3, 0, [1,2]), [1,2])
val _ = run_separate_test ("separate_test19", (3, 0, [1,2,3]), [1,2,3])
val _ = run_separate_test ("separate_test20", (3, 0, [1,2,3,4]), [1,0,2,3,4])
val _ = run_separate_test ("separate_test21", (3, 0, [1,2,3,4,5]), [1,2,0,3,4,5])
val _ = run_separate_test ("separate_test22", (3, 0, [1,2,3,4,5,6]), [1,2,3,0,4,5,6])
val _ = run_separate_test ("separate_test23", (3, 0, [1,2,3,4,5,6,7]), [1,0,2,3,4,0,5,6,7])

fun run_onesSeparate_test test = run_test onesSeparate stringEq test
fun run_tensSeparate_test test = run_test tensSeparate stringEq test
fun run_thousandsSeparate_test test = run_test thousandsSeparate stringEq test

val _ = run_onesSeparate_test ("separate_test24", 1234567890, "1,2,3,4,5,6,7,8,9,0")
val _ = run_tensSeparate_test ("separate_test25", 1234567890, "12,34,56,78,90")
val _ = run_thousandsSeparate_test ("separate_test26", 1234567890, "1,234,567,890")
