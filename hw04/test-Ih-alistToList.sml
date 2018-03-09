val _ : 'a alist -> 'a list = alistToList

fun run_alistToList_test elemEq test = run_test alistToList (listEq elemEq) test

val _ = run_alistToList_test intEq ("alistToList_test01", alistEx1, [])
val _ = run_alistToList_test intEq ("alistToList_test02", alistEx2, [42])
val _ = run_alistToList_test intEq ("alistToList_test03", alistEx3, [23, 24])
val _ = run_alistToList_test intEq ("alistToList_test04", alistEx4, [17, 18, 19])
val _ = run_alistToList_test intEq ("alistToList_test05", alistEx5, [39, 38, 37])
val _ = run_alistToList_test intEq ("alistToList_test06", alistEx6, [27, 28, 29, 79, 78, 77])
val _ = run_alistToList_test intEq ("alistToList_test07", alistEx7, [50, 51, 61, 60])
