val _ : 'a list * 'b list -> ('a * 'b) list = zip

fun run_zip_test check test = run_test zip check test

val _ = run_zip_test (listEq (pairEq (intEq, stringEq))) ("zip_test01", ([],[]), [])
val _ = run_zip_test (listEq (pairEq (intEq, stringEq))) ("zip_test02", ([1,3,5],[]), [])
val _ = run_zip_test (listEq (pairEq (intEq, stringEq))) ("zip_test03", ([],["2","4","6"]), [])
val _ = run_zip_test (listEq (pairEq (intEq, stringEq))) ("zip_test04", ([1,3,5],["2","4","6"]), [(1,"2"),(3,"4"),(5,"6")])
val _ = run_zip_test (listEq (pairEq (intEq, stringEq))) ("zip_test05", ([1,3,5,7,9],["2","4","6"]), [(1,"2"),(3,"4"),(5,"6")])
val _ = run_zip_test (listEq (pairEq (intEq, stringEq))) ("zip_test06", ([1,3,5],["2","4","6","8","10"]), [(1,"2"),(3,"4"),(5,"6")])
val _ = run_zip_test (listEq (pairEq (intEq, stringEq))) ("zip_test07", ([1,3,5,7,9],["2","4","6","8","10"]), [(1,"2"),(3,"4"),(5,"6"),(7,"8"),(9,"10")])
