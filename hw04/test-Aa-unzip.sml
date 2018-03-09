val _ : ('a * 'b) list -> 'a list * 'b list = unzip

fun run_unzip_test check test = run_test unzip check test

val _ = run_unzip_test (pairEq (listEq intEq, listEq stringEq)) ("unzip_test01", [], ([],[]))
val _ = run_unzip_test (pairEq (listEq intEq, listEq stringEq)) ("unzip_test02", [(1,"2"),(3,"4"),(5,"6")], ([1,3,5],["2","4","6"]))
val _ = run_unzip_test (pairEq (listEq intEq, listEq stringEq)) ("unzip_test03", [(1,"2"),(3,"4"),(5,"6"),(7,"8"),(9,"10")], ([1,3,5,7,9],["2","4","6","8","10"]))
