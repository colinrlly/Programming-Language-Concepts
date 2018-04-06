(val-rec [fact : (int -> int)] (lambda ([n : int]) (if (< n 2) 1 (* n (fact (- n 1))))))
(val ans (fact 5))
