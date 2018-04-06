(val even
  (letrec (([even : (int -> bool)]
            (lambda ([x : int])
              (if (<= x 0) #t (odd (- x 1)))))
           ([odd : (int -> bool)]
            (lambda ([x : int])
              (if (<= x 0) #f (even (- x 1 ))))))
    even))
(val ansA (even 20))
(val ansB (even 23))
