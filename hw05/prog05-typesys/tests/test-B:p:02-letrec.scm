(val const
  (letrec (([f : (int -> int)]
            (lambda ([x : int]) 0)))
    f))
(val ans (const 0))
