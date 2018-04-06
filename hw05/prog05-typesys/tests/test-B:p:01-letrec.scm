(val id
  (letrec (([id : (int -> int)]
            (lambda ([x : int]) x)))
    id))
(val ans (id 0))
