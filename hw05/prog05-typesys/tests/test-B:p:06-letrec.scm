(val bogus
  (letrec (([bogus : (int -> bool)] (lambda ([x : int]) x)))
    bogus))
