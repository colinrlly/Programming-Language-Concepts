(val bogus
  (letrec (([bogus : ( -> boo)]
            (lambda () #t)))
    bogus))
