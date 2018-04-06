(val f
  (type-lambda ('a)
  (type-lambda ('b)
  (type-lambda ('c)
  (type-lambda ('d)
  (type-lambda ('e)
  (lambda ([a : 'a] [b : 'b] [c : 'c] [d : 'd] [e : 'e]) 1)))))))

(val f
  (type-lambda ('a)
  (type-lambda ('b)
  (type-lambda ('c)
  (type-lambda ('d)
  (type-lambda ('e)
  (@ (@ (@ (@ (@ f (list 'e)) (list ('d -> 'e))) (list 'd)) ('b -> 'a)) 'e)))))))
