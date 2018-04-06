(val flipApp
  (type-lambda ('b)
    (lambda ([x : 'b])
      (type-lambda ('a)
        (lambda ([f : ('b -> 'a)])
          (f x))))))

(val id
  (type-lambda ('a)
    (lambda ([x : 'a])
      ((@ ((@ flipApp 'a) x) 'a) (lambda ([z : 'a]) z)))))
