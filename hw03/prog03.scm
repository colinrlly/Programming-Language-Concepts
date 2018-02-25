;; Name: 
;; Time spent on assignment: 
;; Collaborators: 


;; (list? x)
;; returns #t if x is a proper list 
;; and returns #f otherwise.
(define list? (x)
    (if (null? x)
        #t
        (if (pair? x)
            (list? (cdr x))
            #f)))

;; (prefix? xs ys)
;; returns #t if xs is a prefix of ys (using equal? to compare elements)
;; and returns #f otherwise. 
(define prefix? (xs ys)
    (if (null? xs)
        #t
        (if (null? ys)
            #f
            (and (equal? (car xs) (car ys))
            (prefix? (cdr xs) (cdr ys))))))


(define even? (n) (= 0 (mod n 2)))
(val odd? (o not even?))
(val positive? ((curry <) 0))
(val zero? ((curry =) 0))
(val negative? ((curry >) 0))
(define flip (f) (lambda (b a) (f a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define append (l m)
   (if (null? l) m
       (cons (car l) (append (cdr l) m))
   )
)

;; Part A (Exercise 2)

;; A.a (Exercise 2a)
;; DEFINE count HERE
(define count (x xs)
    (if (null? xs)
        0
        (if (= x (car xs))
            (+ 1 (count x (cdr xs)))
            (+ 0 (count x (cdr xs)))
        )
    )  
)

;; A.b (Exercise 2b)
;; DEFINE countall HERE
(define countall (x xs)
    (if (null? xs)
        0
        (if (= x (car xs))
            (+ 1 (countall x (cdr xs)))
            (if (list? (car xs))
                (+ (countall x (car xs)) (countall x (cdr xs)))
                (countall x (cdr xs))
            )
        )
    )  
)

;; A.c (Exercise 2c)
(define mirror-help (lst end)
    (if (null? lst)
        end
        (if (list? (car lst))
            (mirror-help 
                (cdr lst)
                (cons 
                    (mirror-help 
                        (car lst) 
                        '()
                    ) 
                    end
                )
            )
            (mirror-help (cdr lst) (cons (car lst) end))
        )
    )
)

(define mirror (lst)
    (mirror-help lst '())  
)

;; A.d (Exercise 2d)
;; DEFINE flatten HERE
(define flatten (lst)
    (if (null? lst)
        lst
        (if (list? lst)
            (append (flatten (car lst)) (flatten (cdr lst)))
            (cons lst '())
        )
    )  
)

;; A.e (Exercise 2e)
;; DEFINE sublist? HERE
(define sublist? (sub xs)
    (if (null? sub)
        #t
        (if (null? xs)
            #f
            (if (prefix? sub xs)
                #t
                (sublist? sub (cdr xs))
            )
        )
    )
)

;; A.f (Exercise 2f)
;; DEFINE subseq? HERE
(define subseq? (sub xs)
    (if (null? sub)
        #t
        (if (null? xs)
            #f
            (if (= (car sub) (car xs))
                (subseq? (cdr sub) (cdr xs))
                (subseq? sub (cdr xs))
            )
        )
    )  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part B

;; B.1 (take)
;; DEFINE take HERE
(define take (n xs)
    (if (or (zero? n) (null? xs))
        '()
        (cons (car xs) (take (- n 1) (cdr xs)))
    )
)

;; B.2 (drop)
;; DEFINE drop HERE
(define drop (n xs)
    (if (or (zero? n) (null? xs))
        xs
        (drop (- n 1) (cdr xs))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part C (interleave)

;; DEFINE interleave HERE
( define interleave (xs ys) 
    (if (null? xs)
        ys
        (if (null? ys)
            xs
            (cons (car xs) (cons (car ys) (interleave (cdr xs) (cdr ys))))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part D (permutation?) !bonus!

;; DEFINE permutation? HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part E (Exercise 10)

;; E.a (Exercise 10a)
;; DEFINE takewhile HERE
(define takewhile (p? xs)
    (if (null? xs)
        xs
        (if (p? (car xs))
            (cons (car xs) (takewhile p? (cdr xs)))
            '()
        )
    )
)

;; E.b (Exercise 10b)
;; DEFINE dropwhile HERE
(define dropwhile (p? xs)
    (if (null? xs)
        xs
        (if (p? (car xs))
            (dropwhile p? (cdr xs))
            xs
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part F (arg-max)

;; DEFINE arg-max HERE
(define arg-max-app (f x y)
    (if (< (f x) (f y))
        y
        x
    )
)

(define arg-max (f xs)
    (if (null? xs)
        0
        (arg-max-app f (car xs) (arg-max f (cdr xs)))
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part G (Exercise 14)

;; G.b (Exercise 14b)
;; DEFINE max* HERE

;; G.c (Exercise 14c)
;; DEFINE gcd* HERE

;; G.g (Exercise 14h)
;; DEFINE append-via-fold HERE

;; G.i (Exercise 14j)
;; DEFINE reverse-via-fold HERE

;; G.j (Exercise 14k)

(define insert (x xs)
    (if (null? xs)
        (list1 x)
        (if (< x (car xs))
            (cons x xs)
            (cons (car xs) (insert x (cdr xs))))))

;; DEFINE insertion-sort HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part H (Exercise 19)

(val emptyset (lambda (x) #f))
(define member? (x s) (s x))

(val evens (lambda (x) (= (mod x 2) 0)))
(val two-digits (lambda (x) (and (<= 10 x) (<= x 99))))

;; H.1 (Exercise 19c)
;; DEFINE add-element HERE

;; H.2 (Exercise 19c)
;; DEFINE union HERE

;; H.3 (Exercise 19c)
;; DEFINE inter HERE

;; H.4 (Exercise 19c)
;; DEFINE diff HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part I

;; DEFINE clamp HERE
(define clamp (f low high)
    (lambda (x)
        ((lambda (n low high)
            (if (< n low)
                low
                (if (> n high)
                    high
                    n
                )
            )
        ) (f x) low high)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part J

;; DEFINE balanced? HERE
(define balanced? (f xs)
    (if (null? xs)
        #t
        (if (odd? (length xs))
            #f
            (if (f (car xs) (cadr xs))
                (balanced? f (cddr xs))
                #f
            )
        )
    )
)
