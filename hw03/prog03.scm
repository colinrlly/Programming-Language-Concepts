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

;; E.b (Exercise 10b)
;; DEFINE dropwhile HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part F (arg-max)

;; DEFINE arg-max HERE


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part J

;; DEFINE balanced?eck-expect (count 'a '()) 0)

(check-expect (count 'a '(a)) 1)
(check-expect (count 'a '(b)) 0)
(check-expect (count 'a '(a a)) 2)
(check-expect (count 'a '(a b)) 1)
(check-expect (count 'a '(b a)) 1)
(check-expect (count 'a '(b b)) 0)
(check-expect (count 'a '(a (a))) 1)
(check-expect (count 'a '(a (b))) 1)
(check-expect (count 'a '(b (a))) 0)
(check-expect (count 'a '(b (b))) 0)
(check-expect (count 'a '((a) (a))) 0)
(check-expect (count 'a '((a) (b))) 0)
(check-expect (count 'a '((b) (a))) 0)
(check-expect (count 'a '((b) (b))) 0)
(check-expect (count 'a '((a a))) 0)
(check-expect (count 'a '((a b))) 0)
(check-expect (count 'a '((b a))) 0)
(check-expect (count 'a '((b b))) 0)
(check-expect (count 'a '(1 b a (c a))) 1)
(check-expect (count 'a '(0 (-1) -1 (-1) ((((5) #f 2 (1 -5 0)) (0) 1 -5) 5 () ((-2 e 1 #f a 1) ()) #t) -1 ((#t ((#t b -5 b 0 (2 d a)) (1 (d) 1) 0) (() (c) b 0)) () -2 -2) #t)) 0)
(check-expect (count 'a '(-1 c a -2 a (b 0 -2 1 0) d #t)) 2)
(check-expect (count 'a '(0 (#t b (0 #t)) -1 () (a (((0) e e e 1 a) d -2 -2 (-2 () e) ()) (d)))) 0)
(check-expect (count 'a '(-1 5 b d 2 (b d) 1 2 ((((-5) #f 2 -1) c)))) 0)
(check-expect (count 'a '((0 -2) (b () (b -1 -5 -1 (1 a #t)) 5 #t c) #f ((-2 2 1) b 5 a) 1 a -2 e d 1 b)) 1)
(check-expect (count 'a '(-5 -5 0 -2 -2 1 c -5 c () d)) 0)
(check-expect (count 'a '(((0 e 1 (0 d (1 0 -2 (a (b 5))) ((1 #f #f) #f () #t -5 -5) (-5 c c #f b)) c) b d #t) ((#t 5 (d -2 2 5)) (a e (((5)) -2 e 5)) a) e 1 -5 (d))) 0)
(check-expect (count 'a '(1 b 0)) 0)
(check-expect (count 'a '(() -2 a -2 (0 ()) e)) 1)

;; test-Ab-countall.scm
(check-expect (countall 'a '()) 0)
(check-expect (countall 'a '(a)) 1)
(check-expect (countall 'a '(b)) 0)
(check-expect (countall 'a '(a a)) 2)
(check-expect (countall 'a '(a b)) 1)
(check-expect (countall 'a '(b a)) 1)
(check-expect (countall 'a '(b b)) 0)
(check-expect (countall 'a '(a (a))) 2)
(check-expect (countall 'a '(a (b))) 1)
(check-expect (countall 'a '(b (a))) 1)
(check-expect (countall 'a '(b (b))) 0)
(check-expect (countall 'a '((a) (a))) 2)
(check-expect (countall 'a '((a) (b))) 1)
(check-expect (countall 'a '((b) (a))) 1)
(check-expect (countall 'a '((b) (b))) 0)
(check-expect (countall 'a '((a a))) 2)
(check-expect (countall 'a '((a b))) 1)
(check-expect (countall 'a '((b a))) 1)
(check-expect (countall 'a '((b b))) 0)
(check-expect (countall 'a '(1 b a (c a))) 2)
(check-expect (countall 'a '(0 (-1) -1 (-1) ((((5) #f 2 (1 -5 0)) (0) 1 -5) 5 () ((-2 e 1 #f a 1) ()) #t) -1 ((#t ((#t b -5 b 0 (2 d a)) (1 (d) 1) 0) (() (c) b 0)) () -2 -2) #t)) 2)
(check-expect (countall 'a '(-1 c a -2 a (b 0 -2 1 0) d #t)) 2)
(check-expect (countall 'a '(0 (#t b (0 #t)) -1 () (a (((0) e e e 1 a) d -2 -2 (-2 () e) ()) (d)))) 2)
(check-expect (countall 'a '(-1 5 b d 2 (b d) 1 2 ((((-5) #f 2 -1) c)))) 0)
(check-expect (countall 'a '((0 -2) (b () (b -1 -5 -1 (1 a #t)) 5 #t c) #f ((-2 2 1) b 5 a) 1 a -2 e d 1 b)) 3)
(check-expect (countall 'a '(-5 -5 0 -2 -2 1 c -5 c () d)) 0)
(check-expect (countall 'a '(((0 e 1 (0 d (1 0 -2 (a (b 5))) ((1 #f #f) #f () #t -5 -5) (-5 c c #f b)) c) b d #t) ((#t 5 (d -2 2 5)) (a e (((5)) -2 e 5)) a) e 1 -5 (d))) 3)
(check-expect (countall 'a '(1 b 0)) 0)
(check-expect (countall 'a '(() -2 a -2 (0 ()) e)) 1)

;; test-Ac-mirror.scm
(check-expect (mirror '()) '())
(check-expect (mirror '(())) '(()))
(check-expect (mirror '(() () (() ()))) '((() ()) () ()))
(check-expect (mirror '(1 2 3 4 5)) '(5 4 3 2 1))
(check-expect (mirror '((a (b 5)) (c d) e)) '(e (d c) ((5 b) a)))
(check-expect (mirror '(0 (-1) -1 (-1) ((((5) #f 2 (1 -5 0)) (0) 1 -5) 5 () ((-2 e 1 #f a 1) ()) #t) -1 ((#t ((#t b -5 b 0 (2 d a)) (1 (d) 1) 0) (() (c) b 0)) () -2 -2) #t)) '(#t (-2 -2 () ((0 b (c) ()) (0 (1 (d) 1) ((a d 2) 0 b -5 b #t)) #t)) -1 (#t (() (1 a #f 1 e -2)) () 5 (-5 1 (0) ((0 -5 1) 2 #f (5)))) (-1) -1 (-1) 0))
(check-expect (mirror '(-1 c a -2 a (b 0 -2 1 0) d #t)) '(#t d (0 1 -2 0 b) a -2 a c -1))
(check-expect (mirror '(0 (#t b (0 #t)) -1 () (a (((0) e e e 1 a) d -2 -2 (-2 () e) ()) (d)))) '(((d) (() (e () -2) -2 -2 d (a 1 e e e (0))) a) () -1 ((#t 0) b #t) 0))
(check-expect (mirror '(-1 5 b d 2 (b d) 1 2 ((((-5) #f 2 -1) c)))) '(((c (-1 2 #f (-5)))) 2 1 (d b) 2 d b 5 -1))
(check-expect (mirror '((0 -2) (b () (b -1 -5 -1 (1 a #t)) 5 #t c) #f ((-2 2 1) b 5 a) 1 a -2 e d 1 b)) '(b 1 d e -2 a 1 (a 5 b (1 2 -2)) #f (c #t 5 ((#t a 1) -1 -5 -1 b) () b) (-2 0)))
(check-expect (mirror '(-5 -5 0 -2 -2 1 c -5 c () d)) '(d () c -5 c 1 -2 -2 0 -5 -5))
(check-expect (mirror '(((0 e 1 (0 d (1 0 -2 (a (b 5))) ((1 #f #f) #f () #t -5 -5) (-5 c c #f b)) c) b d #t) ((#t 5 (d -2 2 5)) (a e (((5)) -2 e 5)) a) e 1 -5 (d))) '((d) -5 1 e (a ((5 e -2 ((5))) e a) ((5 2 -2 d) 5 #t)) (#t d b (c ((b #f c c -5) (-5 -5 #t () #f (#f #f 1)) (((5 b) a) -2 0 1) d 0) 1 e 0))))
(check-expect (mirror '(1 b 0)) '(0 b 1))

;; test-Ad-flatten.scm
(check-expect (flatten '()) '())
(check-expect (flatten '(())) '())
(check-expect (flatten '(() () (() ()))) '())
(check-expect (flatten '((I Ching) (U Thant) (E Coli))) '(I Ching U Thant E Coli))
(check-expect (flatten '((a b) ((c d) e))) '(a b c d e))
(check-expect (flatten '(0 (-1) -1 (-1) ((((5) #f 2 (1 -5 0)) (0) 1 -5) 5 () ((-2 e 1 #f a 1) ()) #t) -1 ((#t ((#t b -5 b 0 (2 d a)) (1 (d) 1) 0) (() (c) b 0)) () -2 -2) #t)) '(0 -1 -1 -1 5 #f 2 1 -5 0 0 1 -5 5 -2 e 1 #f a 1 #t -1 #t #t b -5 b 0 2 d a 1 d 1 0 c b 0 -2 -2 #t))
(check-expect (flatten '(-1 c a -2 a (b 0 -2 1 0) d #t)) '(-1 c a -2 a b 0 -2 1 0 d #t))
(check-expect (flatten '(0 (#t b (0 #t)) -1 () (a (((0) e e e 1 a) d -2 -2 (-2 () e) ()) (d)))) '(0 #t b 0 #t -1 a 0 e e e 1 a d -2 -2 -2 e d))
(check-expect (flatten '(-1 5 b d 2 (b d) 1 2 ((((-5) #f 2 -1) c)))) '(-1 5 b d 2 b d 1 2 -5 #f 2 -1 c))
(check-expect (flatten '((0 -2) (b () (b -1 -5 -1 (1 a #t)) 5 #t c) #f ((-2 2 1) b 5 a) 1 a -2 e d 1 b)) '(0 -2 b b -1 -5 -1 1 a #t 5 #t c #f -2 2 1 b 5 a 1 a -2 e d 1 b))
(check-expect (flatten '(-5 -5 0 -2 -2 1 c -5 c () d)) '(-5 -5 0 -2 -2 1 c -5 c d))
(check-expect (flatten '(((0 e 1 (0 d (1 0 -2 (a (b 5))) ((1 #f #f) #f () #t -5 -5) (-5 c c #f b)) c) b d #t) ((#t 5 (d -2 2 5)) (a e (((5)) -2 e 5)) a) e 1 -5 (d))) '(0 e 1 0 d 1 0 -2 a b 5 1 #f #f #f #t -5 -5 -5 c c #f b c b d #t #t 5 d -2 2 5 a e 5 -2 e 5 a e 1 -5 d))
(check-expect (flatten '(1 b 0)) '(1 b 0))
(check-expect (flatten '(() -2 a -2 (0 ()) e)) '(-2 a -2 0 e))

;; test-C-interleave.scm
(check-expect (interleave '(a b c) '(d e f)) '(a d b e c f))
(check-expect (interleave '(a b c d) '(e f)) '(a e b f c d))
(check-expect (interleave '(a b) '(c d e f)) '(a c b d e f))
(check-expect (interleave '() '()) '())
(check-expect (interleave '(a b) '()) '(a b))
(check-expect (interleave '() '(d e)) '(d e))
(check-expect (interleave '(1 2 3 4 5) '(a b c d e)) '(1 a 2 b 3 c 4 d 5 e))

;; test-Ae-sublist?.scm
(check-expect (sublist? '() '()) #t)
(check-expect (sublist? '() '(a b c)) #t)
(check-expect (sublist? '(a) '()) #f)
(check-expect (sublist? '(a) '(a b c)) #t)
(check-expect (sublist? '(a) '(1 2 a b c 5 6)) #t)
(check-expect (sublist? '(a) '(1 2 a 3 b 4 c 5 6)) #t)
(check-expect (sublist? '(a b c) '()) #f)
(check-expect (sublist? '(a b c) '(a b c)) #t)
(check-expect (sublist? '(a b c) '(c b a)) #f)
(check-expect (sublist? '(a b c) '(1 2 a b c 5 6)) #t)
(check-expect (sublist? '(a b c) '(6 5 c b a 2 1)) #f)
(check-expect (sublist? '(a b c) '(1 2 a 3 b 4 c 5 6)) #f)
(check-expect (sublist? '(a b c) '(6 5 c 4 b 3 a 2 1)) #f)
(check-expect (sublist? '(a y b z c) '()) #f)
(check-expect (sublist? '(a y b z c) '(a b c)) #f)
(check-expect (sublist? '(a y b z c) '(c b a)) #f)
(check-expect (sublist? '(a y b z c) '(1 2 a b c 5 6)) #f)
(check-expect (sublist? '(a y b z c) '(6 5 c b a 2 1)) #f)
(check-expect (sublist? '(a y b z c) '(1 2 a 3 b 4 c 5 6)) #f)
(check-expect (sublist? '(a y b z c) '(6 5 c 4 b 3 a 2 1)) #f)
(check-expect (sublist? '(a b c) '(a a b a b c)) #t)
(check-expect (sublist? '(a a a) '(a a b a b c)) #f)
(check-expect (sublist? '(1 2 3) '(a b 1 1 2 3 c d)) #t)
(check-expect (sublist? '(a b c) '(a b x b c)) #f)

;; test-Af-subseq?.scm
(check-expect (subseq? '() '()) #t)
(check-expect (subseq? '() '(a b c)) #t)
(check-expect (subseq? '(a) '()) #f)
(check-expect (subseq? '(a) '(a b c)) #t)
(check-expect (subseq? '(a) '(1 2 a b c 5 6)) #t)
(check-expect (subseq? '(a) '(1 2 a 3 b 4 c 5 6)) #t)
(check-expect (subseq? '(a b c) '()) #f)
(check-expect (subseq? '(a b c) '(a b c)) #t)
(check-expect (subseq? '(a b c) '(c b a)) #f)
(check-expect (subseq? '(a b c) '(1 2 a b c 5 6)) #t)
(check-expect (subseq? '(a b c) '(6 5 c b a 2 1)) #f)
(check-expect (subseq? '(a b c) '(1 2 a 3 b 4 c 5 6)) #t)
(check-expect (subseq? '(a b c) '(6 5 c 4 b 3 a 2 1)) #f)
(check-expect (subseq? '(a y b z c) '()) #f)
(check-expect (subseq? '(a y b z c) '(a b c)) #f)
(check-expect (subseq? '(a y b z c) '(c b a)) #f)
(check-expect (subseq? '(a y b z c) '(1 2 a b c 5 6)) #f)
(check-expect (subseq? '(a y b z c) '(6 5 c b a 2 1)) #f)
(check-expect (subseq? '(a y b z c) '(1 2 a 3 b 4 c 5 6)) #f)
(check-expect (subseq? '(a y b z c) '(6 5 c 4 b 3 a 2 1)) #f)
(check-expect (subseq? '(a b c) '(a a b a b c)) #t)
(check-expect (subseq? '(a a a) '(a a b a b c)) #t)
(check-expect (subseq? '(a b c) '(a b x b c)) #t)

;; test-B1-take.scm
(check-expect (take 0 '()) '())
(check-expect (take 5 '()) '())
(check-expect (take 0 '(a b c d e)) '())
(check-expect (take 1 '(a b c d e)) '(a))
(check-expect (take 2 '(a b c d e)) '(a b))
(check-expect (take 3 '(a b c d e)) '(a b c))
(check-expect (take 4 '(a b c d e)) '(a b c d))
(check-expect (take 5 '(a b c d e)) '(a b c d e))
(check-expect (take 6 '(a b c d e)) '(a b c d e))
(check-expect (take 7 '(a b c d e)) '(a b c d e))

;; test-B2-drop.scm
(check-expect (drop 0 '()) '())
(check-expect (drop 5 '()) '())
(check-expect (drop 0 '(a b c d e)) '(a b c d e))
(check-expect (drop 1 '(a b c d e)) '(b c d e))
(check-expect (drop 2 '(a b c d e)) '(c d e))
(check-expect (drop 3 '(a b c d e)) '(d e))
(check-expect (drop 4 '(a b c d e)) '(e))
(check-expect (drop 5 '(a b c d e)) '())
(check-expect (drop 6 '(a b c d e)) '())
(check-expect (drop 7 '(a b c d e)) '())
