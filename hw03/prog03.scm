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


;; Part A (Exercise 2)

;; A.a (Exercise 2a)
;; DEFINE count HERE

;; A.b (Exercise 2b)
;; DEFINE countall HERE

;; A.c (Exercise 2c)
;; DEFINE mirror HERE

;; A.d (Exercise 2d)
;; DEFINE flatten HERE

;; A.e (Exercise 2e)
;; DEFINE sublist? HERE

;; A.f (Exercise 2f)
;; DEFINE subseq? HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part B

;; B.1 (take)
;; DEFINE take HERE

;; B.2 (drop)
;; DEFINE drop HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part C (interleave)

;; DEFINE interleave HERE
( define interleave (l1 l2) 
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (cons (car l1) (cons (car l2) (interleave (cdr l1) (cdr l2))))
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

;; DEFINE balanced? HERE


;; test-C-interleave.scm
(check-expect (interleave '(a b c) '(d e f)) '(a d b e c f))
(check-expect (interleave '(a b c d) '(e f)) '(a e b f c d))
(check-expect (interleave '(a b) '(c d e f)) '(a c b d e f))
(check-expect (interleave '() '()) '())
(check-expect (interleave '(a b) '()) '(a b))
(check-expect (interleave '() '(d e)) '(d e))
(check-expect (interleave '(1 2 3 4 5) '(a b c d e)) '(1 a 2 b 3 c 4 d 5 e))
