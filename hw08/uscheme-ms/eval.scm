;;;scheme.nw:6758
(val emptystore '((next 0)))
;;;scheme.nw:3671
(define find-c (key alist success-cont failure-cont)
   (letrec
       ([search (lambda (alist)
                   (if (null? alist)
                       (failure-cont)
                       (if (equal? key (alist-first-key alist))
                           (success-cont (alist-first-attribute alist))
                           (search (cdr alist)))))])
     (search alist)))
;;;scheme.nw:6763
(val sigma emptystore)
(define load  (l)   (find-c l sigma (lambda (x) x)
                            (lambda () (error (list2 'unbound-location: l)))))
(define store (l v) (begin (set sigma (bind l v sigma)) v))
;;;scheme.nw:6772
(define allocate (value)
  (let*
    ([loc (load 'next)])
    (begin
       (store 'next (+ loc 1))
       (store loc value)
       loc)))
;;;scheme.nw:6786
(define bindalloc (name v env)
  (bind name (allocate v) env))
(define bindalloclist (xs vs env)
  (if (and (null? xs) (null? vs))
    env
    (bindalloclist (cdr xs) (cdr vs) (bindalloc (car xs) (car vs) env))))
;;;scheme.nw:6814
(define apply-prim (prim)
  (lambda (args)
    (if (= (length args) 1)
      (prim (car args))
      (if (= (length args) 2)
        (prim (car args) (cadr args))
        (error (list4 'all-primitives-expect-one-or-two-arguments---got (length args)
                      ': args))))))
;;;scheme.nw:6836
(define primenv ()
  (let*
      ([env '()]
       [env (bindalloc '+ (apply-prim +) env)]
       [env (bindalloc '- (apply-prim -) env)]
       [env (bindalloc '* (apply-prim *) env)]
       [env (bindalloc '/ (apply-prim /) env)]
       [env (bindalloc '< (apply-prim <) env)]
       [env (bindalloc '> (apply-prim >) env)]
       [env (bindalloc '= (apply-prim =) env)]
       [env (bindalloc 'car        (apply-prim car)        env)]
       [env (bindalloc 'cdr        (apply-prim cdr)        env)]
       [env (bindalloc 'cons       (apply-prim cons)       env)]
       [env (bindalloc 'println    (apply-prim println)    env)]
       [env (bindalloc 'print      (apply-prim print)      env)]
       [env (bindalloc 'printu     (apply-prim printu)     env)]
       [env (bindalloc 'error      (apply-prim error)      env)]
       [env (bindalloc 'boolean?   (apply-prim boolean?)   env)]
       [env (bindalloc 'null?      (apply-prim null?)      env)]
       [env (bindalloc 'number?    (apply-prim number?)    env)]
       [env (bindalloc 'symbol?    (apply-prim symbol?)    env)]
       [env (bindalloc 'procedure? (apply-prim procedure?) env)]
       [env (bindalloc 'pair?      (apply-prim pair?)      env)])
    env))
;;;scheme.nw:6900
(define find-variable (x env)
  (find-c x env (lambda (x) x) (lambda () (error (list2 'unbound-variable: x)))))
;;;scheme.nw:6942
(define unary (name f rest)
  (if (= (length rest) 1)
    (f (car rest))
    (error (list5 name 'expression-needs-one-argument,-got (length rest) 'in rest))))
;;;scheme.nw:6947
(define binary (name f rest)
  (if (= (length rest) 2)
    (f (car rest) (cadr rest))
    (error (list5 name 'expression-needs-two-arguments,-got (length rest) 'in rest))))
;;;scheme.nw:6952
(define trinary (name f rest)
  (if (= (length rest) 3)
    (f (car rest) (cadr rest) (caddr rest))
    (error (list5 name 'expression-needs-three-arguments,-got (length rest) 'in
                  rest))))
;;;scheme.nw:6872
(define eval (env)
   (letrec
       ([ev (lambda (e) 
;;;scheme.nw:6887
(if (symbol? e)
  (load (find-variable e env))
  (if (atom? e)
    e
    (let ([first (car e)]
          [rest  (cdr e)])
      (if (exists? ((curry =) first) '(set if while lambda quote begin))
          
;;;scheme.nw:6916
(if (= first 'set)    (binary  'set    meta-set    rest)
(if (= first 'if)     (trinary 'if     meta-if     rest)
(if (= first 'while)  (binary  'while  meta-while  rest)
(if (= first 'lambda) (binary  'lambda meta-lambda rest)
(if (= first 'quote)  (unary   'quote  meta-quote  rest)
(if (= first 'begin)  (meta-begin rest)
(error (list2 'this-cannot-happen---bad-ast first))))))))
;;;scheme.nw:6895
          
;;;scheme.nw:6908
((ev first) (map ev rest))
;;;scheme.nw:6895
                                                                                       ))))
;;;scheme.nw:6874
                                                                                        )]
        
;;;scheme.nw:6961
(meta-quote (lambda (e) e))
(meta-if    (lambda (e1 e2 e3) (if (ev e1) (ev e2) (ev e3))))
(meta-while (lambda (condition body) (while (ev condition) (ev body))))
;;;scheme.nw:6967
(meta-set   (lambda (v e)
              (let ([loc (find-variable v env)])
                 (if (null? loc)
                    (error (list2 'set-unbound-variable v))
                    (store loc (ev e))))))
;;;scheme.nw:6976
(meta-begin (lambda (es) (foldl (lambda (e result) (ev e)) '() es)))
;;;scheme.nw:6982
(meta-lambda (lambda (formals body)
               (if (all? symbol? formals)
                 (lambda (actuals)
                   ((eval (bindalloclist formals actuals env)) body))
                 (error (list2 'lambda-with-bad-formals: formals)))))
;;;scheme.nw:6875
                                                                             )
     ev))

;;;scheme.nw:7020
(define meta-val (env) 
  (lambda (x e)
    (if (symbol? x)
        (let* ([env (find-c x env (lambda (_) env) (lambda () (bindalloc x '() env)))])
          (begin
            ((eval env) (list3 'set x e))
            env))
        (error (list2 'val-tried-to-bind-non-symbol x)))))
;;;scheme.nw:7031
(define meta-define (env) 
  (lambda (name formals body)
    ((meta-val env) name (list3 'lambda formals body))))
;;;scheme.nw:7040
(define meta-exp (e env)
  (begin
    (println ((eval env) e))
    env))
;;;scheme.nw:6993
(define evaldef (e env)
  (if (pair? e)
    (let ([first (car e)]
          [rest  (cdr e)])
      (if (= first 'val)
        (binary 'val (meta-val env) rest)
        (if (= first 'define)
            (trinary 'define (meta-define env) rest)
            (meta-exp e env))))
    (meta-exp e env)))
;;;scheme.nw:7051
(define read-eval-print (env es)
    (foldl evaldef env es))
;;;scheme.nw:7060
(define run (es)
  (begin (read-eval-print (primenv) es) 0))
