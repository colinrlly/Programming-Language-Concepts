(class Unit Object () (method print ()) (method println ()))
(val unit (new Unit))

(class BlockFn1 Block () (method value (i) (mod: i 31)))
(class BlockFn2 Block () (method value (i) (mod: (* i i) 31)))
(class BlockFn3 Block () (method value (i) (reciprocal (+ i 1))))
(class BlockFn4 Block () (method value (i) (ifTrue:ifFalse: (= 0 (mod: i 2)) {(asFraction (+ i i))} {(reciprocal (+ i i))})))

(class XVectorTester Object
  (xv name size isMag isNum isInt isFrac isFloat vals)
  (class-method xv:name: (xv name) (init (xv:name: (new self) xv name)))
  (class-method xv:name:-no-init (xv name) (xv:name: (new self) xv name))
  (method xv:name: (thatXV thatName) (set xv thatXV) (set name thatName) self)
  (method init ()
    (set size (size xv))
    (set isMag (or: (= 0 size) {(isKindOf: (elem: xv 0) Magnitude)}))
    (set isNum (or: (= 0 size) {(isKindOf: (elem: xv 0) Number)}))
    (set isInt (or: (= 0 size) {(isKindOf: (elem: xv 0) Integer)}))
    (set isFrac (or: (= 0 size) {(isKindOf: (elem: xv 0) Fraction)}))
    (set isFloat (or: (= 0 size) {(isKindOf: (elem: xv 0) Float)}))
    (set vals (new List))
    (add: vals nil)
    (add: vals true)
    (add: vals false)
    (add: vals 0)
    (add: vals 1)
    (add: vals 2)
    (add: vals 42)
    self)
  (method test-at-ifAbsent () (locals tmp)
    (print 'Testing) (print space) (println name)
    (do: '(0 1 2 3 4 5) (block (i)
      (set tmp i)
      (print left-paren) (print 'at:ifAbsent:) (print space) (print name) (print space) (print tmp) (print space) (print left-curly) (print 'nil) (print right-curly) (print right-paren) (print space) (print '=>) (print space) (println (at:ifAbsent: xv tmp {nil}))))
    (do: '(3 30) (block (i)
      (set tmp (- (raisedToInteger: 2 i) 1))
      (print left-paren) (print 'at:ifAbsent:) (print space) (print name) (print space) (print tmp) (print space) (print left-curly) (print 'nil) (print right-curly) (print right-paren) (print space) (print '=>) (print space) (println (at:ifAbsent: xv tmp {nil}))
      (set tmp (raisedToInteger: 2 i))
      (print left-paren) (print 'at:ifAbsent:) (print space) (print name) (print space) (print tmp) (print space) (print left-curly) (print 'nil) (print right-curly) (print right-paren) (print space) (print '=>) (print space) (println (at:ifAbsent: xv tmp {nil}))
      (set tmp (+ (raisedToInteger: 2 i) 1))
      (print left-paren) (print 'at:ifAbsent:) (print space) (print name) (print space) (print tmp) (print space) (print left-curly) (print 'nil) (print right-curly) (print right-paren) (print space) (print '=>) (print space) (println (at:ifAbsent: xv tmp {nil}))))
    (do: '(30 3) (block (i)
      (set tmp (negated (+ (raisedToInteger: 2 i) 1)))
      (print left-paren) (print 'at:ifAbsent:) (print space) (print name) (print space) (print tmp) (print space) (print left-curly) (print 'nil) (print right-curly) (print right-paren) (print space) (print '=>) (print space) (println (at:ifAbsent: xv tmp {nil}))
      (set tmp (negated (raisedToInteger: 2 i)))
      (print left-paren) (print 'at:ifAbsent:) (print space) (print name) (print space) (print tmp) (print space) (print left-curly) (print 'nil) (print right-curly) (print right-paren) (print space) (print '=>) (print space) (println (at:ifAbsent: xv tmp {nil}))
      (set tmp (negated (- (raisedToInteger: 2 i) 1)))
      (print left-paren) (print 'at:ifAbsent:) (print space) (print name) (print space) (print tmp) (print space) (print left-curly) (print 'nil) (print right-curly) (print right-paren) (print space) (print '=>) (print space) (println (at:ifAbsent: xv tmp {nil}))))
    (do: '(5 4 3 2 1) (block (i)
      (set tmp (negated i))
      (print left-paren) (print 'at:ifAbsent:) (print space) (print name) (print space) (print tmp) (print space) (print left-curly) (print 'nil) (print right-curly) (print right-paren) (print space) (print '=>) (print space) (println (at:ifAbsent: xv tmp {nil}))))
    unit)
  (method test-do () (locals tmp)
    (print 'Testing) (print space) (println name)
    (ifTrue: (< size 20) {(print left-paren) (print 'print) (print space) (print name) (print right-paren) (print space) (print '=>) (print space) (println xv)})
    (ifTrue: (< size 4096) {
      (ifFalse: (| isFrac isFloat) {(do: vals (block (x)
        (print left-paren) (print 'includes:) (print space) (print name) (print space) (print x) (print right-paren) (print space) (print '=>) (print space) (println (includes: xv x))
        (print left-paren) (print 'occurrencesOf:) (print space) (print name) (print space) (print x) (print right-paren) (print space) (print '=>) (print space) (println (occurrencesOf: xv x))))})
      (ifTrue: isNum {
        (print left-paren) (print 'detect:ifNone:) (print space) (print name) (print space) (print 'negative) (print right-paren) (print space) (print '=>) (print space) (println (detect:ifNone: xv (block (x) (negative x)) {nil}))
        (print left-paren) (print 'detect:ifNone:) (print space) (print name) (print space) (print 'nonnegative) (print right-paren) (print space) (print '=>) (print space) (println (detect:ifNone: xv (block (x) (nonnegative x)) {nil}))
        (print left-paren) (print 'detect:ifNone:) (print space) (print name) (print space) (print 'strictlyPositive) (print right-paren) (print space) (print '=>) (print space) (println (detect:ifNone: xv (block (x) (strictlyPositive x)) {nil}))
        })})
     unit)
  (method test-sum ()
    (print 'Testing) (print space) (println name)
    (ifTrue: (& isNum (< size 256)) {(print left-paren) (print 'sum) (print space) (print name) (print right-paren) (print space) (print '=>) (print space) (println (sum xv))})
    unit)
  (method test-product ()
    (print 'Testing) (print space) (println name)
    (ifTrue: (& isNum (< size 15)) {(print left-paren) (print 'product) (print space) (print name) (print right-paren) (print space) (print '=>) (print space) (println (product xv))})
    unit)
  (method test-min ()
    (print 'Testing) (print space) (println name)
    (ifTrue: (& isMag (< 0 size)) {(print left-paren) (print 'min) (print space) (print name) (print right-paren) (print space) (print '=>) (print space) (println (min xv))})
    unit)
  (method test-max ()
    (print 'Testing) (print space) (println name)
    (ifTrue: (& isMag (< 0 size)) {(print left-paren) (print 'max) (print space) (print name) (print right-paren) (print space) (print '=>) (print space) (println (max xv))})
    unit)
  (method test-similar:: (othrXVs othrNs) (locals tmp xv' name' size' isMag' isNum' isInt' isFrac' isFloat')
    (print 'Testing) (print space) (println name)
    (do: vals (block (x)
      (print left-paren) (print 'similar:) (print space) (print name) (print space) (print x) (print right-paren) (print space) (print '=>) (print space) (println (similar: xv x))))
    (set tmp 0)
    (timesRepeat: (size othrXVs) {
      (set xv' (at: othrXVs tmp))
      (set name' (at: othrNs tmp))
      (set size' (size xv'))
      (set isMag' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Magnitude)}))
      (set isNum' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Number)}))
      (set isInt' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Integer)}))
      (set isFrac' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Fraction)}))
      (set isFloat' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Float)}))
      (ifTrue: (| (| (= 0 size) (= 0 size')) (& (eqv: isFrac isFrac') (eqv: isFloat isFloat'))) {
        (print left-paren) (print 'similar:) (print space) (print name) (print space) (print name') (print right-paren) (print space) (print '=>) (print space) (println (similar: xv xv'))})
      (set tmp (+ tmp 1))})
    unit)
  (method test-lt:: (othrXVs othrNs) (locals tmp xv' name' size' isMag' isNum' isInt' isFrac' isFloat')
    (print 'Testing) (print space) (println name)
    (set tmp 0)
    (timesRepeat: (size othrXVs) {
      (set xv' (at: othrXVs tmp))
      (set name' (at: othrNs tmp))
      (set size' (size xv'))
      (set isMag' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Magnitude)}))
      (set isNum' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Number)}))
      (set isInt' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Integer)}))
      (set isFrac' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Fraction)}))
      (set isFloat' (or: (= 0 size') {(isKindOf: (elem: xv' 0) Float)}))
      (ifTrue: (& (& isMag isMag') (| (| (= 0 size) (= 0 size')) (& (eqv: isInt isInt') (& (eqv: isFrac isFrac') (eqv: isFloat isFloat'))))) {
        (print left-paren) (print '<) (print space) (print name) (print space) (print name') (print right-paren) (print space) (print '=>) (print space) (println (< xv xv'))})
      (set tmp (+ tmp 1))})
    unit)
  (method test-fromIndex-toIndex () (locals fromIndices toIndices tmp)
    (set fromIndices '(0 0 0 0 1 1 -1 -2 -3 -5 -2 -4 -1 -2 -3 -4 -5))
    (set toIndices   '(0 1 2 4 1 3 -1 -1 -1 -1 -2 -2  0  1  2  3  4))
    (print 'Testing) (print space) (println name)
    (ifTrue: (> size 5) {
      (set tmp 0)
      (timesRepeat: (size fromIndices) {
        (print left-paren) (print 'fromIndex:toIndex:) (print space) (print name) (print space) (print (at: fromIndices tmp)) (print space) (print (at: toIndices tmp)) (print right-paren) (print space) (print '=>) (print space) (println (fromIndex:toIndex: xv (at: fromIndices tmp) (at: toIndices tmp)))
        (set tmp (+ tmp 1))})})
    unit)
  (method test-debug ()
    (print 'Testing) (print space) (println name)
    (print left-paren) (print 'debug) (print space) (print name) (print right-paren) (print space) (print '=>) (print space) (debug xv) (print newline)
    unit)
  (method test-size ()
    (print 'Testing) (print space) (println name)
    (print left-paren) (print 'size) (print space) (print name) (print right-paren) (print space) (print '=>) (print space) (println (size xv))
    unit)
  (method test-elem () (locals tmp)
    (print 'Testing) (print space) (println name)
    (do: '(0 1 2 3 4 5 6 7 8 9 10 15 16 17 18 19 20 51 52 53 54 55 101 102 103 104 105 501 502 503 504 505 1001 1002 1003 1004 1005) (block (i)
      (set tmp i)
      (ifTrue: (< i (size xv)) {(print left-paren) (print 'elem) (print space) (print name) (print space) (print tmp) (print right-paren) (print space) (print '=>) (print space) (println (elem: xv tmp))})))
    unit)
  (method test-all:: (othrXVs othrNs)
    (test-at-ifAbsent self)
    (test-do self)
    (test-sum self)
    (test-product self)
    (test-min self)
    (test-max self)
    (test-similar:: self othrXVs othrNs)
    (test-lt:: self othrXVs othrNs)
    (test-fromIndex-toIndex self)
    unit)
)

(val xvs unit)
(begin (set xvs (new List)) unit)
(val ns unit)
(begin (set ns (new List)) unit)

(val xv01 unit)
(begin (set xv01 (withArr: ArrayXVector '())) (add: xvs xv01) (add: ns 'xv01) unit)

(val xv02 unit)
(begin (set xv02 (withArr: ArrayXVector '(1))) (add: xvs xv02) (add: ns 'xv02) unit)

(val xv03 unit)
(begin (set xv03 (withArr: ArrayXVector '(1 2 3))) (add: xvs xv03) (add: ns 'xv03) unit)

(val xv04 unit)
(begin (set xv04 (withArr: ArrayXVector '(1 2 3 4 5 6))) (add: xvs xv04) (add: ns 'xv04) unit)

(val xv05 unit)
(begin (set xv05 (withArr: ArrayXVector '(-6))) (add: xvs xv05) (add: ns 'xv05) unit)

(val xv06 unit)
(begin (set xv06 (withArr: ArrayXVector '(-6 -5 -4))) (add: xvs xv06) (add: ns 'xv06) unit)

(val xv07 unit)
(begin (set xv07 (withArr: ArrayXVector '(-6 -5 -4 -3 -2 -1))) (add: xvs xv07) (add: ns 'xv07) unit)

(val xv08 unit)
(begin (set xv08 (withArr: ArrayXVector (from: Array (collect: (asSet '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) (new BlockFn1))))) (add: xvs xv08) (add: ns 'xv08) unit)

(val xv09 unit)
(begin (set xv09 (withArr: ArrayXVector (from: Array (collect: (asSet '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) (new BlockFn2))))) (add: xvs xv09) (add: ns 'xv09) unit)

(val xv10 unit)
(begin (set xv10 (withArr: ArrayXVector (from: Array (collect: (asSet '(1 2 3 4 5)) (new BlockFn3))))) (add: xvs xv10) (add: ns 'xv10) unit)

(val xv11 unit)
(begin (set xv11 (withArr: ArrayXVector (from: Array (collect: (asSet '(1 2 3 4 5 6 7 8 9 10)) (new BlockFn4))))) (add: xvs xv11) (add: ns 'xv11) unit)

(val xv12 unit)
(begin (set xv12 (withArr: ArrayXVector (from: Array (collect: (asSet '(1 2)) (block (i) (= i 1)))))) (add: xvs xv12) (add: ns 'xv12) unit)

(val xv13 unit)
(begin (set xv13 (withArr: ArrayXVector (from: Array (collect: (asSet '(1 2)) (block (i) nil))))) (add: xvs xv13) (add: ns 'xv13) unit)
