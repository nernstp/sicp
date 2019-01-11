;2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (> d 0)
        (cons (/ n g) (/ d g))
        (cons (- (/ n g)) (- (/ d g))))))

        

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;2.2
(define (make-p x y)
  (cons x y))
(define (xp p)
  (car p))
(define (yp p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (xp p))
  (display ",")
  (display (yp p))
  (display ")"))
(define (make-s s e)
  (cons s e))

(define (starts s)
  (car s))
(define (end s)
  (cdr s))
(define (average a b)
  (/ (+ a b) 2))


(define (mid-point s)
  (make-p (average (xp (end s)) (xp (starts s)))
          (average (yp (end s)) (yp (starts s)))))


;2.3
(define (lenr s)
  (abs (- (xp (end s)) (xp (starts s)))))
(define (heigr s)
  (abs (- (yp (end s)) (yp (starts s)))))
(define (make-r t l)
  (cons t
        (cons (make-s (make-p (xp (starts t)) (- (yp (starts t)) (heigr l)))
                      (make-p (xp (end t)) (- (yp (end t)) (heigr l))))
              (cons l
                    (cons (make-s (make-p (+ (lenr t) (xp (starts l))) (yp (starts l)))
                                  (make-p (+ (lenr t) (xp (end l))) (yp (end l))))
                          '())))))
(define (top r)
  (car r))
(define (bot r)
  (cadr r))
(define (left r)
  (caddr r))
(define (right r)
  (cadddr r))

(define (perimeter r)
  (* 2 (+ (lenr (top r)) (heigr (left r)))))

(define (area r)
  (* (lenr (top r)) (heigr (left r))))
(define (ra x)
  (make-r (make-s (make-p 0 5)
                  (make-p 5 5))
          (make-s (make-p 0 0)
                  (make-p 0 5))))
(define (recp tl tr ll lr)
  (make-r (make-s tl tr)
          (make-s ll tl)))

;2.4
(define (conss x y)
  (lambda (m) (m x y)))
(define (cars z)
  (z (lambda (p q) p)))
(define (cdrs z)
  (z (lambda (p q) q)))


;2.5
(define (pp a b)
  (* (expt 2 a) (expt 3 b)))
(define (carpp p)
  (define (itera i r)
    (if (even? i)
        (itera (/ i 2) (+ r 1))
        r))
  (itera p 0))
(define (cdrpp p)
  (define (iterd i r)
    (if (= (remainder i 3) 0)
        (iterd (/ i 3) (+ r 1))
        r))
  (iterd p 0))

;2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
  (((add-1 zero)
    (lambda (x) (+ 1 x))) 0))

(define two
  (((add-1 (add-1 zero))
    (lambda (x) (+ 1 x))) 0))
;2.7
;(define (make-interval a b) (cons a b))
;(define (upper-bound x)
;2.17
(define (last-pair x)
  (if (null? (cdr x))
      (car x)
      (last-pair (cdr x))))

;2.18
(define (reverses x)
  (define (iter i r)
    (if (null? i)
        r
        (iter (cdr i) (cons (car i) r))))
  (iter x '()))
;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (no-more? x) (null? x))
(define (first-denomination x) (car x))
(define (except-first-denomination x) (cdr x))

;2.20
(define (same-parity x . y)
  (define (iter p i)
    (if (null? i)
        '()
        (if (p (car i))
             (cons (car i) (iter p (cdr i)))
             (iter p (cdr i)))))
  (if (even? x)
      (cons x (iter even? y))
      (cons x (iter odd? y))))

;2.21
(define (square x)
  (* x x))
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))
(define (sl items)
  (map square items))

;2.23
(define (for-each2 f x)
  (if (not (null? x))
      ((f x)n
       (for-each2 f (cdr x)))))

;2.25
(define l251 (list 1 3 (list 5 7) 9))
(define l252 (list (list 7)))
(define l253 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define l251a (car (cdr (car (cdr (cdr  l251))))))
(define l252a (car (car l252)))
(define l253a (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l253)))))))))))))

;2.26
(define x26 (list 1 2 3))
(define y26 (list 4 5 6))

;(1 2 3 4 5 6)
;((1 2 3 ) 4 5 6)
;((1 2 3) (4 5 6))

;2.27
(define drx (list (list 1 2) (list 3 4)))
(define (deep-reverse i)
  (define (iter x r)
    (if (null? x)
        r
        (if (list? (car x))
                   (iter (cdr x) (cons (deep-reverse (car x)) r))
                   (iter (cdr x) (cons (car x) r)))))
  (iter i '()))

;2.28

(define t28x (list (list 1 2) (list 3 4)))

(define (fringe x)
  (if (null? x)
      '()
      (if (list? (car x))
          (append (fringe (car x)) (fringe (cdr x)))
          (cons (car x) (fringe (cdr x))))))

;2.29a
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branchm m)
  (car m))
(define (right-branchm m)
  (cadr m))

(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cadr b))

;2.29b
(define (total-weight m)
  (+ (if (list? (branch-structure (left-branchm m)))
         (total-weight (branch-structure (left-branchm m)))
         (branch-structure (left-branchm m)))
     (if (list? (branch-structure (right-branchm m)))
         (total-weight (branch-structure (right-branchm m)))
         (branch-structure (right-branchm m)))))
(define test-m (make-mobile
                (make-branch 5
                             (make-mobile
                              (make-branch
                               5
                               (make-mobile (make-branch 1 2)
                                            (make-branch 2 1)))
                              (make-branch 3 5)))
                (make-branch 8
                             (make-mobile
                              (make-branch 3 2)
                              (make-branch 2 3)))))

(define test2-m (make-mobile
                 (make-branch 2 1)
                 (make-branch 1 2)))
;2.29c
;                         ------Mobile------
;                        /                  \
;                       L                   R
;                      / \                 / \
;                Length   Weight     Length  ----Mobile----- 
;                                           /               \
;                                          L                 R
;                                         / \               / \
;                                   Length   Weight   Length   Weight

(define (balanced? m)
  (let ((ls (branch-structure (left-branchm m)))
        (rs (branch-structure (right-branchm m)))
        (ll (branch-length (left-branchm m)))
        (rl (branch-length (right-branchm m))))
    
    (let ((lres (if (list? ls)
                    (if (balanced? ls)
                        (total-weight ls)
                        #f)
                    ls))
                
          (rres (if (list? rs)
                    (if (balanced? rs)
                        (total-weight rs)
                        #f)
                    rs)))
      (if (or (boolean? lres) (boolean? rres))
          #f
          (= (* ll lres) (* rl rres))))))
;2.30
(define (square-tree t)
  (cond ((null? t) '())
        ((list? t) (cons (square-tree (car t))
                         (square-tree (cdr t))))
        (else (* t t))))

;2.31
(define (tree-map f t)
  (cond ((null? t) '())
        ((list? t) (cons (tree-map f (car t))
                         (tree-map f (cdr t))))
        (else (f t))))

(define (sqt t) (tree-map square t))

;2.32
(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
          
;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (mapp p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (appendp seq1 seq2)
  (accumulate cons seq2 seq1))

(define (lengthp sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;2.34
(define (horner-eval x co-sq)
  (accumulate (lambda (this-co hi-term) (+ this-co (* x hi-term)))
              0
              co-sq))

;2.35
(define (count-leaves t)
  (accumulate +
              0
              (map
               (lambda (x)
                 (if (list? x)
                     (count-leaves x)
                     1))
               t)))

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))
(define (transpose mat)
  (accumulate-n cons (list) mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op last sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (op (car rest) (iter result (cdr rest)))))
  (iter last sequence))

;2.39

(define (reverser sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;2.40
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (unique-pairs n)
  (flatmap
   (lambda (x)
     (map
      (lambda (y)(cons y x))
      (enumerate-interval 1 (- x 1))))
   (enumerate-interval 1 n)))


;2.41
(define (triple-sum n s)
  (filter (lambda (x) (= s (+ (car x) (cadr x) (caddr x))))
          (flatmap (lambda (y) (cons (flatmap (lambda (z)
                                          (map (cons z y)
                                               (enumerate-internal 1 (- y 1))
                                               (enumerate-internal 1 n))))
                                     x)))))
(define (triple n)
  (flatmap (lambda (x) (map (lambda (q) (append (list q) x)) (flatmap (lambda (y) (map (lambda (z) (cons z y))
                                                                              (enumerate-interval 1 (- y 1))))
                                                             (enumerate-interval 1 (- x 1)))))
           (enumerate-interval 1 n)))



;2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens2 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


(define (row x)
  (car x))
(define (col x)
  (cdr x))
(define empty-board '())
(define (adjoin-position nr k roq)
  (append roq (list (cons nr k))))
(define (safe? k p)
  (if (= k 0)
      #t
      (and
       (not-in-row (get-elem k p)
                   (take (- k 1) p))
       (not-in-row (get-elem k p)
                   (zipWith (lambda (x y)
                              (cons (+ x (car y)) (cdr y)))
                             (reverse (enumerate-interval 1 (- k 1))) (take (- k 1) p)))
       (not-in-row (get-elem k p)
                   (zipWith (lambda (x y)
                              (cons (- (car y) x) (cdr y)))
                             (reverse (enumerate-interval 1 (- k 1))) (take (- k 1) p))))))
(define (not-in-row x l)
  (not (fold-left (lambda (y z) (or y z)) #f (map (lambda (y) (= y (row x))) (map row l)))))
(define (take k l)
  (if (or (= k 0) (null? l))
      (list)
      (cons (car l) (take (- k 1) (cdr l)))))

(define (get-elem k l)
  (if (= k 1)
      (car l)
      (get-elem (- k 1) (cdr l))))
(define (zipWith f a b)
  (if (null? a)
      b
      (if (null? b)
          a
          (cons (f (car a) (car b)) (zipWith f (cdr a) (cdr b))))))
  
;(queens 8)

(define (memqs item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memqs item (cdr x)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((exponentiation? exp)
         (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1))))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

              
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;2.56
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? e) (number? b)) (expt b e))
        (else (list '** b e))))
  
;2.57
;(define (

;2.59
(define empty-set '())
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (car set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;2.60
(define (element-of-set?r x set)
  (cond ((null? set) #f)
        ((equal? x (car set) #t))
        (else (element-of-set?r x (cdr set)))))
(define (adjoin-setr x set)
  (cons x set))
;(define (union-setr set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((element-of-set?r (car set1) set2))))
        
(define (intersection-setr set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-setr (cdr set1) set2)))
        (else (intersection-setr (cdr set1) set2))))

;2.61
(define (element-of-set?o x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set?o x (cdr set)))))


(define (intersection-seto set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-seto (cdr set1)
                                           (cdr set2))))
              ((< x1 x2)
               (intersection-seto (cdr set1) set2))
              ((< x2 x1)
               (intersection-seto set1 (cdr set2)))))))
(define (adjoin-seto x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-seto x (cdr set))))))

;2.62
(define (union-seto set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-seto (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-seto (cdr set1) set2)))
        (else (cons (car set2) (union-seto set1 (cdr set2))))))

;2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set?t x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set?t x (left-branch set)))
        ((> x (entry set))
         (element-of-set?t x (right-branch set)))))

(define (adjoin-sett x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-sett x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-sett x (right-branch set))))))
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;a
;2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
        
                                 
;2.65
(define (union-sett set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (entry set1) (entry set2))
         (make-tree (entry set1)
                    (union-sett (left-branch set1)
                                (left-branch set2))
                    (union-sett (right-branch set1)
                                (right-branch set2))))
        ((< (entry set1) (entry set2))
         (make-tree (entry set2)
                    (union-sett (make-tree (entry set1)
                                           (left-branch set1)
                                           '())
                                (left-branch set2))
                    (union-sett (right-branch set1)
                                (right-branch set2))))
        ((> (entry set1) (entry set2))
         (make-tree (entry set2)
                    (union-sett (left-branch set1)
                                (left-branch set2))
                    (union-sett (make-tree (entry set1)
                                           '()
                                           (right-branch set1))
                                (right-branch set2))))))

;(define (intersection-sett set1 set2)
;  (cond (((or (null? set1) (null? set2))) '())
;        ((= (entry set1) (entry set2))
;         (make-tree (entry set1)
;                    (intersection-sett (left-branch set1)
;                                       (left-branch set2))
;                    (intersection-sett (right-branch set1)
;                                       (right-branch set2))))
;        ((< (entry set1) (entry set2))
;         (if (element-of-set?t (entry set2) (right-branch set1)


(define (intersection-sett set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((t1 (tree->list-1 set1))
            (t2 (tree->list-1 set2)))
        (let ((io (intersection-set t1 t2)))
          (list->tree io)))))


;2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (lookupo gk sor)
  (cond ((null? sor) #f)
        ((equal? gk (key (car sor)))
         (car sor))
        ((> gk (key (car sor))) #f)
        (else (lookup gk (cdr sor)))))

;2.67
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
               
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (null? tree)
      '()
      (let ((lsym (symbols (left-branch tree)))
            (rsym (symbols (right-branch tree))))
        (cond ((element-of-set? symbol lsym)
               ((cons 0 (encode-symbol symbol (left-branch tree)))))
              ((element-of-set? symbol rsym)
               ((cons 1 (encode-symbol symbol (right-branch tree)))))
              (else (error "Symbol is not in the tree" symbol))))))
        