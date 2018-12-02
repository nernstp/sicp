
;#lang sicp
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
      ((f x)
       (for-each2 f (cdr x)))))
      
;2.29a
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch m)
  (car m))
(define (right-branch m)
  (cadr m))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cadr b))
;2.29b
(define (total-weight m)
  (let ((ls (branch-structure (left-branch m)))
        (rs (branch-structure (right-branch m))))
    (+ (if (list? ls)
           (total-weight ls)
           ls)
       (if (list? rs)
           (total-weight rs)
           rs))))
(define test (make-mobile
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

(define test2 (make-mobile
               (make-branch 2 1)
               (make-branch 1 2)))
;2.29c
(define (balanced? m)
  (let ((ls (branch-structure (left-branch m)))
        (rs (branch-structure (right-branch m)))
        (ll (branch-length (left-branch m)))
        (rl (branch-length (right-branch m))))
    
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
(define (filter p x)
  (if (null? x)
      (list)
      (if (p (car x))
          (cons (car x) (filter p (cdr x)))
          (filter p (cdr x)))))



(define (triple-sum n s)
  (filter (lambda (i) (= s (+ (car i) (cadr i) (caddr i))))
          (flatmap (lambda (x)
                     (flatmap (lambda (y)
                                (map (lambda (z)
                                       (list z y x))
                                     (enumerate-interval 1 (- y 1))))
                              (enumerate-interval 1 (- x 1))))
                   (enumerate-interval 1 (- n 1)))))

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

