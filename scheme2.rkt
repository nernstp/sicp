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
      ((f x)
       (for-each2 f (cdr x)))))