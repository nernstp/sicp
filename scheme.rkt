;1.8
(define (cr guess x)
  (if (ge? guess x)
      guess
      (cr (improve guess x) x)))

(define (ge? g x)
   (< (abs (- (/ (nm g x) g) 1)) 0.000000000000001))

(define (nm g x)
  (/ (+ (/ x (* g g)) (* 2 g)) 3))

(define (improve g x)
  (/ (+ g (nm g x)) 2))

;(cr 1.0 300000000000000000000000000000000000)



(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;1.11
(define (fff n)
  (cond ((< n 3) n)
        (else (+ (fff (- n 1)) (* 2 (fff (- n 2))) (* 3 (fff (- n 3)))))))

(define (fffi n)
  (define (cal x y z)
    (+ x (* 2 y) (* 3 z)))
  (define (iter a b c i)
    (cond ((< i n) (iter (cal a b c) a b (+ 1 i)))
          (else (cal a b c))))
  (iter 2 1 0 3))

;1.12

;1.16
(define (expo x y)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (eit b a p)
    (cond ((= a 0) 1)
          ((= a 1) (* x p))
          ((even? a) (eit (* b b) (/ a 2) (* p b)))
          (else (eit b (- a 1) (* p b)))))
  (eit x y 1))
                          
;1.17

(define (m a b)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (mi x y p)
    (cond ((= y 0) 0)
          ((= y 1) (+ a p))
          ((even? y) (mi (+ x x) (/ y 2) (+ p x)))
          (else (mi x (- y 1) (+ p x)))))
  (mi a b 0))
    

;l.19
(define (sq x) (* x x))
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
      (cond ((= count 0) b)
            ((even? count)
             (fib-iter a
                       b
                       (+ p (sq q) (sq p))
                       (+ q (sq q) (sq p))
                       (/ count 2)))
            (else (fib-iter (+ (* b q) (* a q) (* a p))
                            (+ (* b p) (* a q))
                            p
                            q
                            (- count 1)))))                                                        
;#| 1 0 0 1
;   1 1 0 1
;   2 1
;   3 2
;   5 3
;
;
;
;0 : 0
;1 : 1
;2 : 1
;3 : 2
;4 : 3
;5 : 5
;6 : 8 = 2^3
;7 : 13
;8 : 21 = 3 x 7
;9 : 34 = 2 x 17
;10 : 55 = 5 x 11
;11 : 89
;12 : 144 = 2^4 x 3^2
;13 : 233
;14 : 377 = 13 x 29
;15 : 610 = 2 x 5 x 61
;16 : 987 = 3 x 7 x 47
;17 : 1597
;18 : 2584 = 2^3 x 17 x 19
;19 : 4181 = 37 x 113
;20 : 6765 = 3 x 5 x 11 x 41
;21 : 10946 = 2 x 13 x 421
;22 : 17711 = 89 x 199
;23 : 28657
;24 : 46368 = 2^5 x 3^2 x 7 x 23
;25 : 75025 = 5^2 x 3001
;26 : 121393 = 233 x 521
;27 : 196418 = 2 x 17 x 53 x 109
;28 : 317811 = 3 x 13 x 29 x 281
;29 : 514229
;30 : 832040 = 2^3 x 5 x 11 x 31 x 61
;31 : 1346269 = 557 x 2417
;32 : 2178309 = 3 x 7 x 47 x 2207

;1.32a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))


;1.32b
(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner a result))))
  (iter 1 null-value))
    

;1.33
(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a result)
     (if (> a b)
         result
         (iter (next a) (combiner (if (predicate a)
                                      (term a)
                                      null-value)
                                  result))))
  (iter a null-value))

(define (prime? a)
  (define (iter i)
    (if (= i a)
        #t
        (if (or (= (modulo a i) 0) (= a 1))
            #f
            (iter (+ 1 i)))))
  (iter 2)
  )
;1.33a
(define (sumSqP a b)
  (filtered-accumulate (lambda (x y) (+ x y)) 0 (lambda (x) (* x x)) a (lambda (x) (+ x 1)) b prime?))

;1.33b


(define (proP b)
  (define (rP? x)
    (= (gcd x b) 1))
  (filtered-accumulate (lambda (x y) (* x y)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) b rP?))

;1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;1.36
(define (fp f fg)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try fg))

(define (golden2 x)
  (fp (lambda (x) (+ 1 (/ 1 x))) x))

(define (xx x)
  (fp (lambda (x) (/ (log 1000) (log x))) x))

;1.37a

(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ 1 i))))))
  (rec 1))

;1.37b
(define (cf n d k)
  (define (iter i r)
    (if (= i 0)
        r
        (iter (- i 1) (/ (n i)
                         (+ (d i) r)))))
  (iter k 0))
              
    
;1.38
(define dfc
  (+ 2
     (cont-frac (lambda (x) 1)
             (lambda (x) (if (= (modulo x 3) 2)
                             (+ 2 (* 2 (quotient x 3)))
                             1.0))
             1000)))

;1.39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
             x
             (* x x)))
  (define (d i)
    (- (* 2 i) 1))
  (define (h i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (- (d i) (h (+ i 1.0))))))
  (h 1))


(define (average x y)
  (/ (+ x y) 2))

(define (sqrts x)
  (fp (lambda (y) (average y (/ x y))) 1.0))

;1.40
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.000000001)
(define (cube x) (* x x x))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (square x)
  (* x x))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrtn x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))


(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;1.41
(define (double g)
  (lambda (x) (g (g x))))
(define (inc x)
  (+ 1 x))

;1.42
(define (compose f g)
  (lambda (x) (f (g x))))
;1.43
(define (repeated f n)
  (define (h i)
    (if (> i n)
        f
        (compose (h (+ n 1)) f)))
  (h 1))

;1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))
(define (nfold f n)
  (lambda (x) (repeated (smooth f) n) x))
;1.45
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (r-damp f n)
  (lambda (x) (repeated (lambda (x) average (f xz)) n) x))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
(define (nroot x n)
  (fixed-point (r-damp (lambda (y) (/ x (expt y (- n 1))))
                       n)
               1.0))