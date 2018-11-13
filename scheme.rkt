#|
1.1
10
12
8
3
6
-
-
19
#f
4
16
6
16

|#



;1.2
#|
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
|#
;1.3

(define (sq-of-sum a b)
  (+ (* a a) (* b b)))
(define (t3 x y z)
  (if (< x y)
      (if (< z x)
          (sq-of-sum x y)
          (sq-of-sum z y))
      (if (< z y)
          (sq-of-sum x y)
          (sq-of-sum z x))))
          
;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;1.6
(define (sqrt-iter guess x)
  (if (ge? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (ge? guess x)
  (< (abs (- (/ (square guess) x) 1)) 0.000000000000001))
  


(define (square x)
  (* x x))



(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
          
(define (nsqi guess x)
  (new-if (good-enough? guess x)
          guess
          (nsqi (improve guess x) x)))

;(sqrt-iter 1.0 0.000000000000001)


;1.9




;(define (+ a b)
;  (if (= a 0) b (inc (+ (dec a) b))))

;(+ 4 5)
;(inc (+ (dec 4) 5))
;(inc (+ 3 5))
;(inc (inc (+ (dec 3) 5)))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ (dec 2) 5))))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ (dec 1) 5)))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5)



;(define (+ a b)
;  (if (= a 0) b (+ (dec a) (inc b))))
;(+ 4 5)
;(+ (dec 4) (inc 5))
;(+ 3 6)
;(+ (dec 3) (inc 6))
;(+ 2 7)
;(+ (dec 2) (inc 7))
;(+ 1 8)
;(+ (dec 1) (inc 8))
;(+ 0 9)
;9



;1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; f = 2*n
; g = 2^n
; h = 2^^^n


;1.11

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
(cond ((= amount 0) 1)
((or (< amount 0) (= kinds-of-coins 0)) 0)
(else (+ (cc amount
(- kinds-of-coins 1))
(cc (- amount
(first-denomination
kinds-of-coins))
kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
(cond ((= kinds-of-coins 1) 1)
((= kinds-of-coins 2) 5)
((= kinds-of-coins 3) 10)
((= kinds-of-coins 4) 25)
((= kinds-of-coins 5) 50)))


;1.12
#|(define (pt n)
  (define (ptr x y n)
    (cond (<= y n)
          (
           (cond ((= x y) 1)
                 ((= x 1) (string-append "1" (ptr (+ 1 x) y)))
                                         
|#
;1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;a 5

;b n

;1.16


(define (expo b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (expoit x y p)
    (cond ((= 0 y) 1)
          ((= 1 y) (* b p))
          ((even? y) (expoit (* x x) (/ y 2) (* p x)))
          (else (expoit x (- y 1) (* p x)))))
  (expoit b n 1))



;1.19

(define (sq x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ p (* q q))
                   (+ q (* p p))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p)); a <- bq+aq+ap
                        (+ (* b p) (* a q))        ; b <- bp+aq
                        p
                        q
                        (- count 1)))))

;1.20

#|(define (gcd a b)
  (cond ((= (remainder a b) 0) b)
        (else (gcd b (remainder a b)))))

(define (gcd a b)
(cond ((= remainder a b) 0) b)
(else (gcd b (remainder a b)))))
|#
#|
gcd 206 40
gcd 40 6
gcd 6 4
gcd 4 2
gcd 2 0



|#

;1.21

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(#%require (only racket/base random))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1))))) 

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


;1.22
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n)
  (define (sfp start count)
    (if (> count 0)
         (cond ((prime? start)
                (timed-prime-test start)
                ;(display start)
                (newline)
                (sfp (+ start 1) (- count 1)))
               (else (sfp (+ start 1) count)))))
  (sfp n 3))
 

;1.23
(define (next n)
  (cond ((even? n) (+ 1 n))
        (else (+ 2 n))))
;1.24
;
;(define (timed-prime-test2 


;1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))


;1.26
(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod2 base (/ exp 2) m)
                       (expmod2 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base
                       (expmod2 base (- exp 1) m))
                    m))))


;1.27


;1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
  (/
   (* h
      (+ (f a)
         (f (+ a (* n h)))
         (sum (lambda (x) (* (+ 2 (* 2 (modulo x 2))) (f (+ a (* x h)))))
              1
              (lambda (x) (+ x 1))
              (- n 1))
         )
      )
   3)))
    


;1.30
(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter 0 0))
;1.31a

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter 1 1))

(define (piapp)
  (product (lambda (x) (if (even? x)
                           (/ (+ 2.0 x)
                              (+ 1 x))
                           (/ (+ 1 x)
                              (+ 2 x))))
           1 (lambda (x) (+ 1 x)) 20000000))

;1.31b

(define (product2 term a next b)
  (if (> a b)
      1
      (* (term a)
         (product2 term (next a) next b ))))

;1.3.3
(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

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
        (compose (h (+ i 1)) f)))
  (h 1))

;1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))
(define (nfold f n)
  (lambda (x) (((repeated smooth n) f) x)))
;1.45
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (r-damp f n)
  (lambda (x) (((repeated average-damp n) f) x)))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
(define (nroot x n)
  (fixed-point (r-damp (lambda (y) (/ x (expt y (- n 1))))
                       n)
               1.0))

;1.46
(define (iterative-improve good? improve)
  (define (ii x)
    (if (good? x (improve x))
        (improve x)
        (ii (improve x))))
  (lambda (x)
    (ii x)))
(define (sqrtii x)
  (exact->inexact ((iterative-improve (lambda (a b)
                        (if (<
                             (abs
                              (- a b))
                             tolerance)
                            #t
                            #f))
                      (lambda (y)
                        (average y (/ x y)))) x)))







