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
