#lang sicp
(#%require sicp-pict)

;(paint einstein)

;2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;2.45
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
     (below bottom top))))

(define (split f s)
  (lambda (p)
    (f p (s p p))))
;(define right-splits (split beside below))
;(define up-splits (split below beside))
