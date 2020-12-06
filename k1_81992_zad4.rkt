#lang racket

(define (triangular? mat)
  (define (helper lst counter)
    (cond[(= (car (car mat)) 0)        #f]
         [(not (= (car (car mat)) 0))  #t]
         [else                         (helper (cdr lst) counter)]))
(helper mat 0))

(triangular? '((1 2 3) (0 5 6) (0 0 9))) ; -> #t
(triangular? '((0 2 3) (0 0 6) (1 0 0))) ; -> #f
(triangular? '((1 2 3) (1 5 6) (0 0 9))) ; -> #f