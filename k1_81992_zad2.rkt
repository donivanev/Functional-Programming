#lang racket

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst) (remove-duplicates (filter (λ (x) (not (= x (car lst)))) (cdr lst))))))

(define (kth-max-min xs k)
  (define (helper number lst counter)
    (cond[(or (> counter k) (>= (car lst) 0)) "No such number"]
         [(= counter k)                       (car lst)]
         [else                                (helper number (cdr lst) (+ counter 1))]))
(helper k (remove-duplicates (sort xs <)) 1))

;(λ (f) (λ (x) (f x)))
;(define (f g k) (λ (x) (g k)))

(kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0) 2)
(kth-max-min '(-1 0 -1 0 -2 3 1 -1) 3)

;((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) ; -> -2
;((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; -> No such number