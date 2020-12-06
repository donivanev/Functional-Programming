#lang racket

(define (half-len lst) (quotient (length lst) 2))

(define (split-lists original-list)
  (define (helper original lst counter)
    (cond[(= counter (half-len original-list)) lst]
         [else                                 (cons (car original) (helper (cdr original) lst (+ counter 1)))]))
(helper original-list '() 0))

(define (split-lists-second original-list)
  (define (helper original lst counter)
    (cond[(= counter (length original-list))   lst]
         [(< counter (half-len original-list)) (helper (cdr original) lst (+ counter 1))]
         [else                                 (cons (car original) (helper (cdr original) lst (+ counter 1)))]))
(helper original-list '() 0))

(define (shuffle xs)
  (define (helper lst1 lst2 result)
    (cond[(null? lst1)       result]
         [else               (helper (cdr lst1) (cdr lst2) (append result (list (car lst1) (car lst2))))]))
(helper (split-lists xs) (split-lists-second xs) '()))

(shuffle '(2 5 1 3 4 7)) ; -> '(2 3 5 4 1 7)
(shuffle '(1 2 3 4 4 3 2 1)) ; -> '(1 4 2 3 3 2 4 1)
(shuffle '(1 1 2 2)) ; -> '(1 2 1 2)