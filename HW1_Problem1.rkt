#lang racket

(define (square n) (* n n))

(define (automorphic? n)
  (define (helper n n2)
    (cond[(= n 0) #t]
         [(not(= (remainder n 10) (remainder n2 10))) #f]
         [else   (helper (quotient n 10) (quotient n2 10))]))
(helper n (square n)))

(automorphic? 5)
(automorphic? 25)
(automorphic? 36)
(automorphic? 890625)