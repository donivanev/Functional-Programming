#lang racket

(display "Problem 1\n")

(define (square n) (* n n))

(define (automorphic? n)
  (define (helper n n2)
    (cond[(= n 0) #t]
         [(not(= (remainder n 10) (remainder n2 10))) #f]
         [else   (helper (quotient n 10) (quotient n2 10))]))
(helper n (square n)))

(automorphic? 5)      ; -> #t
(automorphic? 25)     ; -> #t
(automorphic? 36)     ; -> #f
(automorphic? 890625) ; -> #t

(display "________\n")
(display "\nProblem 2\n")

(define (cube n) (* n n n))

(define (prime? n)
  (define (helper d)
    (cond [(= d n)               #t]
          [(= 0 (remainder n d)) #f]
          [else                  (helper (+ d 1))]))
(if (= n 1) #f (helper 2)))

(define (nth-cuban n)
   (define (helper n2 n1 counter)
    (cond[(and (prime? (- (cube n2) (cube n1))) (not(= counter n))) (helper (+ n2 1) (+ n1 1) (+ counter 1))]
         [(and (prime? (- (cube n2) (cube n1))) (= counter n)) (- (cube n2) (cube n1))]
         [else (helper (+ n2 1) (+ n1 1) counter)]))
(helper 2 1 1))

(nth-cuban 1) ; -> 7 (2^3 - 1^3)
(nth-cuban 4) ; -> 61 (5^3 - 4^3)
;(nth-cuban 6) ; -> 127 (7^3 - 6^3)
;(nth-cuban 12) ; -> 1657 (...)
(nth-cuban 50) ; -> 55897 (137^3 - 136^3)
(nth-cuban 100) ; -> 283669 (308^3 - 307^3)