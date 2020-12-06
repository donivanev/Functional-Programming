#lang racket

(define (reverse-number n)
  (define (helper r k)
    (if(< k 10) (+ k (* r 10)) (helper (+ (remainder k 10) (* r 10)) (quotient k 10))))
(helper 0 n))

(define (sum-of-powered-digits number power)
  (define (helper num sum pow)
    (cond[(= num 0)  sum]
         [(= sum 0) (helper (quotient num 10) (+ sum (expt (remainder num 10) pow)) pow)]
         [else (helper (quotient num 10) (+ sum (expt (remainder num 10) (+ pow 1))) (+ pow 1))]))
(helper (reverse-number number) 0 power))

(define (dig-pow n p)
  (if(= (remainder (sum-of-powered-digits n p) n) 0) (quotient (sum-of-powered-digits n p) n) -1))

(dig-pow 89 1) ; -> 1 (81 + 92 = 89 = 89 * 1)
(dig-pow 92 1) ; -> -1 (няма k – такова, че 91 + 22 = 92 * k)
(dig-pow 695 2) ; -> 2 (62 + 93 + 54 = 1390 = 695 * 2)
(dig-pow 46288 3) ; -> 51 (43 + 64 + 25 + 86 + 87 = 2360688 = 46288 * 51)