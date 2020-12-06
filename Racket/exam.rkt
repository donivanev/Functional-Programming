#lang racket

(define (reverse-number n)
  (define (helper res k)
    (if(< k 10) (+ k (* res 10)) (helper (+ (remainder k 10) (* res 10)) (quotient k 10))))
(helper 0 n))

(define (sum-of-powered-digits number power)
  (define (helper num sum pow)
    (cond[(= num 0)  sum]
         [(= sum 0) (helper (quotient num 10) (+ sum (expt (remainder num 10) pow)) pow)]
         [else (helper (quotient num 10) (+ sum (expt (remainder num 10) (+ pow 1))) (+ pow 1))]))
(helper (reverse-number number) 0 power))

(define (dig-pow n p)
  (if(= (remainder (sum-of-powered-digits n p) n) 0) (quotient (sum-of-powered-digits n p) n) -1))

;(dig-pow 89 1) ; -> 1 (81 + 92 = 89 = 89 * 1)
;(dig-pow 92 1) ; -> -1 (няма k – такова, че 91 + 22 = 92 * k)
;(dig-pow 695 2) ; -> 2 (62 + 93 + 54 = 1390 = 695 * 2)
;(dig-pow 46288 3) ; -> 51 (43 + 64 + 25 + 86 + 87 = 2360688 = 46288 * 51)

(define (kth-max-min xs k)
  (define (helper number lst counter)
    (cond[(> counter k) "No such number"]
         [(= counter k) (car lst)]
         [else          (helper number (cdr lst) (+ counter 1))]))
(helper k (sort xs <) 1))

;(λ (f) (λ (x) (f x)))
;(define (f g k) (λ (x) (g k)))
(kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0) 2)

;((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) ; -> -2
;((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; -> No such number

(define (half-len lst) (quotient (length lst) 2))

(define (split-lists original-list)
  (define (helper original lst counter)
    (cond[(= counter (half-len original-list)) lst]
         [else (cons (car original) (helper (cdr original) lst (+ counter 1)))]))
(helper original-list '() 0))

(define (split-lists-second original-list)
  (define (helper original lst counter)
    (cond[(= counter (length original-list)) lst]
         [(< counter (half-len original-list)) (helper (cdr original) lst (+ counter 1))]
         [else (cons (car original) (helper (cdr original) lst (+ counter 1)))]))
(helper original-list '() 0))

(define (shuffle xs)
  (define (helper lst1 lst2 result)
    (cond[(null? lst1)       result]
         [else               (helper (cdr lst1) (cdr lst2) (append result (list (car lst1) (car lst2))))]))
(helper (split-lists xs) (split-lists-second xs) '()))

;(shuffle '(2 5 1 3 4 7)) ; -> '(2 3 5 4 1 7)
;(shuffle '(1 2 3 4 4 3 2 1)) ; -> '(1 4 2 3 3 2 4 1)
;(shuffle '(1 1 2 2)) ; -> '(1 2 1 2)

(define (triangular? mat)
  (define (helper lst counter)
    (cond[(= (car (car mat)) 0)  #f]
         [else (not (= (car (car mat)) 0))  #t]))
(helper mat 0))

(triangular? '((1 2 3) (0 5 6) (0 0 9))) ; -> #t
(triangular? '((0 2 3) (0 0 6) (1 0 0))) ; -> #f
(triangular? '((1 2 3) (1 5 6) (0 0 9))) ; -> #f