#lang racket

(define (cartesian element lst)
  (if (null? lst) lst (append (list (cons element (car lst))) (cartesian element (cdr lst)))))

(define (cartesian-product xs ys)
  (if(null? xs) '() (append (cartesian (car xs) ys) (cartesian-product (cdr xs) ys))))

(cartesian-product '(1 2) '(3 4)); -> '((1.3) (1.4) (2.3) (2.4))
(cartesian-product '(1 2 3 4 5) '(6 7 8)); -> '((1.6) (1.7) (1.8) (2.6)
;(2.7) (2.8) (3.6) (3.7) (3.8) (4.6) (4.7) (4.8) (5.6) (5.7) (5.8))

(define (factorize n)
  (define (helper num counter)
    (cond[(or (= num 0) (= num 1))      '()]
         [(= (remainder num 2) 0)       (append (list 2) (helper (quotient num 2) counter))]
         [(= (remainder num 3) 0)       (append (list 3) (helper (quotient num 3) counter))]
         [(= (remainder num counter) 0) (append (list counter) (helper (quotient num counter) counter))]
         [else                          (helper num (+ counter 2))]))
(helper n 3))

(factorize 6); -> '(2 3)
(factorize 13); -> '(13)
(factorize 123); -> '(3 41)
(factorize 152); -> '(2 2 2 19)
(factorize 1050); -> '(2 3 5 5 7)
(factorize 134134); -> '(2 7 11 13 67)
