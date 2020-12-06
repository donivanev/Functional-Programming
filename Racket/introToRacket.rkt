#lang racket

1
(* 5 5)
(define A 5)
(define (square x)(* x x))
(square A)

(= 2 (+ 1 1))

(define x 3)
(define y 7)
(define (min x y)(if (< x y) x y))

(define (f x y)
  (cond [(< x 10) (* 2 x)]
        [(< x 20) y]
        [else (* 2 y)]))

(define (min2 x y)
  (if(< x y)
     x
     y))

(define (average x y)(/ (+ (square x) (square y)) 2))
(average x y)

(define (factorial n)
  (if(= n 0)
     1
     (* n (factorial(- n 1)))))
(factorial 5)


