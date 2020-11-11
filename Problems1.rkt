#lang racket

(define x 2)
(define y 8)

(define (mymin x y) (if(< x y) x y))
(mymin x y)

(define a 1)
(define b 9)
;(define (inside? x a b) (cond[((> x a) #t)][((< x b) #f)]))

(define (inside? x a b) (if(and (> x a)(< x b)) #t #f))
(inside? x a b)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (myfunc x y) (average(square x) (square y)))
(myfunc x y)

(define (myfib n) (if(<= n 2) 1
                     (+ (myfib(- n 1)) (myfib(- n 2)))))
(myfib 3)