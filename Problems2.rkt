#lang racket

;(define (myfib n))


(define x 5)
(define y 10)
(define (mygcd x y) (if(= (remainder x y) 0) x y))

(mygcd x y)