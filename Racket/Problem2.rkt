#lang racket

(define (myfib n) (if(<= n 2) 1
                     (+ (myfib(- n 1)) (myfib(- n 2)))))

(define (myfib-iter n) (helper 1 1 1 n))

(define (helper prev curr i n)
  (if(>= i n)
     curr
     (helper curr (+ prev curr) (+ i 1) n)))

(myfib 50)
; итеративен процес = опашкова рекурсия

(define x 5)
(define y 10)
(define (mygcd x y)
  (cond[(= x y) x]
       [(< x y) (mygcd x (- y x))]
       [else    (mygcd y (- x y))]))

(mygcd x y)

(define (mymaxdivisor x) 
  (define (helper1 d x)
    (if (= (remainder x d) 0)
        d
        (helper1(- d 1) x)))
;(helper1(- d 1))))
(helper1 (- x 1) x))
;(helper1 (- x 1)))

(mymaxdivisor 21)

(define a 1)
(define b 10)
(define (sum-odds a b)
  (cond [(> a b) 0]
       [(= (remainder a 2) 1) (+ a (sum-odds (+ a 2) b))]
       [else                  (sum-odds (+ a 1) b)]))

(sum-odds a b)

(define (sum-odds-iter a b)
  (define (helper3 sum a)
    (if (> a b)
        sum
        (helper3 (+ a sum) (+ a 2))))
  (if (= (remainder a 2) 1)
      (helper3 0 a)
      (helper3 0 (+ a 1))))
;с какво число се започва пресмятането

(sum-odds-iter a b)

(define (prime? n)
  (define (helper d)
    (cond [(= d n)               #t]
          [(= 0 (remainder n d)) #f]
          [else                  (helper (+ d 1))]))
  (if (= n 1) #f (helper 2)))

(prime? 4)