#lang racket

(define (derive f eps) (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))
(define (g x) (* 2 x x))

(define (derive2 f eps) (derive (derive f eps) eps))
(define g-s (derive2 g 0.001))
(g-s 2)

(define (derive-n f n eps)
  (define (helper g counter)
    (cond [(= counter n) g]
          [else          (helper (derive g eps) (+ counter 1))]))
  (helper f 0))
(define g-n (derive-n g 3 0.001))
(g-n 4)

(define (repeated f n)
  (define (helper f n x)
    (if (= n 0)
        x
        (f (helper f (- n 1) x))))
  (λ (x) (helper f n x)))
((repeated (λ (x) (+ x 1)) 5) 1)

(define (repeated-1 f n)
  (λ (x)
    (if (= n 0)
        x
        (f ((repeated-1 f (- n 1)) x)))))
((repeated-1 (λ (x) (+ x 1)) 5) 1)

(define (my-compose f g) (λ (x) (f (g x))))

(define (repeated-c f n)
  (if (= n 1)
      f
      (my-compose f (repeated-c f (- n 1)))))
((repeated-c (λ (x) (+ x 1)) 5) 1)

;(define (derive-x f eps)
;  )

(define (sum-digit-divisors n)
  (define (helper k)
    (cond [(= k 0)                              0]
          [(= (remainder k 10) 0)               (helper (quotient k 10))]
          [(= 0 (remainder n (remainder k 10))) (+ (remainder k 10) (helper (quotient k 10)))]
          [else                                 (helper (quotient k 10))]))
(helper n))

(define (same-sum a b)
  (define (helper-m m)
    (define (helper-n n)
      (cond[(> n b)                                           0]
           [(= (sum-digit-divisors m) (sum-digit-divisors n)) (+ 1 (helper-n (+ n 1)))]
           [else                                              (helper-n (+ n 1))]))
(if (= m b) 0 (+ helper-n (+ m 1)) (helper-m (+ m 1)))))
(helper-m a))