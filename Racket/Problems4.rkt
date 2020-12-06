#lang racket

(define (suffix? a b)
  (cond [(< a 10)                                    (= a (remainder b 10))]
        [(not (= (remainder a 10) (remainder b 10))) #f]
        [else                                        (suffix? (quotient a 10) (quotient b 10))]))

(define (suffix-1? a b)
  (or (and (< a 10)
           (= a (remainder b 10)))
      (and (= (remainder a 10) (remainder b 10))
           (suffix-1? (quotient a 10) (quotient b 10)))))

(define (substr? a b)
  (and (<= a b)
       (or (suffix? a b)
           (substr? a (quotient b 10)))))

(define next (λ (x) (+ x 4)))

(define (my-identity x) x)
(define (my-compose f g) (λ (x) (f (g x))))
(define (my-negate p?)   (λ (x) (not (p? x))))
(define (my-negate-c p?) (my-compose not p?))
(define (my-curry f a)   (λ (b c) (f a b c)))

(define (difference F a b) (- (F b) (F a)))
(difference (lambda (x) (+ x 2)) 1 4)

(define f (λ (x) (* 2 x)))

(define (derive f eps) (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))
(define (g x) (* 2 x x))
(define g-p (derive g 0.001))
(g-p 1)