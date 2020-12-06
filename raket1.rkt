#lang racket

;Problem 1

(define (len number)
  (define (helper num counter)
    (if(= num 0) counter (helper (quotient num 10) (+ counter 1))))
(helper number 0))

(define (desc-order number) 
  (define (helper num curr prev counter)
    (cond[(< prev curr)                  #f]
         [(= counter (- (len number) 1)) #t]
         [else (helper (quotient num 10) (remainder num 10) (remainder (quotient num 10) 10) (+ counter 1))]))
(helper number (remainder number 10) (remainder (quotient number 10) 10) 0))

(define (sum-numbers a b)
  (cond[(> a b)        0]
       [(desc-order a) (+ a (sum-numbers (+ a 1) b))]
       [else           (sum-numbers (+ a 1) b)]))

;(sum-numbers 1 9)
;(sum-numbers 199 203)
;(sum-numbers 219 225)

;Problem 2

;(define (bigger-numbers lst num counter)
;  (cond[(null? lst) counter]
;       [(> (car lst) num) (bigger-numbers (cdr lst) num (+ 1 counter))]
;       [else (bigger-numbers (cdr lst) num counter)]))

;(define (num-bigger-elements lst)
;  (define (helper n remLst result)
;   (cond[(null? remLst) result]
;        [else (helper (cons (car remLst) (bigger-numbers lst (car remLst) 0)) (cdr remLst) (cons result n))]))
;(helper (car lst) lst '()))

(define (num-bigger-elements lst)
  (define (makePair x)
    (cons x (length (filter (λ (y) (> y x)) lst))))
(map makePair lst))

;(num-bigger-elements '(5 6 3 4)); → '((5 1) (6 0) (3 3) (4 2))
;(num-bigger-elements '(1 1 1)); → '((1 0) (1 0) (1 0))

;Problem 3

(define (switchsum f g n)
  (define (helper count prev result)
    (cond[(>= count n)              result]
         [(= (remainder count 2) 0) (helper (+ count 1) (g prev) (+ result prev))]
         [else                      (helper (+ count 1) (f prev) (+ result prev))]))
(λ (x) (helper 0 (f x) 0)))

;((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 1) 2); → 3

;Problem 4

(define (repeater str)
  (λ (count glue)
    (if (<= count 1) str
        (string-append str glue ((repeater str) (- count 1) glue)))))

;((repeater "I love Racket") 3 " ")

;Problem 5

(define (sum-of-digits num)
  (if(= num 0) 0 (+ (remainder num 10) (sum-of-digits (quotient num 10)))))

(define (sum-sum-digit a b k)
  (define (helper a b k sum)
    (cond[(>= a b)                               sum]
         [(= (remainder (sum-of-digits a) k) 0) (helper (+ a 1) b k (+ sum a))]
         [else                                  (helper (+ a 1) b k sum)]))
(helper a b k 0))

;(sum-sum-digit 10 20 5)

;Problem 6

(define (orderedPrefix xs)
  (cond [(null? xs)              '()]
        [(null? (cdr xs))        xs]
        [(<= (car xs) (cadr xs)) (cons (car xs) (orderedPrefix (cdr xs)))]
        [else                    (list (car xs))]))

(define (max-ordered-sublist xs)
  (define (helper max curr)
    (define currPrefix (orderedPrefix curr))
      (cond [(null? curr)                         max]
            [(< (length max) (length currPrefix)) (helper currPrefix (cdr curr))]
            [else                                 (helper max (cdr curr))]))
(helper '() xs))

;(max-ordered-sublist '(1 5 2 4 6 8 3 4 1)); → '(2 4 6 8)

;Problem 7

(define (where list-elements list-predicates)
  (if (or (null? list-predicates) (null? list-elements)) list-elements
      (where (filter (car list-predicates) list-elements) (cdr list-predicates))))

;(where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))); → (6 8 10)

(define (set-union-short xs ys) (sort (remove-duplicates (append xs ys)) <))

(define (set-union xs ys)
  (define (helper xs ys lst)
    (cond [(null? xs)            (append lst ys)]
          [(null? ys)            (append lst xs)]
          [(< (car xs) (car ys)) (helper (cdr xs) ys       (append lst (list (car xs))))]
          [(> (car xs) (car ys)) (helper xs       (cdr ys) (append lst (list (car ys))))]
          [else                  (helper (cdr xs) (cdr ys) (append lst (list (car xs))))]))
(helper xs ys '()))

;(set-union '(1 3 5 7) '(5 7 13)); → '(1 3 5 7 13)
;set-union '(5 7 13) '(1 3 5 7)); → '(1 3 5 7 13)

(define (is-in-list element lst)
  (cond[(null? lst)           #f]
       [(= element (car lst)) #t]
       [else (is-in-list element (cdr lst))]))

(define (count-occurences element lst)
  (define (helper l counter)
    (cond[(null? l)           counter]
         [(= element (car l)) (helper (cdr l) (+ counter 1))]
         [else                (helper (cdr l) counter)]))
(helper lst 0))

;(count-occurences 1 '(1 2 3 5 1 7 6 1 8 8 1))

(define (prime? n)
  (define (helper d)
    (cond [(= d n)               #t]
          [(= 0 (remainder n d)) #f]
          [else                  (helper (+ d 1))]))
(if (= n 1) #f (helper 2)))

(define (suffix? a b)
  (cond [(< a 10)                                    (= a (remainder b 10))]
        [(not (= (remainder a 10) (remainder b 10))) #f]
        [else                                        (suffix? (quotient a 10) (quotient b 10))]))

(define (substr? a b)
  (and (<= a b) (or (suffix? a b) (substr? a (quotient b 10)))))