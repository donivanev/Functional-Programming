#lang racket

(define (list-length lst)
  (if (null? lst) 0 (+ 1 (list-length (cdr lst)))))

(list-length '(1 2 3))

(define (is-in-list lst element)
  (cond[(null? lst)           #f]
       [(= element (car lst)) #t]
       [else                  (is-in-list (cdr lst) element)]))

(is-in-list '(1 2 3) 7)

(define (add-element lst element position)
  (cond[(= position 0) (cons element lst)]
       [(null? lst)    (cons element lst)]
       [else           (cons (car lst) (add-element (cdr lst) element (- position 1)))]))

(add-element '(1 2 3 4) 8 3)

(define (find-min lst)
  (define (helper lst min)
    (cond[(null? lst)       min]
         [(< (car lst) min) (helper (cdr lst) (car lst))]
         [else              (helper (cdr lst) min)]))
(helper '(54 19 25) +inf.0))

(define (min-lst lst)
  (if (null? (cdr lst)) (car lst) (min (car lst) (min-lst (cdr lst)))))

(find-min '(1 2 3))

(define (erase-element lst element)
  (cond[(null? lst)           lst]
       [(= element (car lst)) (cdr lst)]
       [else  (cons (car lst) (erase-element (cdr lst) element))]))

(erase-element '(1 2 3) 2)

(define (concat lst lst2)
  (if (null? lst) lst2 (cons (car lst) (concat (cdr lst) lst2))))

(concat '(1 2 3) '(4 5 6))
  
(define (reverse-list list)
  (define (helper lst revLst)
    (if (null? lst) revLst (helper (cdr lst) (cons (car lst) revLst))))
(helper list '()))

(reverse-list '(1 2 3 4 5))