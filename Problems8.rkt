#lang racket
(define (f l)
   (cond [(null? l) '()]
         [(not (pair? (car l))) (cons (car l) (f (cdr l)))]
         [else (append (f (car l)) (f (cdr l)))]))
;(f '((9 (8 (7))) 6 ((5 (4)) 3)))

;(map (lambda (x) (list 1 x)) (list 9 8 7))
;(map (lambda (x) (* x x)) ((lambda (y) (append '(9 8 7) y)) '(3 4)))

(define (atom? x) (not (pair? x)))

(define (deep-delete xss)
  (define (helper xss depth)
    (define (p? x)
      (and (atom? x) (< x depth))) 
    (cond [(null? xss)       '()]
          [(list? (car xss)) (cons (helper (car xss) (+ depth 1)) (helper (cdr xss) depth))]
          [(p? (car xss))    (helper (cdr xss) depth)]
          [else              (cons (car xss) (helper (cdr xss) depth))]))
  (helper xss 1))

;(deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))); -> (1 (2 (4)) (3 ())))

(define (len lst) (if(null? lst) 0 (+ 1 (len (cdr lst)))))
  
(define (has-matching-lengths l1 l2)
  (if(= (len l1) (len l2)) #t #f))

(define (hasMatchingLengths l1 l2)
  (define (helper diff l1 l2)
    (cond [(null? (cdr l1)) #t]
          [(= diff (- (length (cadr l1)) (length (cadr l2)))) (helper diff (cdr l1) (cdr l2))]
          [else             #f]))
(helper (- (length (car l1)) (length (car l2))) l1 l2))

(has-matching-lengths '((3 4) (5 6 7) (1)) '((4 5 3) (9 8 2) (3)))