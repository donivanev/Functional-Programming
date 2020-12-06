#lang racket

;Още функции за работа със списъци
;1) (take xs n) - взима първите n елемента на списък
;2) (drop xs n) - премахва първите n елемента на списък
;3) (zip xs ys) - връща списък от двойки (Ai,Bi), където Ai е i-тия елемент на xs, Bi е i-тия елемент на ys

;Функции от по-висок ред за работа със списъци:
;1) (any? pred? xs) - проверява дали поне един елемент на списъка удовлетворява даден предикат
;2) (all? pred? x) - проверява дали всеки елемент на списъка удовлетворява даден предикат
;3) (map f xs) - прилага f към всеки елемент на xs
;4) (filter p xs) - премахва (филтрира) елементите, за които предикатът p е грешен
;5) (apply f xs) - връща оценката на f със аргументи - елементите на списъка 
;6) foldl, foldr - обхождане на списък с последователно прилагане на f
;7) (zipWith f xs ys) - връща списък с елементи функцията f приложена към елемент на xs и елемент на ys

(define (remove-duplicates xs)
  (if (null? xs) '() (cons (car xs) (remove-duplicates (filter (λ (x) (not (= x (car xs)))) (cdr xs))))))

(remove-duplicates '(1 2 2 3 5 5 5 7))

(define (sublist-between start end xs)
  (take (drop xs start) (+ 1 (- end start))))

(sublist-between 1 4 '(1 3 6 4 7))

(define (prefix-list lst lst2)
  (define (helper lst lst2 counter)
    (cond[(not (= (car lst) (car lst2))) #f]
         [(= counter (length lst))       #t]
         [else (helper (cdr lst) (cdr lst2) (+ counter 1))]))
(helper lst lst2 0))

(prefix-list '(1 2) '(1 2 3 4 5))

(define (count-occurrences subxs xs)
  (define (helper subxs xs counter)
    (cond[(null? xs)             counter]
         [(prefix-list subxs xs) (+ counter 1)]
         [else (helper subxs (cdr xs) counter)]))
(helper subxs xs 0))

(count-occurrences '(1 2) '(1 2 3 1 2 5 1 2))

(define (ordered? xs pred)
  (cond[(null? (cdr xs))               #t]
       [(pred (car xs) (car (cdr xs))) (ordered? (cdr xs) pred)]
       [else                           #f]))

(ordered? '(1 2 3) (λ (x y) (< x y)))

(define (max-ordered-sublist xs)
  (cond[(null? xs) '()]
       [(or (null? (cdr xs)) (>= (car xs) (cadr xs))) (list (car xs))]
       [else (cons (car xs) (max-ordered-sublist (cdr xs)))]))

(define (flatten xss)
  (cond [(null? xss) '()]
        [(list? (car xss)) (append (flatten (car xss)) (flatten (cdr xss)))]
        [else (cons (car xss) (flatten (cdr xss)))]))

(max-ordered-sublist '(1 4 3 4 5 6 2 9))

(flatten '(1 2 4 '(4 6 3) 6 '(7 7)))