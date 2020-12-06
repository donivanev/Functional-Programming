#lang racket

(define (count-digits n)
  (if(< n 10) 1 (+ 1 (count-digits(quotient n 10)))))

(count-digits 145)

(define (reverse-number-recursive n)
  (define (helper k count)
    (if (< k 10) k (+ (* (remainder k 10) (expt 10 (- count 1)))
        (helper (quotient k 10) (- count 1)))))
(helper n (count-digits n)))

(define (reverse-number n)
  (define (helper res k)
  (if(< k 10) (+ k (* res 10))
     (helper (+ (remainder k 10) (* res 10)) (quotient k 10))))
(helper 0 n))

(reverse-number 123)

(define (palindrome? n) (= n (reverse-number n)) )

(define (count-palindromes a b)
  (cond[(> a b) 0]
       [(palindrome? a) (+ 1 (count-palindromes (+ a 1) b))]
       [else                (count-palindromes (+ a 1) b)]))
(count-palindromes 1 100)

;при итеративен процес се изисква помощна функция
(define (count-divisors n)
  (define (helper count d)
    (cond[(> d n) count]
         [(= 0 (remainder n d)) (helper (+ 1 count) (+ d 1))]
         [else (helper count (+ d 1))]))
 (helper 0 1))

(count-divisors 6)

;6 28 496 8128

(define (perfect-number? n)
  (define (sum-of-divisors sum d)
    (cond [(= d n) sum]
          [(= 0 (remainder n d)) (sum-of-divisors (+ d sum) (+ d 1))]
          [else (sum-of-divisors sum (+ d 1))]))
(= n (sum-of-divisors 0 1)))

(perfect-number? 15)

;(define (inc-digits? n)
;  (define helper(prev curr)
;    (cond[(= n 0) #t]
;         [(< curr prev) #f]
;        [else (helper (remainder n 10) (remainder (quotient n 10) 10))])))

;(inc-digits? 12345)

(define (inc-digits? n)
  (or (< n 10) #t (and (< remainder (quotient n 10) 10) (remainder n 10))
     (inc-digits? (quotient n 10))))

(inc-digits? 123)
(inc-digits? 122)

;много операции, не е оптимално
;(define (sum-of-xn x n) (if(= n 0) 1 (+ (expt x n) (sum-of-xn x (- n 1)))))

(define (sum-of-xn x n)
  (define (helper prev i sum)
    (if(> i n) sum (helper (* prev x) (+ i 1) (+ sum prev))))
  (helper 1 0 0))

(sum-of-xn 2 10)