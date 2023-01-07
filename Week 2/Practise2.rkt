#lang r5rs

;;sum of interval [a, b]
(define (for a b r)
  (if (< a b)
      (for (+ a 1) b (+ r a 1)) r))


(define (sum-interval a b)
  (for a b a))

;;count-digits - number of time d is found in n


(define (countfor d n i)
  (if (>= n 1)
      (if (= (modulo n 10) d) (countfor d (quotient n 10) (+ i 1)) (countfor d (quotient n 10) i)) i))


(define (count-digit d n)
  (countfor d n 0))

;;reverse-digits - reverses digits of a number


(define (reversefor n r i)
  (if (> n 0)
      (reversefor (quotient n 10) (+ (* r 10) (modulo n 10)) (+ i 1)) r))

(define (reverse-digits n)
  (reversefor n 0 0))

;;prime - checks for a prime number

(define (forprime n i )
  (if (<= n i) (forprime (+ n 1) i) #t)

(define (prime? n)
  (if (= n 1) #f
  (forprime 1 (sqrt n))))



