#lang r5rs
;;Exercises from https://github.com/triffon/fp-2022-23/blob/main/exercises/cs1/ex00-20221005-tasks.md

;;Ex. 2 - Factorials
(define (factorial x)
  (* x (if (> x 1) (factorial (- x 1)) 1)))

;;Ex. 3 - Fibonacci numbers


(define (fib x)
  (define (1- x) (- x 1))
  (define (2- x) (- x 2))
  (if (<= x 1) x (+ (fib (1- x)) (fib (2- x)))))

;;Ex. 4 - Sum of numbers in interval [a,b]

(define (1+ x) (+ x 1))
(define (sum-interval x y)
  (if (< x y) (+ (sum-interval (1+ x) y) x) y))

;;Ex. 5 - Count the number of digits of n

(define (10/ x) (quotient x 10))
(define (count-digits n)
  (if (< n 1) 0 (+ 1 (count-digits (10/ n)))))

;;Ex. 6 - Reverse a digit

(define (reverse-digits n)
  (if (< n 10) n (+ (* (remainder n 10) (expt 10 (- (count-digits n) 1))) (reverse-digits (quotient n 10)))))

;;Ex. 7 - Check if a number is a palindrome

;We will split the program into main and helper functions

;Helper Functions

(define (firstNum n) (quotient n (expt 10 (- (count-digits n) 1))))

(define (lastNum n) (remainder n 10))

(define (nextIteration n)
  (quotient (remainder n (expt 10 (- (count-digits n) 1))) 10))

;Main function

(define (palindrome? n)
  (if (<= (count-digits n) 1) #t
      (if (= (firstNum n) (lastNum n)) (palindrome? (nextIteration n)) #f)))
  