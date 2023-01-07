#lang r5rs
(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))


;;definition of sum and example uses
(define (sum a b term next)
  (if (> a b) 0 (+ (term a) (sum (next a) b term next))))



(define (square x) (* x x))
(define (1+ x) (+ x 1))
(define (sum1 k) (sum k 100 square 1+))



(define (sum2 a b f dx)
  (define (term x) (* dx (f x)))
  (define (next x) (+ x dx))
  (sum a b term next))

;;definition of prod

(define (prod a b term next)
  (if (> a b) 0 (* (term a) (prod (next a) b term next))))

;;definition of accumulate
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))


(define (sumacc a b term next) (accumulate + 0 a b term next))