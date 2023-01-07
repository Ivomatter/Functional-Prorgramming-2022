#lang racket

;;Accumulates

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

;;Task 1

(define (mod7 x) (modulo x 7))
(define (id x) x)
(define (1+ x) (+ x 1))


(define (argmin f a b)
  (define (minV x y)
    (if (< (f x) (f y)) x y))
  (accumulate-i minV a a b id 1+))

;;Task 2


;Фиксираме втория елемент на наредената двойка и итерираме само по първия, докато не стигнем b-1,
;тъй като числата, които се получават през интервала a->b са същите като тези при вариране
;и на двата елемента: 16 + 20 = 17 + 19 = 36.

(define (sumpair x)
  (+ (car x) (cdr x)))

(define (divCount x)
  (define (checkDiv currDivCount div)
    (if (= (remainder x div) 0)
        (1+ currDivCount) currDivCount))
  (accumulate-i checkDiv 0 1 x id 1+))

(define (best-pair a b)
  (define (maxDiv x y)
    (if (> (divCount (sumpair x)) (divCount (sumpair y))) x y))
  (define (pairb x)
    (cons x b))
  (accumulate-i maxDiv (pairb a) a (- b 1) pairb 1+))


;;Task 3

(define (integral a b y f dx)
  (define (calcfy x) (f x y))
(* dx (accumulate + 0 a b calcfy (lambda (x) (+ x dx)))))


(define (integrate2 f a b c d dx dy)
  (define (calcIntegral y)
    (* dy (integral a b y f dx)))
  (accumulate-i + 0 c d calcIntegral (lambda (y) (+ y dy))))


(define pi 3.14159265359)
(define (f x y) (+ x (sin y) 1))


;;Task 4

;;Използваме дефиницията от лекции

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))


(define (length l)   ;дефинираме length
  (define (add1 x y)
    (1+ x))
  (foldl add1 0 l))

(define (n-rooks l n)
  (define (rookcmp x y nv)
    (if (and nv (not (= (car x) (car y))) (not (= (cdr x) (cdr y)))) nv (1+ nv)))
  (define (occurences x)
    (foldl (lambda (nv elem) (rookcmp x elem nv)) 0 l))
  (and (= (length l) n)
  (foldl (lambda (nv elem) (and nv (= (occurences elem) 1))) #t l)))
    
  

(define board1 (list (cons 0 2) (cons 1 3) (cons 2 4) (cons 3 0) (cons 4 1)))
(define board2 (list (cons 0 2) (cons 1 3) (cons 2 4) (cons 3 2)))

        
;;МРАЗЯ RACKET


  
