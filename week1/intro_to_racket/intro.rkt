; this is a comment and it will always will be a comment

#lang racket

(provide (all-defined-out))

(define x 5) ; (define is like val binding)

(define max_of_two
  (lambda (x y)
    (cond [(< x y) y]
          [#t x])))

; append
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

; my-map
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))))

; syntax of lambda

; max of two numbers
(define max-two
  (lambda (x y)
    (if (< x y)
        y
        x)))

; find sum of a list
(define (sum xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum (cdr xs)))]
        [(list? (car xs))  (+ (sum (car xs)) (sum (cdr xs)))]
        [#t (sum (cdr xs))]))


; find the maximum value in a list

(define (max-list xs)
  (cond [(null? xs) (error "No elements found in the list")]
        [(number? (car xs))
                   (if (> (car xs) (max-list (cdr xs)))
                       (car xs)
                       (max-list (cdr xs)))]))
; amazing programming language... easy to see the whole abstract syntax tree

; filter
(define (my-filter f xs)
  (cond [(null? xs) null]
        [(f (car xs)) (cons (car xs) (my-filter f (cdr xs)))]
        [#t  (my-filter f (cdr xs))]))
;;;; Very Nice programming language

; better list-max
(define (max-list-2 xs)
  (cond [(null? xs) (error "Empty list found")]
        [(null? (cdr xs)) (car xs)]
        [(number? (car xs))
                 (let ([max-rem-list (max-list-2 (cdr xs))])
                      (if (< (car xs) max-rem-list)
                          max-rem-list
                          (car xs)))]))

;; Lets just assume
;; Can't shadow more than once in a file... good rule

(define (my-if e1 e2 e3) (if e1 e2 e3))


;;; Lets look at the redux thunk function and how it uses the lambda expressions
; Binary Search Program in Racket
(define (linear-search xs key)
  (cond [(null? xs) #f]
        [#t (or (= (car xs) key) (linear-search (cdr xs) key))]))
      



