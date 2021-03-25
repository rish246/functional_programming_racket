
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (i) (string-append i suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (if (negative? n)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([s-thunked (s)]) 
      (cons (car s-thunked) (stream-for-n-steps (cdr s-thunked) (- n 1))))))

;; Problem 5
(define funny-number-stream
  (letrec ([f (lambda(x) (cons (if (= (remainder x 5) 0)
                                  (- x)
                                  x)
                              (lambda() (f (+ x 1)))))])(lambda () (f 1))))

;; Problem 6
(define dan-then-dog
  (letrec ([f (lambda(x) (cons (if x
                                   "dan.jpg"
                                   "dog.jpg")
                               (lambda () (f (not x)))))])
                              (lambda () (f #t))))

;; Problem 7
(define (stream-add-zero s)
  (lambda()(cons (cons 0 (car (s))) (lambda () (stream-add-zero (cdr (s)))))))

;; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda(n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda() (f (+ n 1)))))])
           (lambda() (f 0))))
  
;; Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda(n) (cond [(>= n (vector-length vec)) #f]
                               [(and (pair? (vector-ref vec n)) (equal? (car  (vector-ref vec n)) v))  (vector-ref vec n)]
                               [#t (f (+ n 1))]))]) (f 0)))
;; Problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache_pos 0]
           [f (lambda(p)
              (letrec ([cache_check (vector-assoc p cache)])
                (if cache_check
                    cache_check
                    (letrec ([fetch (assoc p xs)])
                      (begin (vector-set! cache cache_pos fetch)
                             (set! cache_pos (remainder (+ cache_pos 1) n))
                             fetch)))))]) f))
                           

