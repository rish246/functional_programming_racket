
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define sequence
  (lambda (low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride)))))

;; 2
(define string-append-map
  (lambda (xs suffix)
    (map (lambda (el)
         (string-append el suffix))
         xs)))

;; 3
(define list-nth-mod
  (lambda (xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error "list-nth-mod: empty list")]
          [#t (let* ([rem (remainder n (length xs))]
                    [tail (list-tail xs rem)])
                (car tail))])))

;; 4
(define stream-for-n-steps
  (lambda (s n)
    (cond [(< n 1) null]
          [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))])))

;; 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0)
                          (- x (* x 2))
                          x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
    
;; 6
(define dan-then-dog
  (letrec ([f (lambda (s)
              (let ([next (if (equal? s "dan") "dog" "dan")])
                (cons (string-append s ".jpg") (lambda () (f next)))))])
    (lambda () (f "dan"))))

;; 7
(define stream-add-zero
  (lambda (s)
    (letrec ([f (lambda (s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
      (lambda () (f s)))))
    
;; 8
(define cycle-lists
  (lambda (xs ys)
    (letrec ([f (lambda (xxs yys)
                  (let ([nxs (if (null? xxs) xs xxs)]
                        [nys (if (null? yys) ys yys)])
                  (cons (cons (car nxs) (car nys)) (lambda () (f (cdr nxs) (cdr nys))))))])
    (lambda () (f xs ys)))))
    
;; 9
(define vector-assoc
  (lambda (v vec)
    (letrec ([f (lambda (vec)
                 (cond [(equal? (vector-length vec) 0) #f]
                       [(equal? (pair? (vector-ref vec 0)) #f) (f (vector-drop vec 1))]
                       [(equal? (car (vector-ref vec 0)) v) (vector-ref vec 0)]
                       [#t (f (vector-drop vec 1))]))])
    (f vec))))

;; 10
(define cached-assoc
  (lambda (xs n)
    (let* ([cache (make-vector n #f)]
           [slot 0])
      (lambda (v)
        (let ([cached-result (vector-assoc v cache)])
          (cond [cached-result cached-result]
                [(assoc v xs)
                 (vector-set! cache slot (assoc v xs))
                 (if (< slot (- n 1)) (set! slot (+ slot 1)) (set! slot 0))
                 (assoc v xs)]
                [#t #f]))))))
              