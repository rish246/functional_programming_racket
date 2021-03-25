
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
; Problem 1 : Sequence generation function
(define (sequence lo hi stride)
  (cond [(> lo hi) null]
        [(> (+ lo stride) hi) (cons lo null)]
        [#t (cons lo (sequence (+ lo stride) hi stride))]))
      
; Problem 2 : string-append-map
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; list n-th mod
(define (list-nth-mod xs n)
  (cond [(= (length xs) 0) (error "list-nth-mod: empty list")]
        [(< n 0) (error "list-nth-mod: negative number")]
        [#t  (let
                 ([i (remainder n (length xs))])
                (car (list-tail xs i)))]))
           
           

; stream for n-steps
(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (cons
       (car (stream))
       (stream-for-n-steps (cdr (stream)) (- n 1)))))

; funny-number-stream
(define (funny-number-stream)
  (letrec
      ([f (lambda (x)
            (if (= (remainder x 5) 0)
                (cons (* -1 x) (lambda () (f (+ 1 x))))
                (cons x (lambda () (f (+ 1 x))))))])
    (f 1)))

; dan-then-dog
(define (dan-then-dog)
  (letrec
      ([f (lambda (x)
            (if (= (remainder x 2) 0)
                (cons "dan.jpg" (lambda () (f (+ 1 x))))
                (cons "dog.jpg" (lambda () (f (+ 1 x))))))])
    (f 0)))


; stream-add-zero
(define (stream-add-zero stream)
  (lambda ()
    (cons 
     (cons 0 (car (stream)))
     (lambda () (stream-add-zero (cdr (stream)))))))


; cycle-lists
(define (cycle-lists xs ys)
  (lambda ()
    (letrec
        ([f (lambda (i)
              (let*
                  ([cur-x (list-nth-mod xs i)]
                   [cur-y (list-nth-mod ys i)])
                (cons
                    (cons cur-x cur-y)
                    (lambda () (f (+ 1 i))))))])
      (f 0))))
                   
              

;; assoc function in racket
(define (vector-assoc v vec)
  (letrec
      ([find-v  (lambda (pos)
                  (cond [(>= pos (vector-length vec)) #f]
                        [(pair? (vector-ref vec pos))
                                (if (equal? (car (vector-ref vec pos)) v)
                                    (vector-ref vec pos)
                                    (find-v (+ 1 pos)))]
                        [#t (find-v (+ 1 pos))]))])
    (find-v 0)))

;; cached-assoc
(define (cached-assoc xs n)
  (letrec
      ([cache (make-vector n #f)]
       [pos 0])
    (lambda (v)
      (let*
          ([val-in-cache (vector-assoc v cache)])
        (if val-in-cache
            val-in-cache
            (let*
                ([val-in-list (assoc v xs)])
              (if val-in-list
                  (begin
                    (vector-set! cache pos val-in-list)
                    (set! pos (remainder (+ 1 pos) n))
                    val-in-list)
                  #f)))))))