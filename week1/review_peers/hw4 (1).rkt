
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;;1. sequence
(define (sequence low high stride)
  (if(> low high) null
       (cons low (sequence (+ low stride) high stride))))

;;2. string-append-map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;3. list-nth-mod
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

;;4. stream-for-n-steps
(define (stream-for-n-steps s n)
  (if(= n 0)
     null
     (let([next (s)])
     (cons (car next)(stream-for-n-steps (cdr next) (- n 1))))))

;;5. funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda(x)
                (if (= 0 (remainder x 5))
                     (cons (* x -1)(lambda () (f (+ x 1))))
                     (cons x (lambda()(f (+ x 1))))))])
    (lambda()(f 1))))

;;6. dan-then-dog
(define dan-then-dog
  (letrec ([f (lambda(x)
                (if (equal? x "dan.jpg")
                    (cons x (lambda ()(f "dog.jpg")))
                    (cons x (lambda ()(f "dan.jpg")))))])
    (lambda()(f "dan.jpg"))))

;;7. stream-add-zero
(define (stream-add-zero s)
    (letrec([f (lambda(s)
             (let([next (s)])
               (cons(cons 0 (car next))(lambda()(f(cdr next))))))])
    (lambda()(f s))))

;;8. cycle-lists
(define (cycle-lists xs ys)
  (letrec([f (lambda(n)
               (cons(cons (list-nth-mod xs n)(list-nth-mod ys n))(lambda()(f (+ n 1)))))])
    (lambda()(f 0))))

;;9. vector-assoc
(define (vector-assoc v vec)
  (letrec([f (lambda(n)
              (if (equal? n (vector-length vec))
                  #f
               (let([element (vector-ref vec n)])
                 (cond[(equal? n (vector-length vec)) #f]
                      [(and (pair? element)(equal? v (car element))) element]
                      [#t (f (+ n 1))]))))])
    (f 0)))
               
;;10. cached-assoc
(define (cached-assoc xs n)
  (let([cache (make-vector n #f)]
       [slot 0])
    (letrec([f (lambda(v)
              (let([ans (vector-assoc v cache)])
                (if(not ans)
                   (let([new-ans (assoc v xs)])
                     (if (pair? new-ans)
                         (begin
                           (vector-set! cache slot new-ans)
                           (set! slot (remainder (+ 1 slot) (vector-length cache)))
                           new-ans)
                         #f))
                   #f)))])
      f)))
                
;;11 while-less macro
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let([e1-result e1])
       (letrec([loop(lambda()
                      (let([e2-result e2])
                        (if (>= e2-result e1-result)
                            #t
                            (begin (loop)))))])
         (loop)))]))
                        
       
    