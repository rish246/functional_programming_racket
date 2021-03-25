#lang racket

; make stream 2 4 8 16 ...
; Weird looking syntax .. not gonna lie but worth it
(define power-of-two
  (lambda ()
    (letrec ([f (lambda (x)
                  (cons x (lambda () (f (* 2 x)))))])
            (f 2))))