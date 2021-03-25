;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist lst)
  (cond [(null? lst) (aunit)]
        [#t (apair
                  (car lst)
                  (racketlist->mupllist (cdr lst)))]))

;; CHANGE (put your solutions here)

;; Problem 2
(define (mupllist->racketlist lst)
  (cond [(aunit? lst) null]
        [#t (cons
                 (apair-e1 lst)
                 (mupllist->racketlist (apair-e2 lst)))]))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]


        ;; values
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]

        ;; if greater expression
        [(ifgreater? e) (let* ([v1 (eval-under-env (ifgreater-e1 e) env)]
                               [v2 (eval-under-env (ifgreater-e2 e) env)])
                          (if (and (int? v1) (int? v2))
                              (if (> (int-num v1) (int-num v2))
                                  (eval-under-env (ifgreater-e3 e) env)
                                  (eval-under-env (ifgreater-e4 e) env))
                              (error "e1 and e2 in ifgreater should eval to ints")))]


        ;; mlet expression
        [(mlet? e)   (let ([new-env (cons
                                     (cons (mlet-var e) (eval-under-env (mlet-e e) env))
                                     env)])
                       (eval-under-env (mlet-body e) new-env))]

        ;; apair expression
        [(apair? e)  (apair
                          (eval-under-env (apair-e1 e) env)
                          (eval-under-env (apair-e2 e) env))]

        ;; fst
        [(fst? e) (let ([result (eval-under-env (fst-e e) env)])
                    (if (apair? result)
                        (apair-e1 result)
                        (error "The expression should be a pair")))]
        ;; snd
        [(snd? e) (let ([result (eval-under-env (snd-e e) env)])
                    (if (apair? result)
                        (apair-e2 result)
                        (error "The expression should be a pair")))]


        ;; isaunit
        [(isaunit? e) (let ([result (eval-under-env (isaunit-e e) env)])
                        (if (aunit? result)
                            (int 1)
                            (int 0)))]

        ;; function

        [(fun? e) (closure env e)] ;; env should extend functions name to the exp


        ;; call
        ;; (struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
        ;; (struct call (funexp actual)       #:transparent) ;; function call
        ;; (struct closure (env fun) #:transparent) 
        [(call? e) (let ([v1 (eval-under-env (call-funexp e) env)]
                         [v2 (eval-under-env (call-actual e) env)])
                     (if (closure? v1)
                         (letrec ([func (closure-fun v1)]
                                [func-binding (cons (fun-nameopt func) v1)]
                                [param-binding (cons (fun-formal func) v2)]
                                [new-env (cons param-binding (closure-env v1))])
                           (if (fun-nameopt func)
                               (eval-under-env (fun-body func) (cons func-binding new-env))
                               (eval-under-env (fun-body func) new-env)))
                         (error "first expression of call exp should evaluate to a closure")))]
                                               

                         
                            
        
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;; beatifully done
(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [#t (mlet (car (car lstlst)) (cdr (car lstlst))
                  (mlet* (cdr lstlst) e2))]))

;; if e1 > e2 or e1 < e2 then e4 else e3
(define (ifeq e1 e2 e3 e4)
  (mlet*
       (list (cons "_x" e1)
             (cons "_y" e2))
       
       (ifgreater (var "_x") (var "_y")
                  e4
                  (ifgreater (var "_y") (var "_x")
                             e4
                             e3))))
             

;; Problem 4

(define mupl-map
  (fun #f "func"
       (fun "apply" "xs"
              (ifgreater (isaunit (var "xs")) (int 0)
                         (aunit)
                         (apair
                          (call (var "func") (fst (var "xs")))
                          (call (var "apply") (snd (var "xs"))))))))
              
                        

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (fun #f "xs"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "i"))))
                        (var "xs"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
