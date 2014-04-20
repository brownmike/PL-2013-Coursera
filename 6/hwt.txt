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

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; (a)
(define (racketlist->mupllist rktLst)
  (letrec ([f (lambda (rktLst muplLst)
                (if (null? rktLst)
                    muplLst
                    (f (cdr rktLst) (apair (car rktLst) muplLst))))])
    (f (reverse rktLst) (aunit))))

;; (b)
(define (mupllist->racketlist muplLst)
  (letrec ([f (lambda (muplLst rktLst)
                (if (aunit? muplLst)
                    (reverse rktLst)
                    (f (apair-e2 muplLst) (cons (apair-e1 muplLst) rktLst))))])
    (f muplLst null)))

;; Problem 2

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
        ;; CHANGE add more cases here
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        
        [(fst? e)
         (let ([value (eval-under-env (fst-e e) env)])
           (if (apair? value)
               (apair-e1 value)
               (error "MUPL fst applied to non-apair")))]
        
        [(snd? e)
         (let ([value (eval-under-env (snd-e e) env)])
           (if (apair? value)
               (apair-e2 value)
               (error "MUPL snd applied to non-apair")))]
        
        [(fun? e)
         (closure env e)]
        
        [(mlet? e)
         (let ([value (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) value) env)))]
        
        [(call? e)
         (let* ([CallArg (eval-under-env (call-actual e) env)]
                [FunClosure (eval-under-env (call-funexp e) env)])
           (if (closure? FunClosure)
               (letrec ([ClosureEnv (closure-env FunClosure)]
                        [Function (closure-fun FunClosure)]
                        [FunctionBody (fun-body Function)]
                        [FunctionName (fun-nameopt Function)]
                        [EnvVar (cons (fun-formal Function) CallArg)]
                        [CombinedEnv (append (cons EnvVar ClosureEnv) env)])
                 (if FunctionName
                     (eval-under-env FunctionBody (cons (cons FunctionName FunClosure) CombinedEnv))
                     (eval-under-env FunctionBody CombinedEnv)))
               (error "MUPL call used with non-function")))]
           
        ;; END OF ANSWER
        [#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

;; 3a
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;; 3b
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([lst-remainder (mlet* (cdr lstlst) e2)])
        (mlet (car (car lstlst)) (cdr (car lstlst)) lst-remainder))))

;; 3c
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "AnonFunction"
       (fun "map" "muplList"
            (ifaunit (var "muplList")
                     (if #t (aunit) (aunit))
                     (mlet* (list
                             (cons "head" (call (var "AnonFunction") (fst (var "muplList"))))
                             (cons "tail" (call (var "map") (snd (var "muplList")))))
                            (apair (var "head") (var "tail")))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "element" (add (var "i") (var "element")))))))

(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

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