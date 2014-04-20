#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; pur your code below

;; 1
(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))]))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (cond [(= (remainder n (length xs)) 0) (car xs)]
                  [#t (car (list-tail xs (remainder n (length xs))))])]))

;; 4
(define (stream-for-n-steps s n)
  (let ([ans (s)])
    (if (= n 0)
        null
        (cons (car ans) (stream-for-n-steps (cdr ans) (- n 1))))))

;; 5
(define funny-number-stream
  (letrec ([f (lambda (n)
                (if (= (remainder n 5) 0)
                    (cons (- n n n) (lambda () (f (+ n 1))))
                    (cons n (lambda () (f (+ n 1))))))])
    (lambda () (f 1))))

;; 6
(define dan-then-dog
  (letrec ([f1 (lambda () (cons "dan.jpg" (lambda () (f2))))]
           [f2 (lambda () (cons "dog.jpg" (lambda () (f1))))])
    (lambda () (f1))))

;; 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s)))
                      (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

;; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (i)
                (if (= i len)
                    #f
                    (let ([vref (vector-ref vec i)])
                      (cond [(= i len) #f]
                            [(not (pair? vref)) (f (+ i 1))]
                            [(equal? (car vref) v) vref]
                            [#t (f (+ i 1))]))))])
    (f 0)))

;; 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cIndx 0]
           [cache-magic (lambda (cIndx v)
                          (let ([cAns (vector-assoc v cache)])
                          (if cAns
                              cAns
                              (let ([xsAns (assoc v xs)])
                                (if xsAns
                                    (begin
                                      (vector-set! cache cIndx xsAns)
                                      (set! cIndx (remainder (+ cIndx 1) n))
                                      xsAns)
                                    #f)))))])
    (lambda (v) (cache-magic cIndx v))))