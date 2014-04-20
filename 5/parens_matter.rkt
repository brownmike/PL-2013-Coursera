; Programming Languages, Dan Grossman, Jan-Mar 2013 
; Section 5: Parentheses Matter! (Debugging Practice)

#lang racket

(provide (all-defined-out))

; [first big difference from ML (and Java)] PARENS MATTER!!

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) ; right

(define (fact1 n) (if (= n 0) (1) (* n (fact1 (- n 1))))) ; wrong! (1)

(define (fact1b n) (if (= n 0) (1) (* n (fact (- n 1))))) ; also wrong!

(define (fact2 n) (if (= n 0) 1 (* n fact2 (- n 1)))) ; also wrong! function cannot be passed to * function