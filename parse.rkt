#lang racket
;Oliver Ripps and Osama Abdelrahman


(provide (all-defined-out))

(define (lit-exp num)
  (cond [(number? num) (list 'lit-exp num)]
        [else (error 'lit-exp "~s is not a number" num)]))

(define (lit-exp? exp)
  (cond [(empty? exp) #f]
        [(and (list? exp) (equal? (car exp) 'lit-exp)) #t]
        [else #f]))

(define (lit-exp-num exp)
  (cond [(lit-exp? exp) (second exp)]
        [else (error 'lit-exp-num "~s is not a lit-exp" exp)]))

(define (var-exp symb)
  (cond [(symbol? symb) (list 'var-exp symb)]
        [else (error 'var-exp "~s is not a symbol" symb)]))

(define (var-exp? exp)
  (cond [(empty? exp) #f]
        [(and (list? exp) (equal? (car exp) 'var-exp)) #t]
        [else #f]))

(define (var-exp-symbol exp)
  (cond [(var-exp? exp) (second exp)]
        [else (error 'var-exp-symbol "~s is not a var-exp" exp)]))

(define (app-exp proc args)
  (cond [(var-exp? proc) (list proc args)]
        [(lambda-exp? proc) (list proc args)]
        [else (error 'app-exp "~s is not a procedure" proc)]))

(define (app-exp? exp)
  (cond [(empty? exp) #f]
        [(symbol? (first exp)) #t]
        [(var-exp? (first exp)) #t]
        [(lambda-exp? (first exp)) #t]
        [else #f]))

(define (app-exp-proc exp)
  (cond [(app-exp? exp) (first exp)]
        [else (error 'app-exp-proc "~s is not a app-exp" exp)]))

(define (app-exp-args exp)
  (cond [(app-exp? exp) (rest exp)]
        [else (error 'app-exp-proc "~s is not a app-exp" exp)]))

(define (cond-exp exp)
  (cond [(equal? (first exp) 'if) exp]
        [else (error 'cond-exp "~s is not a cond-exp" exp)]))

(define (cond-exp? exp)
  (cond [(empty? exp) #f]
        [(equal? (car exp) 'if) #t]
        [else #f]))

(define (cond-exp-iftrue exp)
  (cond [(cond-exp? exp) (second exp)]
        [else (error 'cond-exp-iftrue "~s is not a cond-exp" exp)]))

(define (cond-exp-iffalse exp)
  (cond [(cond-exp? exp) (third exp)]
        [else (error 'cond-exp-iffalse "~s is not a cond-exp" exp)]))


(define (let-exp exp)
  (cond [(equal? 'let (first exp)) exp]
        [else (error 'let-exp "~s is not a let-exp" exp)]))

(define (let-exp? exp)
  (cond [(empty? exp) #f]
        [(equal? (first exp) 'let) #t]
        [else #f]))

(define (let-exp-symbols exp)
  (cond [(let-exp? exp) (second exp)]
        [else (error 'let-exp-sybols "~s is not a let-exp" exp)]))
(define (let-exp-bindings exp)
  (cond [(let-exp? exp) (third exp)]
        [else (error 'let-exp-bindings "~s is not a let-exp" exp)]))
(define (let-exp-body exp)
  (cond [(let-exp? exp) (fourth exp)]
        [else (error 'let-exp-body "~s is not a let-exp" exp)]))

(define (lambda-exp exp)
  (cond [(equal? 'lambda (first exp)) exp]
        [else (error 'lambda-exp "~s is not a lambda-exp" exp)]))
        
(define (lambda-exp? exp)
  (cond [(empty? exp) #f]
        [(equal? (first exp) 'lambda) #t]
        [else #f]))

(define (lambda-exp-parameters exp)
  (cond [(lambda-exp? exp) (second exp)]
        [else (error 'lambda-exp-parameters "~s is not a lambda-exp" exp)]))
(define (lambda-exp-body exp)
  (cond [(lambda-exp? exp) (third exp)]
        [else (error 'lambda-exp-body "~s is not a lambda-exp" exp)]))

(define (set-exp exp)
  (cond [(equal? 'set! (first exp)) exp]
        [else (error 'set-exp "~s is not a set-exp" exp)]))
(define (set-exp? exp)
  (cond [(empty? exp) #f]
        [(equal? (first exp) 'set!) #t]
        [else #f]))
(define (set-exp-parameter exp)
  (cond [(set-exp? exp) (second exp)]
        [else (error 'set-exp-parameter "~s is not a set-exp" exp)]))
(define (set-exp-binding exp)
  (cond [(set-exp? exp) (third exp)]
        [else (error 'set-exp-binding "~s is not a set-exp" exp)]))

(define (begin-exp exp)
  (cond [(equal? 'begin (first exp)) exp]
        [else (error 'begin-exp "~s is not a begin-exp" exp)]))
(define (begin-exp? exp)
  (cond [(empty? exp) #f]
        [(equal? (first exp) 'begin) #t]
        [else #f]))
(define (begin-statements exp)
  (cond [(begin-exp? exp) (second exp)]
        [else (error 'begin-statements "~s is not a begin-exp" exp)]))

(define (parse input)
  (cond [(number? input) (lit-exp input)]
        [(symbol? input) (var-exp input)]
        [(list? input)
         (cond [(empty? input) (parse error)]
               [(cond-exp? input) (cond-exp (list 'if (parse (second input)) (parse (third input)) (parse (fourth input))))]
               [(let-exp? input) (let-exp (list 'let (map first (second input)) (map (lambda (x) (parse x)) (map second (second input))) (parse (third input))))]
               [(lambda-exp? input) (lambda-exp (list 'lambda (second input) (parse (third input))))]
               [(set-exp? input) (set-exp (list 'set! (second input) (parse (third input))))]
               [(begin-exp? input) (begin-exp (list 'begin (map parse (cdr input))))]
               [(app-exp? input) (app-exp (parse (first input))
                                          (cond [(equal? (length input) 2) (list (parse (second input)))]
                                                [(> (length input) 2) (map parse (rest input))]
                                                [else empty]))])]
        [else (error 'parse "Invalid syntax ~s" input)]))

