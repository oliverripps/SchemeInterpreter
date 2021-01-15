#lang racket

(require "parse.rkt" "env.rkt")


(define (closure? obj)
  (cond [(empty? obj) #f]
        [(equal? (first obj) 'closure) #t]
        [else #f]))
(define (closure parameter-list body environment)
  (cond [(list 'closure parameter-list body environment)]))

(define (closure-params c)
  (cond [(closure? c) (second c)]
        [else (error 'closure-params "~s is not a closure" c)]))
(define (closure-body c)
  (cond [(closure? c) (third c)]
        [else (error 'closure-body "~s is not a closure" c)]))
(define (closure-env c)
  (cond [(closure? c) (fourth c)]
        [else (error 'closure-env "~s is not a closure" c)]))

(define (do-begins tree e last)
  (cond [(empty? tree) last]
        [else (do-begins (cdr tree) e (eval-exp (car tree) e))]))
(define (prim-proc symb)
  (cond [(symbol? symb) (list 'var-exp symb)]
        [else (error 'prim-proc "~s is not a symbol" symb)]))

(define (prim-proc? proc)
  (cond [(empty? proc) #f]
        [(and (list? proc) (equal? (car proc) 'var-exp)) #t]
        [else #f]))
(define (prim-proc-op proc)
  (cond [(prim-proc? proc) (second proc)]
        [else (error 'prim-proc-op "~s is not a prim-proc" proc)]))

(define (apply-proc proc args)
  (cond [(prim-proc? proc)
         (apply-primitive-op (prim-proc-op proc) args)]
        [(closure? proc) (eval-exp (closure-body proc) (env (closure-params proc) args (closure-env proc)))] 
        [else (error 'apply-proc "bad procedure: ~s" proc)]))
(define (apply-primitive-op op args)
  (cond [(eq? op '+) (apply + args)]
        [(eq? op '-) (apply - args)]
        [(eq? op '*) (apply * args)]
        [(eq? op '/) (apply / args)]
        [(eq? op 'negate) (apply (lambda (num) (* num -1)) args)]
        [(eq? op 'add1) (apply (lambda (num) (+ num 1)) args)]
        [(eq? op 'sub1) (apply (lambda (num) (- num 1)) args)]
        [(eq? op 'list) (apply list args)]
        [(eq? op 'cons) (apply cons args)]
        [(eq? op 'car) (first (first args))]
        [(eq? op 'cdr) (rest (first args))]
        [(eq? op 'eqv?) (if (apply equal? args) 'True 'False)]
        [(eq? op 'lt?) (if (apply < args) 'True 'False)]
        [(eq? op 'gt?) (if (apply > args) 'True 'False)]
        [(eq? op 'leq?) (if (apply <= args) 'True 'False)]
        [(eq? op 'null?) (if (null? (car args)) 'True 'False)]
        [(eq? op 'list?) (if (list? (car args)) 'True 'False)]
        [(eq? op 'number?) (if (number? (car args)) 'True 'False)]
        [else (error 'apply-primitive-op "Unknown primitive: ~s" op)]))
(define primitive-operators '(+ - * / negate add1 sub1 list cons car cdr eqv? lt? gt? leq? null? list? number?))

(define prim-env
  (env primitive-operators
       (map prim-proc primitive-operators)
       empty-env))
(define init-env
  (env '(x y null empty True False)
       '(23 42 () () True False)
       prim-env))

(define (eval-exp tree e)
  (cond [(lit-exp? tree) (lit-exp-num tree)]
        [(var-exp? tree) (unbox (env-lookup e (var-exp-symbol tree)))]
        [(cond-exp? tree) (
                           cond [(or (equal? 'True (eval-exp (second tree) e)) (not (equal? 'False (eval-exp (second tree) e)))) (eval-exp (third tree) e)]
                                [else (eval-exp (fourth tree) e)])]
        [(let-exp? tree) (eval-exp (fourth tree) (env (second tree)
                                 (map (lambda (x) (eval-exp x e)) (third tree))
                                 e))]
        [(lambda-exp? tree) (closure (second tree) (third tree) e)]
        [(set-exp? tree) (set-box! (env-lookup e (set-exp-parameter tree)) (eval-exp (set-exp-binding tree) e))]
        [(begin-exp? tree) (do-begins (second tree) e empty)]
        [(app-exp? tree) (apply-proc (eval-exp (app-exp-proc tree) e) (map (lambda (element) (eval-exp element e)) (car (app-exp-args tree))))]
        [else (error 'eval-exp "Invalid tree: ~s" tree)]))


(provide (all-defined-out))
