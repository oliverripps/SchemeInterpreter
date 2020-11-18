#lang racket
; Oliver Ripps and Osama Abdelrahman

(require rackunit rackunit/text-ui rackunit/gui)
(require "env.rkt" "parse.rkt" "interp.rkt")

(provide interp-tests)

(define test-env
  (env '(x y) '(10 23) init-env))


(define interp-tests
  (test-suite
   "Interpreter tests"
   (test-equal? "App-exp test 2 args"
                (eval-exp '((var-exp +) ((lit-exp 2) (lit-exp 4))) test-env)
                6)
   (test-equal? "App-exp test 1 arg"
                (eval-exp '((var-exp add1) ((lit-exp 2))) test-env)
                3)
   (test-equal? "Var-exp test"
                (eval-exp '(var-exp x) test-env)
                10)
   (test-equal? "Lit-exp test"
                (eval-exp (lit-exp 10) empty-env)
                10)
   (test-equal? "testing conditional"
                (eval-exp '(if ((var-exp eqv?) ((lit-exp 10) (var-exp x))) (var-exp x) (var-exp False)) test-env)
                10)
   (test-equal? "testing list"
                (eval-exp '(if ((var-exp list?) (((var-exp list) ((lit-exp 5) (lit-exp 6))))) (var-exp True) (var-exp False)) test-env)
                'True)
   (test-equal? "testing extending an environment and replacing bindings"
                (eval-exp '(let (x y z) ((lit-exp 5) (lit-exp 4) (lit-exp 2)) ((var-exp +) ((var-exp x) (var-exp y)))) init-env)
                9)
   (test-equal? "evaluating the first example"
                (eval-exp '(let (a b) ((lit-exp 1) (lit-exp 5)) ((var-exp +) ((var-exp a) (var-exp b)))) test-env)
                6)
   (test-equal? "evaluating the first example"
                (eval-exp '(let (a b) (((var-exp *) ((lit-exp 2) (lit-exp 3))) (lit-exp 24)) (let (c) (((var-exp -) ((var-exp b) (var-exp a)))) ((var-exp *) ((var-exp c) ((var-exp +) ((var-exp a) (var-exp b))))))) test-env)
                540)
   (test-equal? "evaluating parsed lambda exp"
                (eval-exp (parse ((lambda (x) x) 1)) test-env)
                1)
   (test-equal? "evaluating parsed let and lambda exp"
                (eval-exp (parse (let ((sqr (lambda (x) (* x x)))) (let ((cube (lambda (x) (* x (sqr x))))) (cube 3)))) test-env)
                27)
   (test-equal? "changing binding and then evaluating expression"
                (eval-exp (parse '(+ 2 2)) test-env)
                4)
   (eval-exp '(set! + (var-exp -)) test-env)
   
   (test-equal? "changing binding and then evaluating expression"
                (eval-exp (parse '(+ 2 2)) test-env)
                0)
   (eval-exp (parse '(set! + (lambda (x y) (- x (negate y))))) test-env)
   
   (test-equal? "changing binding and then evaluating expression with begin"
                (eval-exp (parse '(let ([x 1] [y 2]) (begin (set! x 23) (+ x y)))) test-env)
                25)
   ))


(run-tests interp-tests)
