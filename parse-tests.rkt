#lang racket
;Oliver Ripps and Osama Abdelrahman


(require rackunit rackunit/text-ui rackunit/gui)
(require "parse.rkt")

(provide parse-tests)

(define lit1 (lit-exp 5))
(define var1 (var-exp 'x))

(define parse-tests
  (test-suite
   "Parse tests"
   (test-pred "Literal"
           lit-exp?
           (parse 5))
   (test-true "lit1 is a lit-exp"
              (lit-exp? lit1))
   (test-equal? "lit1 has a value of 5"
              (lit-exp-num lit1)
              5)
   (test-true "var1 is a var-exp"
              (var-exp? var1))
   (test-equal? "var1 has a value of x"
              (var-exp-symbol var1)
              'x)
   (test-equal? "var1 has a value of x"
              (var-exp-symbol var1)
              'x)
   (test-equal? "parsing (foo)"
                (parse '(foo))
                '((var-exp foo) ()))
   (test-equal? "parsing (bar 1)"
                (parse '(bar 1))
                '((var-exp bar) ((lit-exp 1))))
   (test-equal? "parsing (baz x y)"
                (parse '(baz x y))
                '((var-exp baz) ((var-exp x) (var-exp y))))
   (test-exn "parsing an empty list should throw an error"
          exn:fail?
          (λ () (parse '())))
   (test-equal? "parsing conditional statement"
                (parse '(if (eqv? 23 x) x False))
                '(if ((var-exp eqv?) ((lit-exp 23) (var-exp x))) (var-exp x) (var-exp False)))
   (test-true "testing cond-exp?"
              (cond-exp? (parse '(if (eqv? 23 x) x False))))
   
   (test-exn "not giving a condition should throw an error"
             exn:fail?
             (λ () (parse '(if 4 4))))
   (test-equal? "parsing a let expression"
               (parse '(let ([x 5] [y 4] [z 2]) (+ x y)))
               '(let (x y z) ((lit-exp 5) (lit-exp 4) (lit-exp 2)) ((var-exp +) ((var-exp x) (var-exp y)))))
   (test-exn "not giving symbols should throw an error"
             exn:fail?
             (λ () (parse '(let (4 4) (+ x y)))))
   (test-equal? "parsing example one expression"
               (parse '(let ([a 1] [b 5]) (+ a b)))
               '(let (a b) ((lit-exp 1) (lit-exp 5)) ((var-exp +) ((var-exp a) (var-exp b)))))
   (test-equal? "parsing example two expression"
               (parse '(let ([a (* 2 3)] [b 24]) (let ([c (- b a)]) (* c (+ a b)))))
               '(let (a b) (((var-exp *) ((lit-exp 2) (lit-exp 3))) (lit-exp 24)) (let (c) (((var-exp -) ((var-exp b) (var-exp a)))) ((var-exp *) ((var-exp c) ((var-exp +) ((var-exp a) (var-exp b))))))))
   (test-equal? "parsing lambda expression"
               (parse '((lambda (x) x) 1))
               '((lambda (x) (var-exp x)) ((lit-exp 1))))
   (test-equal? "parsing let expression with lambda"
               (parse '(let ((sqr (lambda (x) (* x x)))) (let ((cube (lambda (x) (* x (sqr x))))) (cube 3))))
               '(let (sqr)
   ((lambda (x) ((var-exp *) ((var-exp x) (var-exp x)))))
   (let (cube) ((lambda (x) ((var-exp *) ((var-exp x) ((var-exp sqr) ((var-exp x))))))) ((var-exp cube) ((lit-exp 3))))))
   (test-equal? "parsing set! expression"
               (parse '(set! + -))
               '(set! + (var-exp -)))
   (test-equal? "parsing set! expression"
               (parse '(set! + (lambda (x y) (- x (negate y)))))
               '(set! + (lambda (x y) ((var-exp -) ((var-exp x) ((var-exp negate) ((var-exp y))))))))
   (test-equal? "parsing set! expression"
                (parse '(let ([x 1] [y 2]) (begin (set! x 23) (+ x y))))
                '(let (x y) ((lit-exp 1) (lit-exp 2)) (begin ((set! x (lit-exp 23)) ((var-exp +) ((var-exp x) (var-exp y)))))))))

(run-tests parse-tests)