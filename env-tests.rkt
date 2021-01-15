#lang racket


(require rackunit rackunit/text-ui rackunit/gui)
(require "env.rkt")

(provide env-tests)
; Define an environment for testing.
(define test-env
  (env '(x y) '(1 2) empty-env))
(define test-env2
  (env '(a b c z x) '(10 9 8 5 3) test-env))

(define env-tests
  (test-suite
   "Environment tests"
   (test-true "Empty environment recognizer"
              (empty-env? empty-env))
   
   (test-true "Empty environment is an environment"
              (env? empty-env))
   
   (test-false "Empty environment is not extended"
               (extended-env? empty-env))

   (test-true "Extended environment recognizer"
              (extended-env? test-env))

   (test-true "Extended environment is an environment"
              (env? test-env))

   (test-false "Extended environment is not empty"
               (empty-env? test-env))

   (test-equal? "Symbols accessor"
                (env-syms test-env)
                '(x y))

   (test-equal? "Values accessor"
                (map unbox (env-vals test-env))
                '(1 2))

   (test-equal? "Previous environment accessor"
                (env-previous test-env)
                empty-env)

   (test-equal? "Previous environment accessor with non-empty previous"
                (env-previous (env '(z) '(3) test-env))
                test-env)

   (test-exn "Empty environment has no previous"
             exn:fail?
             (λ () (env-previous empty-env)))
   (test-exn "The Environment is empty"
          exn:fail?
          (λ () (env-lookup empty-env 'z)))
   (test-equal? "a is bound to 10"
                (unbox (env-lookup test-env2 'a))
                10)
   (test-equal? "y is bound to 2 from the original extended environment"
                (unbox (env-lookup test-env2 'y))
                2)
   (test-equal? "x is bound to 3 after being bound to 1 in the extended environment"
                (unbox (env-lookup test-env2 'x))
                3)
   (test-exn "Symbol z does not exist in test-env"
          exn:fail?
          (λ () (env-lookup test-env 'z)))))


(run-tests env-tests)
