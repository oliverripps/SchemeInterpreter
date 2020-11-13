#lang racket
; Oliver Ripps and Osama Abdelrahman


(require "parse.rkt")
(require "interp.rkt")

(define (read-eval-print)
  (let ([orig (error-escape-handler)])
    (let/ec exit
      (let retry-loop ()
        (let/ec escape
          (error-escape-handler
           (lambda () (escape #f)))
          (let loop ()
            (begin
              (display "MS> ")
              (let ([in (read)])
                (if (eq? in 'exit )
                    (begin
                      (printf "returning to Scheme proper~n")
                      (exit #f))
                    (begin
                      (display (eval-exp (parse in) init-env))
                      (newline)
                      (loop)))))))
        (retry-loop)))
    (error-escape-handler orig)))

(read-eval-print)
