#lang racket
; Oliver Ripps and Osama Abdelrahman

(provide env
         env?
         empty-env
         empty-env?
         extended-env?
         env-syms
         env-vals
         env-previous
         env-lookup)

; The empty environment is null.
(define empty-env null)

; Environment constructor.
(define (env syms vals previous-env)
  (cond [(not (list? syms)) (error 'env "syms is not a list")]
        [(not (list? vals)) (error 'env "vals is not a list")]
        [(not (env? previous-env)) (error 'env "previous-env is not an env")]
        [else (list 'env syms vals previous-env)]))

; Environment recognizers.
(define (env? e)
  (or (empty-env? e) (extended-env? e)))

(define (empty-env? e)
  (null? e))

(define (extended-env? e)
  (and (list? e)
       (not (null? e))
       (eq? (first e) 'env)))

; Environment accessors.
(define (env-syms e)
  (cond [(empty-env? e) empty]
        [(extended-env? e) (second e)]
        [else (error 'env-syms "e is not an env")]))

(define (env-vals e)
  (cond [(empty-env? e) empty]
        [(extended-env? e) (third e)]
        [else (error 'env-vals "e is not an env")]))

(define (env-previous e)
  (cond [(empty-env? e) (error 'env-previous "e has no previous env")]
        [(extended-env? e) (fourth e)]
        [else (error 'env-previous "e is not an env")]))

(define (env-lookup environment symbol)
  (termat (indof (env-syms environment) symbol environment) symbol))

(define (indof lst s e)
  (define (indof* lst s acc e)
    (cond [(and (empty? lst) (empty-env? (env-previous e)))  (list -1 e)]
          [(empty? lst) (indof* (env-syms (env-previous e)) s 0 (env-previous e))]
          [(equal? (car lst) s) (list acc e)]
          [else (indof* (cdr lst) s (+ 1 acc) e)]))
  (indof* lst s 0 e))
(define (termat numenv s)
  (define (termat* num s lst)
    (cond [(equal? -1 num) (error 'env-lookup "No binding for ~s" s)]
          [(equal? num 0) (car lst)]
          [else (termat* (- num 1) s (cdr lst))]))
  (termat* (first numenv) s (env-vals (second numenv))))

