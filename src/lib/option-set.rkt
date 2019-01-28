#lang typed/racket/base

(provide (all-defined-out))
(require racket/set)

(: option-set-add (All (A) (Option (Setof A)) A -> (Option (Setof A))))
(define (option-set-add os x)
  (and os (set-add os x)))

(: option-set-remove (All (A) (Option (Setof A)) (Option A) -> (Option (Setof A))))
(define (option-set-remove os x)
  (and os x (set-remove os x)))

(: option-set-empty? (All (A) (Option (Setof A)) -> Boolean))
(define (option-set-empty? os)
  (and os (set-empty? os)))

(: option-set-union
   (All (A) (Option (Setof A)) (Option (Setof A)) -> (Option (Setof A))))
(define (option-set-union os1 os2)
  (and os1 os2 (set-union os1 os2)))

(: option-set-union*
   (All (A) (Listof (Option (Setof A))) -> (Option (Setof A))))
(define (option-set-union* os*)
  (define (and-foldr [acc : (Setof A)] [os* : (Listof (Option (Setof A)))])
    : (Option (Setof A))
    (cond
      [(null? os*) acc]
      [(car os*)
       (and-foldr (set-union acc (car os*)) (cdr os*))]
      [else #f]))
  (and-foldr (seteq) os*))
