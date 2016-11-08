#lang typed/racket
#|
Pass: src/casts/define->letrec
Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)
Description: This pass translates top level defines into nested letrecs.
Subsequent defines end up in the same letrec but each observable expression
triggers a new scope. This is not how this should be implemented but it is
what I can do now. 
This behavior is not what we would normally think of for letrec and should
be fixed to conform with letrec* semantics.
+-------------------------------------------------------------------------------+
|Input Grammar Cast0-Language                                                   |
|Output Grammar Cast0.5-Language                                                  |
+------------------------------------------------------------------------------|#
(require 
         (submod "../logging.rkt" typed)
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast0.rkt")

(provide define->let
         (all-from-out "../language/cast0.rkt"))

;; The entry point for this pass it is called by impose-casting semantics
;; Eventually we should make this 
(: define->let (Cast0-Lang -> Cast0.5-Lang))
(define (define->let prgm)
  (match-define (Prog (list name next type) t*) prgm)
  (define uc (make-unique-counter next))
  (define e : C0-Expr 
    (parameterize ([current-unique-counter uc])
      (: make-letrec : C0-Bnd* C0-Top* -> C0-Expr)
      (define (make-letrec b* t*)
        (match t*
          [(cons (Define #t i _ e) t*-rest)
           (make-letrec (cons (cons i e) b*) t*-rest)]
          [t* (Letrec b* (loop t*))]))
      (: make-begin : C0-Expr* C0-Top* -> C0-Expr)
      (define (make-begin e* t*)
        (match t*
          [(cons (and e (Observe _ _)) t*-rest)
           (make-begin (cons e e*) t*-rest)]
          [t* (Begin (reverse e*) (loop t*))]))
      (: loop : C0-Top* -> C0-Expr)
      (define (loop t*)
        (match t*
          [(cons (Define #f i _ e) t*-rest)
           (Let `((,i . ,e)) (loop t*-rest))]
          [(cons (Define #t i _ e) t*-rest)
           (make-letrec `((,i . ,e)) t*-rest)]
          [(cons (and e (Observe _ _)) t*-rest)
           (make-begin `(,e) t*-rest)]
          [(list) NO-OP]
          [other (error 'define->letrec/nested-letrec "unhandled case: ~a" other)]))
      (loop t*)))
  
  (debug (Prog (list name (unique-counter-next! uc) type) e)))
