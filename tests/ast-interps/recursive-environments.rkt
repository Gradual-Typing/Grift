#lang racket/base

(provide
 empty-env
 env-lookup
 env-extend
 env-cons
 env-extend-rec
 global-env
 global-env-extend!
 global-env-lookup)

;; Create a place holder for locations that are uninitialized
(define undefined (gensym))

(define (undefined? x)
  (eq? x undefined))

#;(: env-lookup ((Env Value) Id -> Value))
;; Lookup the value associated with Id in Env
(define (env-lookup env id)
  (define (if-not-found) (error 'AST-Interp "Unbound var ~a" id))
  (define val (unbox (hash-ref env id if-not-found)))
  (when (undefined? val)
    (error 'AST-Interp "Undefined values reference ~a" id))
  val)

#;(: env-cons (-> Uid CL-Value (Env CL-Value) (Env CL-Value)))
(define (env-cons uid val env)
  (hash-set env uid (box val)))

#;(: env-extend ((Env Value) Uid* Value* -> (Env Value)))
;; Extend the Environment with each Id mapping to the associated
;; value given.
;; Assumes that the lists given are of equal length
(define (env-extend env uid* val*)
  (foldr env-cons env uid* val*))

#;(: env-extend/undef* (-> (Env CL-Value) Uid* (Env CL-Value)))
;; Helper for env-extend-rec allocates all the variables in
;; the environment without initiallizing them.
(define (env-extend/undef env uid*)
  (define (make-empty-box _) (box undefined))
  (env-extend env uid* (map make-empty-box uid*)))

#;(: env-set! (-> (Env CL-Value) Uid ((Env Value) -> Value) Void))
;; Helper for env-extend-rec*. Initialize the value of a
;; variable in the environment.
(define ((env-set! env) id ctr)
  (define (if-not-found) (error 'ast-interp "Unbound var ~a" id))
  (define b (hash-ref env id if-not-found))
  (set-box! b (ctr env)))

#|
(: env-extend-rec*
   ((Env Value) Uid* (Listof ((Env CL-Value) -> CL-Value)) -> 
    (Env Value)))
Extend an environment with mutually recursive bindings. Were each
variable is mapped to the value produce by giving the recusive
environment to the corresponding value constructor.
Offers a means of recursively extendin environments with mutually
recursive bindings. Non of the values should require the value
of an not yet initialized binding.
|#
(define (env-extend-rec env uid* mk-val*)
  (define rec-env (env-extend/undef env uid*))
  (for-each (env-set! rec-env) uid* mk-val*)
  rec-env)

;; Construct an empty-environment
(define-syntax-rule (empty-env) (hash))
(define-syntax-rule (global-env) (make-hash))

(define (global-env-extend! env id* code*)
  (for-each (lambda (id code) (hash-set! env id code)) id* code*))

(define (global-env-lookup env id)
  (hash-ref env id (lambda () (error 'global-env-lookup "unbound ~a" id))))
