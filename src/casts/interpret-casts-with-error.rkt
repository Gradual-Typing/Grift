#lang typed/racket/base
#| This pass implements casts by error whenever anything that is non-static exists.
   It is used to implement a static varient of the GTLC that relies on as much
   of the same machinery of as the gradual varients as possible. 
|#
(require
 racket/match
 "../language/cast0.rkt"
 "../language/cast-or-coerce3.rkt"
 "./interpret-casts-common.rkt")
(provide interpret-casts/error)

(: interpret-casts/error : -> (C0-Expr -> CoC3-Expr))
(define (interpret-casts/error)
  (make-map-expr
   #:compile-cast compile-cast/error
   #:compile-lambda
   (lambda ([f* : Uid*] [e : CoC3-Expr])
     (Lambda f* (Castable #f e)))
   #:compile-app App-Fn 
   #:pbox-ref Unguarded-Box-Ref
   #:pbox-set Unguarded-Box-Set!
   #:pvec-ref Unguarded-Vect-Ref
   #:pvec-set Unguarded-Vect-Set!
   #:pvec-len Unguarded-Vect-length
   ;; Override all of monotonic with regular reference types
   #:mbox
   (λ ([v : CoC3-Expr] [t : Schml-Type])
     (Unguarded-Box v))
   #:mbox-ref
   (λ ([a : CoC3-Expr] [t2 : CoC3-Expr])
     (Unguarded-Box-Ref a))
   #:mbox-set
   (λ ([a : CoC3-Expr] [v : CoC3-Expr] [t1 : CoC3-Expr])
     (Unguarded-Box-Set! a v))
   #:stc-mbox-ref Unguarded-Box-Ref
   #:stc-mbox-set Unguarded-Box-Set!
   #:mvec
   (λ ([v : CoC3-Expr] [s : CoC3-Expr] [t : Schml-Type])
     (Unguarded-Vect v s))
   #:mvec-ref
   (λ ([a : CoC3-Expr][i : CoC3-Expr][t2 : CoC3-Expr])
     (Unguarded-Vect-Ref a i))
   #:mvec-set
   (λ ([a : CoC3-Expr] [i : CoC3-Expr] [v : CoC3-Expr] [t1 : CoC3-Expr])
     (Unguarded-Vect-Set! a i v))
   #:mvec-len Unguarded-Vect-length 
   #:stc-mvec-ref Unguarded-Vect-Ref
   #:stc-mvec-set Unguarded-Vect-Set!
   
   #:dyn-pbox-ref    (err 'dyn-pbox-ref)
   #:dyn-pbox-set    (err 'dyn-pbox-set!)
   #:dyn-pvec-ref    (err 'dyn-pvec-ref)
   #:dyn-pvec-set    (err 'dyn-pvec-set!)
   #:dyn-pvec-len    (err 'dyn-pvec-len)
   #:dyn-mbox-ref    (err 'dyn-mbox-ref)
   #:dyn-mbox-set    (err 'dyn-mbox-set!)
   #:dyn-mvec-ref    (err 'dyn-mvec-ref)
   #:dyn-mvec-set    (err 'dyn-mvec-set!)
   #:dyn-fn-app      (err 'dyn-fn-app)
   #:dyn-tup-prj     (err 'dyn-tup-prj)))

  (define ((err sym) . a)
    (error 'interpret-casts/error
           "~a: used in supposedly static code"
           sym))
  (define (compile-cast/error v t1 t2 l [mt : CoC3-Expr ZERO-EXPR]
                              #:t1-not-dyn [t1-not-dyn #f]
                              #:t2-not-dyn [t2-not-dyn #f])
    (error 'interpret-casts/error
           "compile-cast: used in supposedly static code"))


