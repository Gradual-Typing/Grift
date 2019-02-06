#lang racket/base
(require "forms.rkt")

(provide (all-defined-out))
#|-----------------------------------------------------------------------------+
| Language/Cast3 created by interpret-casts                                    |
+-----------------------------------------------------------------------------|#

(struct Closure form:simple-branch
  (name well-known? code-generation code-label
        self caster free-vars parameters code)
  #;
  ([name : Uid]
   [well-known? : Boolean]
   [code-generation : Code-Generation]
   [code-label : Uid]
   [self : Uid]
   [caster : (Option Uid)]
   [free-vars : (Listof F)]
   [parameters : Uid*]
   [code : E]) ;; bogus if `code-generations` = 'closure-only
  #:transparent)

#;(define-type (Closure* F E) (Listof (Closure F E)))

(struct Closure-Code form:simple-branch
  (arg)
  #:transparent)

(struct Closure-Caster form:simple-branch
  (arg)
  #:transparent)

(struct Closure-Ref form:leaf
  (arg key)
  #:transparent)

(struct Closure-App form:simple-branch
  (code closure arguments)
  #:transparent)

;; (define-type (Closure-Ops E)
;;   (U (Let-Closures Uid E)
;;      (Closure-Code E)
;;      (Closure-Caster E)
;;      (Closure-App E)))

;; (define-type (Closure-Ops/Ref E)
;;   (U (Let-Closures E E)
;;      (Closure-Code E)
;;      (Closure-Caster E)
;;      (Closure-Ref E) 
;;      (Closure-App E)))

;; (define-type (Hybrid-Fn-Proxy-Forms E)
;;   (U Closure-Proxy
;;      (Hybrid-Proxy-Huh E)
;;      (Hybrid-Proxy-Closure E)
;;      (Hybrid-Proxy-Coercion E)))

;; (define-type (Data-Fn-Proxy-Forms E)
;;   (U (Fn-Proxy Index E E)
;;      (Fn-Proxy-Huh E)
;;      (Fn-Proxy-Closure E)
;;      (Fn-Proxy-Coercion E)))

#;
(define-type Cast-or-Coerce6-Lang
  (Prog (List String Natural Grift-Type)
        (Static*
         (List Bnd-Mu-Type*
               Bnd-Type*
               Bnd-Mu-Crcn*
               Bnd-Crcn*
               (Bnd* (Fun CoC6-Expr))
               (Closure* CoC6-Expr CoC6-Expr)
               (Bnd* CoC6-Expr))
         CoC6-Expr)))

#;
(define-type CoC5-Expr
  (Rec
   E
   (U (Closure-Ops E)
      (Data-Fn-Proxy-Forms E)
      (Hybrid-Fn-Proxy-Forms E)
      (Gen-Data-Forms E)
      (Code-Forms E)
      (Quote-Coercion Immediate-Coercion)
      (Hyper-Coercion-Operation-Forms E)
      (Coercion-Operation-Forms E)
      (Type Immediate-Type)
      (Type-Operation-Forms E)
      (Let (Bnd* E) E)
      (Var Uid)
      (Global String)
      (Assign Id E)
      (Control-Flow-Forms E)
      (Op Grift-Primitive (Listof E))
      No-Op
      (Quote Cast-Literal)
      (Blame E)
      (Observe E Grift-Type)
      (Unguarded-Forms E)
      (Guarded-Proxy-Forms E)
      (Monotonic-Forms E Immediate-Type)
      (Error E)
      (Tuple-Operation-Forms E))))

#;
(define-type CoC6-Expr
  (Rec
   E
   (U (Closure-Ops/Ref E)
      (Data-Fn-Proxy-Forms E)
      (Hybrid-Fn-Proxy-Forms E)
      (Gen-Data-Forms E)
      (Code-Forms E)
      (Quote-Coercion Immediate-Coercion)
      (Hyper-Coercion-Operation-Forms E)
      (Coercion-Operation-Forms E)
      (Type Immediate-Type)
      (Type-Operation-Forms E)
      (Let (Bnd* E) E)
      (Var Uid)
      (Global String)
      (Assign Id E)
      (Control-Flow-Forms E)
      (Op Grift-Primitive (Listof E))
      No-Op
      (Quote Cast-Literal)
      (Blame E)
      (Observe E Grift-Type)
      (Unguarded-Forms E)
      (Guarded-Proxy-Forms E)
      (Monotonic-Forms E Immediate-Type)
      (Error E)
      (Tuple-Operation-Forms E))))

#;
(define-type Code-Generation
  (U
   ;; both code and closure
   'regular
   ;; Only generate the code, but be compatible with
   ;; any identical fv-list. This is used for the
   ;; definition closure-casters, and when a well-known
   ;; closure shares the closures of closure that isn't
   ;; well-known.
   ;; TODO list invarients needed by 'code-only-code
   'code-only
   ;; Only allocate the closure, don't generate the
   ;; code. This is used to share code between function
   ;; cast, apply-casted closure, of the same arity.
   'closure-only))

;; TODO This should go in configuration.rkt
#;(define-type Fn-Proxy-Representation (U 'Hybrid 'Data))
#;(: fn-proxy-representation (Parameterof Fn-Proxy-Representation))
(define fn-proxy-representation
  (make-parameter 'Hybrid))

(struct Static-Let* form:simple-branch
  (bindings program)
  #:transparent)

(struct Closure-Proxy form:leaf (closure)
  #:transparent)

;; TODO this should go in 
(struct Let-Closures form:simple-branch (bindings body)
  #:transparent)

;; (define-type CoC6-Expr* (Listof CoC6-Expr))
;; (define-type CoC6-Code (Code Uid* CoC6-Expr))
;; (define-type CoC6-Bnd-Code (Pairof Uid CoC6-Code))
;; (define-type CoC6-Bnd-Code* (Listof CoC6-Bnd-Code))
;; (define-type CoC6-Bnd-Data  (Pairof Uid CoC6-Expr))
;; (define-type CoC6-Bnd-Data* (Listof CoC6-Bnd-Data))
;; (define-type CoC6-Bnd-Type  (Pairof Uid Compact-Type))
;; (define-type CoC6-Bnd-Type* (Listof CoC6-Bnd-Type))
;; (define-type CoC6-Bnd-Crcn  (Pairof Uid Compact-Coercion))
;; (define-type CoC6-Bnd-Crcn* (Listof CoC6-Bnd-Crcn))

