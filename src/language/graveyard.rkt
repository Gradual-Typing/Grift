#lang typed/racket/no-check

;; The diffs between languages are one of the best sources of documentation
;; of what passes do to the language grammar that we currently have.
;; I want to leave them in place until we have contracts that are able to
;; dynamically check the language grammars. With the hopes that we will
;; be able to calculate the contracts based on the diffs.

#|-----------------------------------------------------------------------------+
| Language/Cast3 created by interpret-casts                                    |
+-----------------------------------------------------------------------------|#

(define-type Cast-or-Coerce3-Lang
  (Prog (List String Natural Grift-Type)
    (Static* (List CoC3-Bnd*)
             CoC3-Expr)))


(define-type CoC3-Expr
  (Rec E (U (Castable-Lambda-Forms E)
            (Fn-Proxy-Forms E)
            (Letrec (Bnd* E) E)
            (Let (Bnd* E) E)
            (Var Uid) 
            (Global String)
            (Assign Id E)
            (Gen-Data-Forms E)
            (Code-Forms E)
            (Quote-Coercion Grift-Coercion)
            (Coercion-Operation-Forms E)
            (Quote-HCoercion Mixed-Coercion) 
            (Hyper-Coercion-Operation-Forms E)
            (Type Grift-Type)
            (Type-Operation-Forms E)
            (Control-Flow-Forms E)
            (Op Grift-Primitive (Listof E))
            (Quote Cast-Literal)
            No-Op
            (Blame E)
            (Observe E Grift-Type)
            (Unguarded-Forms E)
            (Guarded-Proxy-Forms E)
            (Monotonic-Forms E Grift-Type)
            (Error E)
            (Tuple-Operation-Forms E))))

#;
(define-type CoC3-Expr
  (Rec E (U
          (Construct CoC3-Gen-Data CoC3-Gen-Ctor (Listof E))
          (Access CoC3-Gen-Data CoC3-Gen-Access E (Option E))
          (Check CoC3-Gen-Data CoC3-Gen-Pred E (Listof E))
          (Observe E Grift-Type) 
          No-Op
          ;; Code Labels
          (Code-Label Uid)
          (Labels CoC3-Bnd-Code* E)
          (App-Code E (Listof E))
          ;; Functions as an interface
          (Lambda Uid* (Castable (Option Uid) E))
          (Fn-Caster E)
          (App-Fn E (Listof E))
          ;; Our Lovely Function Proxy Representation
          (App-Fn-or-Proxy Uid E (Listof E))
          (Fn-Proxy (List Index Uid) E E)
          (Fn-Proxy-Huh E)
          (Fn-Proxy-Closure E)
          (Fn-Proxy-Coercion E)
          ;; Coercions
          (Quote-Coercion Grift-Coercion)
          (Quote-HCoercion Mixed-Coercion)
          (Compose-Coercions E E)
          (Id-Coercion-Huh E)
          (Fn-Coercion-Huh E)
          (Make-Fn-Coercion Uid E E E)
          (Fn-Coercion (Listof E) E)
          (Fn-Coercion-Arg E E)
          (Fn-Coercion-Return E)
          (Fn-Coercion-Arity E)
          (Id-Fn-Coercion E)
          (Fn-Coercion-Return-Set! E E)
          (Fn-Coercion-Arg-Set! E E E) 
          (Ref-Coercion E E E)
          (Ref-Coercion-Huh E)
          (Ref-Coercion-Read E)
          (Ref-Coercion-Write E)
          (Ref-Coercion-Ref-Huh E)
          (Sequence-Coercion E E)
          (Sequence-Coercion-Huh E)
          (Sequence-Coercion-Fst E)
          (Sequence-Coercion-Snd E)
          (Project-Coercion E E)
          (Project-Coercion-Huh E)
          (Project-Coercion-Type E)
          (Project-Coercion-Label E)
          (Inject-Coercion E)
          (Inject-Coercion-Type E)
          (Inject-Coercion-Huh E)
          (Failed-Coercion E)
          (Failed-Coercion-Huh E)
          (Failed-Coercion-Label E)
          (HC E E E E E E)
          (HC-Inject-Huh E)
          (HC-Project-Huh E)
          (HC-Identity-Huh E)
          (HC-Label E)
          (HC-T1 E)
          (HC-T2 E)
          (HC-Med E)
          ;;Type operations
          (Type Grift-Type)
          (Type-Dyn-Huh E)
          (Type-Fn-Huh E)
          (Type-Fn-arity E)
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-GRef-Huh E)
          (Type-GRef-Of  E)
          (Type-GVect-Huh E)
          (Type-GVect-Of E)
          (Type-Mu-Huh E)
          (Type-Mu-Body E)
          ;; Tags are exposed before specify This is bad
          ;; TODO fix this after the deadline
          (Type-Tag E)
          (Tag Tag-Symbol)
          ;; Binding Forms - Lambda
          (Letrec CoC3-Bnd* E)
          (Let CoC3-Bnd* E)
          (Var Uid)
          (Global String)
          (Assign Id E)
          ;; Control Flow
          (If E E E)
          (Switch E (Switch-Case* E) E)
          (Begin CoC3-Expr* E)
          (Repeat Uid E E Uid E E)
          Break-Repeat
          ;;Primitives
          (Op Grift-Primitive (Listof E))
          (Quote Cast-Literal)
          ;; Observations
          (Blame E)
          (Observe E Grift-Type)
          ;; Unguarded-Representation
          (Unguarded-Box E)
          (Unguarded-Box-Ref E)
          (Unguarded-Box-Set! E E)
          (Unguarded-Vect E E)
          (Unguarded-Vect-Ref E E)
          (Unguarded-Vect-Set! E E E)
          (Guarded-Proxy-Huh E)
          (Guarded-Proxy E (Twosome E E E))
          (Guarded-Proxy E (Coercion E))
          (Guarded-Proxy-Ref E)
          (Guarded-Proxy-Source E)
          (Guarded-Proxy-Target E)
          (Guarded-Proxy-Blames E)
          (Guarded-Proxy-Coercion E)
          (Unguarded-Vect-length E)
          ;; Monotonic references
          (Mbox E Grift-Type)
          (Mbox-val-set! E E)
          (Mbox-val-ref E)
          (Mbox-rtti-set! E E)
          (Mbox-rtti-ref E)
          (Make-GLB-Two-Fn-Types Uid E E)
          (Make-GLB-Two-Tuple-Types Uid E E)
          (MRef-Coercion-Huh E)
          (MRef-Coercion-Type E)
          (MRef-Coercion E)
          (Type-GRef E) ;; glb need to create new types in runtime
          (Type-GVect E)
          (Type-MRef E)
          (Type-MRef-Huh E)
          (Type-MRef-Of E)
          (Mvector E E Grift-Type)
          (Mvector-length E)
          (Mvector-val-ref E E VectorAccessMode)
          (Mvector-val-set! E E E VectorAccessMode)
          (Mvector-rtti-ref E)
          (Mvector-rtti-set! E E)
          (Type-MVect E)
          (Type-MVect-Huh E)
          (Type-MVect-Of E)
          (MVect-Coercion-Huh E)
          (MVect-Coercion-Type E)
          (MVect-Coercion E)
          (Error E)
          (Create-tuple (Listof E))
          (Tuple-proj E E)
          (Tuple-Coercion-Huh E)
          (Tuple-Coercion-Num E)
          (Tuple-Coercion-Item E E)
          (Id-Tuple-Coercion E)
          (Tuple-Coercion-Item-Set! E E E)
          (Coerce-Tuple Uid E E)
          (Coerce-Tuple-In-Place Uid E E E E E)
          (Cast-Tuple Uid E E E E)
          (Cast-Tuple-In-Place Uid E E E E E E E)
          (Type-Tuple-Huh E)
          (Type-Tuple-num E)
          (Type-Tuple-item E E)
          (Make-Tuple-Coercion Uid E E E)
          (Mediating-Coercion-Huh E))))

;; Hoist Types and Coercions
(define-type (HT&C+ E)
  (U (Type Immediate-Type)
     (Quote-Coercion Immediate-Coercion)
     (Mvector E E Immediate-Type)
     (Mbox E Immediate-Type)))

(define-type (HT&C- E)
  (U (Type Grift-Type)
     (Quote-Coercion Grift-Coercion)
     (Quote-HCoercion Mixed-Coercion)
     (Mvector E E Grift-Type)
     (Mbox E Grift-Type)))

(define-type (HT&C= E)
  (U (Monotonic-Forms-w/o-Constructors E)
     (Castable-Lambda-Forms E)
     (Fn-Proxy-Forms E) 
     (Letrec (Bnd* E) E)
     (Let (Bnd* E) E)
     (Var Uid)
     (Global String)
     (Assign Id E)
     (Gen-Data-Forms E)
     (Code-Forms E)
     (Coercion-Operation-Forms E)
     (Hyper-Coercion-Operation-Forms E)
     (Type-Operation-Forms E)
     (Control-Flow-Forms E)
     (Op Grift-Primitive (Listof E))
     (Quote Cast-Literal)
     No-Op
     (Blame E)
     (Observe E Grift-Type)
     (Unguarded-Forms E)
     (Guarded-Proxy-Forms E)
     (Error E)
     (Tuple-Operation-Forms E)))


(define-type (Closure-Ops E)
  (U (Let-Closures Uid E)
     (Closure-Code E)
     (Closure-Caster E)
     (Closure-App E)))

(define-type (Closure-Ops/Ref E)
  (U (Let-Closures E E)
     (Closure-Code E)
     (Closure-Caster E)
     (Closure-Ref E) 
     (Closure-App E)))

(define-type (Hybrid-Fn-Proxy-Forms E)
  (U Closure-Proxy
     (Hybrid-Proxy-Huh E)
     (Hybrid-Proxy-Closure E)
     (Hybrid-Proxy-Coercion E)))

(define-type (Data-Fn-Proxy-Forms E)
  (U (Fn-Proxy Index E E)
     (Fn-Proxy-Huh E)
     (Fn-Proxy-Closure E)
     (Fn-Proxy-Coercion E)))


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

;; Uncover Free Eliminates the following forms
(define-type (U- E)
  (U (Named-Castable-Lambda-Forms E)
     (Fn-Proxy-Forms E)))

;; Uncover Free Adds The following forms
(define-type (U+ E)
  (U (Closure-Ops E)
     (Data-Fn-Proxy-Forms E)
     (Hybrid-Fn-Proxy-Forms E)))

;; Uncover Free is invariant in the following forms
(define-type (U= E)
  (U (Gen-Data-Forms E)
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
     (Tuple-Operation-Forms E)))

;; This pass removes Lambdas from expression contexts
(define-type (Label-Lambdas- E)
  (Castable-Lambda E))

;; And leaves the rest expression forms alone.
(define-type (Label-Lambdas= E)
  (U (Named-Castable-Lambda-Forms E)
     (App-Fn E (Listof E))
     (Fn-Proxy-Forms E)
     (Let (Bnd* E) E)
     (Var Uid)
     (Global String)
     (Assign Id E)
     (Gen-Data-Forms E)
     (Code-Forms E)
     (Quote-Coercion Immediate-Coercion)
     (Coercion-Operation-Forms E)
     (Hyper-Coercion-Operation-Forms E)
     (Type Immediate-Type)
     (Type-Operation-Forms E)
     (Control-Flow-Forms E)
     (Op Grift-Primitive (Listof E))
     (Quote Cast-Literal)
     No-Op
     (Blame E)
     (Observe E Grift-Type)
     (Unguarded-Forms E)
     (Guarded-Proxy-Forms E)
     (Monotonic-Forms E Immediate-Type)
     (Error E)
     (Tuple-Operation-Forms E)))

(define-type Cast-or-Coerce3.1-Lang
  (Prog (List String Natural Grift-Type)
    (Static* (List
              Bnd-Mu-Type*
              Bnd-Type*
              Bnd-Mu-Crcn*
              Bnd-Crcn*
              CoC3.1-Bnd*) 
             CoC3.1-Expr)))

(define-type (Castable-Lambda E) (CLambda E))

(define-type (Gen-Data-Forms E)
  (U (Construct Gen-Data Gen-Ctor (Listof E))
     (Access Gen-Data Gen-Access E (Option E))
     (Check Gen-Data Gen-Pred E (Listof E))))

(define-type (Code-Forms E)
  (U (Code-Label Uid)
     (Labels (Bnd* (Code Uid* E)) E)
     (App-Code E (Listof E))))

(define-type (Fn-Proxy-Forms E)
  (U (App-Fn-or-Proxy Uid E (Listof E))
     (Fn-Proxy (List Index Uid) E E)
     (Fn-Proxy-Huh E)
     (Fn-Proxy-Closure E)
     (Fn-Proxy-Coercion E)))

(define-type (Coercion-Operation-Forms E)
  (U
   (Sequence-Coercion E E)
   (Sequence-Coercion-Huh E)
   (Sequence-Coercion-Fst E)
   (Sequence-Coercion-Snd E)

   (Project-Coercion E E)
   (Project-Coercion-Huh E)
   (Project-Coercion-Type E)
   (Project-Coercion-Label E)

   (Inject-Coercion E)
   (Inject-Coercion-Huh E)
   (Inject-Coercion-Type E)

   (Mediating-Coercion-Huh E)
     
   (Make-Fn-Coercion Uid E E E)
   (Id-Coercion-Huh E)
   (Id-Fn-Coercion E) 
   (Fn-Coercion-Huh E)
   (Fn-Coercion (Listof E) E)
   (Fn-Coercion-Arity E)
   (Fn-Coercion-Arg E E)
   (Fn-Coercion-Return E)
   (Fn-Coercion-Return-Set! E E)
   (Fn-Coercion-Arg-Set! E E E)

   (Ref-Coercion E E E)
   (Ref-Coercion-Huh E)
   (Ref-Coercion-Read E)
   (Ref-Coercion-Write E)
   (Ref-Coercion-Ref-Huh E)

   (MRef-Coercion E)
   (MRef-Coercion-Huh E)
   (MRef-Coercion-Type E)

   (MVect-Coercion-Huh E)
   (MVect-Coercion-Type E)
   (MVect-Coercion E)

   (Failed-Coercion E)
   (Failed-Coercion-Huh E)
   (Failed-Coercion-Label E)

   (Make-Tuple-Coercion Uid E E E)
   (Id-Tuple-Coercion E)
   (Tuple-Coercion-Huh E)
   (Tuple-Coercion-Num E)
   (Tuple-Coercion-Item E E)
   (Tuple-Coercion-Item-Set! E E E)
   
   Make-Mu-Coercion
   (Mu-Coercion-Huh E)
   (Mu-Coercion-Body-Set! E E)
   (Mu-Coercion-Body E)))

(define-type (Type-Operation-Forms E)
  (U (Type-Dyn-Huh E)
     (Type-Fn-Huh E)
     (Type-Fn-arity E)
     (Type-Fn-arg E E)
     (Type-Fn-return E)
     (Type-GRef-Huh E)
     (Type-GRef-Of  E)
     (Type-GVect-Huh E)
     (Type-GVect-Of E)
     (Type-GRef E) 
     (Type-GVect E)
     (Type-MRef E)
     (Type-MRef-Huh E)
     (Type-MRef-Of E)
     (Type-MVect E)
     (Type-MVect-Huh E)
     (Type-MVect-Of E)
     (Type-Tuple-Huh E)
     (Type-Tuple-num E)
     (Type-Tuple-item E E)
     (Type-Mu-Huh E)
     (Type-Mu-Body E)))

(define-type (Tag-Forms E) 
  ;; Tags are exposed before specify This is bad
  ;; TODO fix this after the deadline
  (U (Type-Tag E)
     (Tag Tag-Symbol)))

(define-type (Control-Flow-Forms E)
  (U (If E E E)
     (Switch E (Switch-Case* E) E)
     (Begin (Listof E) E)
     (Repeat Uid E E Uid E E)
     Break-Repeat))

(define-type (Unguarded-Forms E)
  (U (Unguarded-Box E)
     (Unguarded-Box-On-Stack E)
     (Unguarded-Box-Ref E)
     (Unguarded-Box-Set! E E)
     (Unguarded-Vect E E)
     (Unguarded-Vect-Ref E E)
     (Unguarded-Vect-Set! E E E)
     (Unguarded-Vect-length E)))

(define-type (Guarded-Proxy-Forms E)
  (U (Guarded-Proxy-Huh E)
     (Guarded-Proxy E (U (Twosome E E E) (Coercion E)))
     (Guarded-Proxy-Ref E)
     (Guarded-Proxy-Source E)
     (Guarded-Proxy-Target E)
     (Guarded-Proxy-Blames E)
     (Guarded-Proxy-Coercion E)))

(define-type (Monotonic-Forms-w/o-Constructors E)
  (U (Mbox-val-set! E E)
     (Mbox-val-ref E)
     (Mbox-rtti-set! E E)
     (Mbox-rtti-ref E) 
     (Mvector-length E)
     (Mvector-val-ref E E VectorAccessMode)
     (Mvector-val-set! E E E VectorAccessMode)
     (Mvector-rtti-ref E)
     (Mvector-rtti-set! E E) 
     (Make-GLB-Two-Fn-Types Uid E E)
     (Make-GLB-Two-Tuple-Types Uid E E)))

(define-type (Monotonic-Forms E T)
  (U (Mvector E E T)
     (Mbox E T)
     (Monotonic-Forms-w/o-Constructors E)))

(define-type (Hyper-Coercion-Operation-Forms E)
  (U (HC E E E E E E)
     (HC-Inject-Huh E)
     (HC-Project-Huh E)
     (HC-Identity-Huh E)
     (HC-Label E)
     (HC-T1 E)
     (HC-T2 E)
     (HC-Med E)))


(define-type (Tuple-Operation-Forms E)
  (U (Create-tuple (Listof E)) 
     (Tuple-proj E E)
     (Copy-Tuple E E)
     ;; FIXME These forms shouldn't exist anymore
     (Coerce-Tuple Uid E E)
     (Coerce-Tuple-In-Place Uid E E E E E)
     (Cast-Tuple Uid E E E E)
     (Cast-Tuple-In-Place Uid E E E E E E E)))

(define-type (Castable-Lambda-Forms E)
  (U (Lambda Uid* (Castable (Option Uid) E))
     (Fn-Caster E)
     (App-Fn E (Listof E))))

(define-type CoC3.1-Expr
  (Rec E
       (U (Castable-Lambda-Forms E)
          (Fn-Proxy-Forms E)
          (Letrec (Bnd* E) E)
          (Let (Bnd* E) E)
          (Var Uid) 
          (Global String)
          (Assign Id E)
          (Gen-Data-Forms E)
          (Code-Forms E)
          (Quote-Coercion Immediate-Coercion)
          (Coercion-Operation-Forms E)
          (Hyper-Coercion-Operation-Forms E)
          (Type Immediate-Type)
          (Type-Operation-Forms E)
          (Control-Flow-Forms E)
          ;;Primitives
          (Op Grift-Primitive (Listof E))
          (Quote Cast-Literal)
          No-Op
          ;; Observations
          (Blame E)
          (Observe E Grift-Type)
          (Unguarded-Forms E)
          (Guarded-Proxy-Forms E)
          (Monotonic-Forms E Immediate-Type)
          (Error E)
          (Tuple-Operation-Forms E))))

(define-type Cast-or-Coerce3.2-Lang
  (Prog (List String Natural Grift-Type)
    (Static* (List
              Bnd-Mu-Type*
              Bnd-Type*
              Bnd-Mu-Crcn*
              Bnd-Crcn*
              CoC3.2-Bnd*) 
             CoC3.2-Expr)))

(define-type CoC3.2-Expr
  (Rec E
       (U (Named-Castable-Lambda-Forms E)
          (Castable-Lambda E)
          (Fn-Proxy-Forms E)
          (Let (Bnd* E) E)
          (Var Uid)
          (Global String)
          (Assign Id E)
          (Gen-Data-Forms E)
          (Code-Forms E)
          (Quote-Coercion Immediate-Coercion)
          (Coercion-Operation-Forms E)
          (Hyper-Coercion-Operation-Forms E)
          (Type Immediate-Type)
          (Type-Operation-Forms E)
          (Control-Flow-Forms E)
          (Op Grift-Primitive (Listof E))
          (Quote Cast-Literal)
          No-Op
          (Blame E)
          (Observe E Grift-Type)
          (Unguarded-Forms E)
          (Guarded-Proxy-Forms E)
          (Monotonic-Forms E Immediate-Type)
          (Error E)
          (Tuple-Operation-Forms E))))
