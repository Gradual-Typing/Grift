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

(define-type (Purify-Letrec+ E)
  (Letrec (Bnd* (Castable-Lambda E)) E))

(define-type (Purify-Letrec- E)
  (Letrec (Bnd* E) E))

(define-type (Purify-Letrec= E)
  (U (Castable-Lambda-Forms E)
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
     (Tuple-Operation-Forms E)))

(define-type Data0-Lang
  (Prog (List String Natural Grift-Type)
        (GlobDecs Uid* D0-Expr)))

(define-type D0-Bnd-Code* (Listof D0-Bnd-Code))
(define-type D0-Bnd-Code (Pairof Uid D0-Code))
(define-type D0-Code (Code Uid* D0-Expr))



(define-type D0-Expr
  (Rec E (U (Labels D0-Bnd-Code* E)
	    (App-Code E (Listof E))
        (UIL-Op! E)
        (UIL-Op E)
        No-Op
	    (If E E E)
        (Switch E (Switch-Case* E) E)
	    (Begin D0-Expr* E)
        (Repeat Uid E E Uid E E)
        Break-Repeat
	    (Var Uid)
        (Global String)
	    (Code-Label Uid)
	    (Quote D0-Literal)
        (Assign Id E)
        Success
        Stack-Alloc)))

(define-type D0-Expr* (Listof D0-Expr))

(define-type Data5-Lang
  (Prog (List String Natural Grift-Type)
	(GlobDecs Uid*
                    (Labels D5-Bnd-Code*
                            D5-Body))))

(define-type D5-Body (Locals Uid* (Bnd* Nat) D5-Tail))
(define-type D5-Bnd-Code* (Listof D5-Bnd-Code))
(define-type D5-Bnd-Code (Pairof Uid D5-Code))
(define-type D5-Code (Code Uid* D5-Body))

(define-type D5-Tail
  (Rec T
   (U (If D5-Pred T T)
      (Switch D5-Trivial (Switch-Case* T) T)
      (Begin D5-Effect* T)
      (Return D5-Value)
      (Return Success))))

(define-type D5-Pred (Relop UIL-Pred-Prim D5-Trivial D5-Trivial))

(define-type D5-Effect
  (U (Repeat Uid D5-Trivial D5-Trivial #f #f (Begin D5-Effect* No-Op))
     (If D5-Pred (Begin D5-Effect* No-Op) (Begin D5-Effect* No-Op))
     Break-Repeat
     (Switch D5-Trivial
             (Switch-Case* (Begin D5-Effect* No-Op))
             (Begin D5-Effect* No-Op))
     (UIL-Op! D5-Trivial)
     (Assign Id D5-Value)
     No-Op))

(define-type D5-Value
  (U D5-Trivial
     Halt
     (UIL-Op D5-Trivial)
     (App-Code D5-Trivial D5-Trivial*)
     (If D5-Pred D5-Trivial D5-Trivial)))

(define-type D5-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Global String)
     (Quote D5-Literal)))


(define-type Data4-Lang
  (Prog (List String Natural Grift-Type)
	(GlobDecs Uid*
                    (Labels D4-Bnd-Code*
                            D4-Body))))

(define-type D4-Body (Locals Uid* (Bnd* Nat) D4-Tail))
(define-type D4-Bnd-Code* (Listof D4-Bnd-Code))
(define-type D4-Bnd-Code (Pairof Uid D4-Code))
(define-type D4-Code (Code Uid* D4-Body))

(define-type D4-Tail
  (Rec T
   (U (If D4-Pred T T)
      (Switch D4-Trivial (Switch-Case* T) T)
      (Begin D4-Effect* T)
      (Return D4-Value)
      (Return Success))))

(define-type D4-Pred
 (Rec P
      (U (If D4-Pred P P)
         (Switch D4-Trivial (Switch-Case* P) P)
         (Begin D4-Effect* P)
         (Relop UIL-Pred-Prim D4-Trivial D4-Trivial))))

(define-type D4-Effect
 (Rec E
      (U (If D4-Pred E E)
         (Switch D4-Trivial (Switch-Case* E) E)
         (Begin D4-Effect* No-Op)
         (Repeat Uid D4-Trivial D4-Trivial #f #f E)
         (UIL-Op! D4-Trivial)
         (Assign Id D4-Value)
         Break-Repeat
         No-Op)))

(define-type D4-Value
  (U D4-Trivial
     Halt
     (UIL-Op D4-Trivial)
     (App-Code D4-Trivial D4-Trivial*)))

(define-type D4-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Global String)
     (Quote D4-Literal)))

(define-type Data3-Lang
  (Prog (List String Natural Grift-Type)
	(GlobDecs Uid*
                    (Labels D3-Bnd-Code*
                            D3-Body))))

(define-type D3-Body (Locals Uid* (Bnd* Nat) D3-Tail))
(define-type D3-Bnd-Code* (Listof D3-Bnd-Code))
(define-type D3-Bnd-Code (Pairof Uid D3-Code))
(define-type D3-Code (Code Uid* D3-Body))

(define-type D3-Tail
  (Rec T
   (U (If D3-Pred T T)
      (Switch D3-Trivial (Switch-Case* T) T)
      (Begin D3-Effect* T)
      (Return D3-Value)
      (Return Success))))

(define-type D3-Value
 (Rec V
  (U D3-Trivial
     Halt
     (If D3-Pred V V)
     (Switch D3-Trivial (Switch-Case* V) V)
     (Begin D3-Effect* V)
     (Op UIL-Expr-Prim (Listof D3-Trivial))
     (App-Code D3-Trivial D3-Trivial*))))

(define-type D3-Pred
 (Rec P
      (U (If D3-Pred P P)
         (Switch D3-Trivial (Switch-Case* P) P)
         (Begin D3-Effect* P)
         (Relop UIL-Pred-Prim D3-Trivial D3-Trivial))))

(define-type D3-Effect
 (Rec E
      (U (If D3-Pred E E)
         (Switch D3-Trivial (Switch-Case* E) E)
         (Begin D3-Effect* No-Op)
         (Repeat Uid D3-Trivial D3-Trivial #f #f E)
         Break-Repeat
         (App-Code D3-Trivial D3-Trivial*)
         (UIL-Op! D3-Trivial)
         (Assign Id D3-Value)
         No-Op)))

;; (TODO halt is not trivial though because we are targeting c it may be treated so)
;; remove Halt earlier
(define-type D3-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Global String)
     (Quote D3-Literal)))

(define-type D3-Value* (Listof D3-Value))

(define-type Data2-Lang
  (Prog (List String Natural Grift-Type)
        (GlobDecs Uid*
                    (Labels D2-Bnd-Code*
                            D2-Body))))

(define-type D2-Body (Locals Uid* (Bnd* Nat) D2-Tail))
(define-type D2-Bnd-Code* (Listof D2-Bnd-Code))
(define-type D2-Bnd-Code (Pairof Uid D2-Code))
(define-type D2-Code (Code Uid* D2-Body))

(define-type D2-Tail D1-Tail)

(define-type D2-Value D1-Value)

(define-type D2-Pred D1-Pred)

(define-type D2-Effect D1-Effect)

(define-type D2-Value* (Listof D2-Value))

(define-type D2-Effect* (Listof D2-Effect))

(define-type D2-Literal Data-Literal)

(define-type Data1-Lang
  (Prog (List String Natural Grift-Type)
        (GlobDecs Uid*
                    (Labels D1-Bnd-Code*
                            D1-Tail))))

(define-type D1-Bnd-Code* (Listof D1-Bnd-Code))
(define-type D1-Bnd-Code (Pairof Uid D1-Code))
(define-type D1-Code (Code Uid* D1-Tail))

(define-type D1-Tail
  (Rec T
   (U (If D1-Pred T T)
      (Switch D1-Value (Switch-Case* T) T)
      (Begin D1-Effect* T)
      (App-Code D1-Value D1-Value*)
      (Op UIL-Expr-Prim D1-Value*)
      (Var Uid)
      (Global String)
      Halt Success Stack-Alloc
      (Var Uid)
      (Code-Label Uid)
      (Quote D1-Literal))))

(define-type D1-Value
 (Rec V
      (U (If D1-Pred V V)
         (Switch V (Switch-Case* V) V)
         (Begin D1-Effect* V)
         (App-Code V (Listof V))
         (Op UIL-Expr-Prim (Listof V))
         Halt
         Stack-Alloc
         (Var Uid)
         (Global String)
         (Code-Label Uid)
         (Quote D1-Literal))))

(define-type D1-Pred
 (Rec P
      (U (If D1-Pred P P)
         (Switch D1-Value (Switch-Case* P) P)
         (Begin D1-Effect* P)
         (Relop UIL-Pred-Prim D1-Value D1-Value))))

(define-type D1-Effect
 (Rec E
      (U (If D1-Pred E E)
         (Switch D1-Value (Switch-Case* E) E)
         (Begin D1-Effect* No-Op)
         (Repeat Uid D1-Value D1-Value #f #f E)
         Break-Repeat
         (App-Code D1-Value D1-Value*)
         (UIL-Op! D1-Value)
         (Assign Id D1-Value)
         No-Op)))

(define-type Cast-or-Coerce4-Lang
  (Prog (List String Natural Grift-Type)
    (Static* (List CoC4-Bnd-Mu-Type*
                   CoC4-Bnd-Type*
                   CoC4-Bnd-Mu-Crcn*
                   CoC4-Bnd-Crcn*
                   CoC4-Bnd-Data*)
             CoC4-Expr)))

(define-type (Named-Castable-Lambda-Forms E)
  (U (Letrec (Bnd* (Castable-Lambda E)) E)
     (Fn-Caster E)
     (App-Fn E (Listof E))))

(define-type CoC4-Expr
  (Rec E (U
          (Gen-Data-Forms E)
          (Code-Forms E)
          (Named-Castable-Lambda-Forms E)
          (Fn-Proxy-Forms E)
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
          (Tuple-Operation-Forms E)
          )))

(define-type Cast0-Lang
  (Prog (List String Natural Grift-Type) C0-Top*))

(define-type C0-Top* (Listof C0-Top))

(define-type C0-Top
  (U (Define Boolean Uid Grift-Type C0-Expr)
     (Observe C0-Expr Grift-Type)))

(define-type Cast0.5-Lang
  (Prog (List String Natural Grift-Type) C0-Expr))

(define-type C0-Expr
  (Rec E (U ;; Non-Terminals
          (Observe E Grift-Type)
          (Lambda Uid* E)
          (Letrec C0-Bnd* E)
          (Let C0-Bnd* E)
          (App E (Listof E))
          (Op Grift-Primitive (Listof E))
          (If E E E)
          (Switch E (Switch-Case* E) E)
          (Cast E (Twosome Grift-Type Grift-Type Blame-Label))
          (Begin C0-Expr* E)
          (Repeat Uid E E Uid E E)
          ;; Guarded effects
          (Gbox E)
          (Gunbox E)
          (Gbox-set! E E)
          (Gvector E E)
          (Gvector-set! E E E)
          (Gvector-ref E E)
          (Gvector-length E)
          ;; Monotonic
          (Mbox E Grift-Type)
          (Munbox E) ;; fast read
          (Mbox-set! E E) ;; fast write
          (MBoxCastedRef E Grift-Type)
          (MBoxCastedSet! E E Grift-Type)
          (Mvector E E Grift-Type)
          (Mvector-ref E E) ;; fast read
          (Mvector-set! E E E) ;; fast write
          (MVectCastedRef E E Grift-Type)
          (MVectCastedSet! E E E Grift-Type)
          (Mvector-length E)
          ;; Dynamic Operations
          (Dyn-GVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-GVector-Ref E E Blame-Label)
          (Dyn-GVector-Len E E)
          (Dyn-GRef-Set! E E Grift-Type Blame-Label)
          (Dyn-GRef-Ref E Blame-Label)
          (Dyn-MVector-Set! E E E Grift-Type Blame-Label)
          (Dyn-MVector-Ref E E Blame-Label)
          (Dyn-MRef-Set! E E Grift-Type Blame-Label)
          (Dyn-MRef-Ref E Blame-Label)
          (Dyn-Fn-App E C0-Expr* Grift-Type* Blame-Label)
          (Dyn-Tuple-Proj E E E)
          (Create-tuple (Listof E))
          (Tuple-proj E Index)
          ;; Terminals
          (Var Uid)
          (Quote Cast-Literal)
          No-Op)))

(define-type C0-Expr* (Listof C0-Expr))

(define-type Cast-or-Coerce5-Lang
  (Prog (List String Natural Grift-Type)
    (Let-Static* Bnd-Mu-Type*
                 Bnd-Type*
                 Bnd-Mu-Crcn*
                 Bnd-Crcn* 
                 CoC5-Expr)))

(define-type CoC5-Expr
  (Rec E
       (U
        (Construct CoC5-Gen-Data CoC5-Gen-Ctor (Listof E))
        (Access CoC5-Gen-Data CoC5-Gen-Access E (Option E))
        (Check CoC5-Gen-Data CoC5-Gen-Pred E (Listof E))
        ;; Code Labels
        (Code-Label Uid)
        (Labels CoC5-Bnd-Code* E)
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
        (Quote-Coercion Immediate-Coercion)
        ;(Compose-Coercions E E)
        (HC E E E E E E)
        (HC-Inject-Huh E)
        (HC-Project-Huh E)
        (HC-Identity-Huh E)
        (HC-Label E)
        (HC-T1 E)
        (HC-T2 E)
        (HC-Med E)
        (Id-Coercion-Huh E)
        (Fn-Coercion-Huh E)
        (Make-Fn-Coercion Uid E E E)
        (Fn-Coercion (Listof E) E)
        (Fn-Coercion-Arity E)
        (Fn-Coercion-Arg E E)
        (Fn-Coercion-Return E)
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
        ;;Type operations
        (Type Immediate-Type)
        (Type-Fn-arity E)
        (Type-Fn-arg E E)
        (Type-Fn-return E)
        (Type-GRef-Of  E)
        (Type-GVect-Of E)
        (Type-Dyn-Huh E)
        (Type-Fn-Huh E)
        (Type-GRef-Huh E)
        (Type-GVect-Huh E)
        (Type-Mu-Huh E)
        (Type-Mu-Body E)
        ;; Tags are exposed before specify This is bad
        ;; TODO fix this after the deadline
        (Type-Tag E)
        (Tag Tag-Symbol)
        ;;(Type-Ctr-Case E Type-Ctr-Case-Clause* E)
        ;; Binding Forms - Lambda
        (Letrec CoC5-Bnd-Lambda* E)
        (Let CoC5-Bnd-Data* E)
        (Var Uid)
        (Global String)
        (Assign Id E)
        ;; Controll Flow
        (If E E E)
        (Switch E (Switch-Case* E) E)
        (Begin CoC5-Expr* E)
        (Repeat Uid E E Uid E E)
        Break-Repeat
        ;;Primitives
        (Op Grift-Primitive (Listof E))
        No-Op
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
        (Mbox E Immediate-Type)
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
        (Error E)
        (Mvector E E Immediate-Type)
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
        ;;
        (Create-tuple (Listof E))
        (Copy-Tuple E E)
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

(define-type CoC5-Expr* (Listof CoC5-Expr))
(define-type CoC5-Code (Code Uid* CoC5-Expr))
(define-type CoC5-Bnd-Code (Pairof Uid CoC5-Code))
(define-type CoC5-Bnd-Code* (Listof CoC5-Bnd-Code))
(define-type CoC5-Lambda (Lambda Uid* (Free (Option Uid) Uid* CoC5-Expr)))
(define-type CoC5-Bnd-Lambda  (Pairof Uid CoC5-Lambda))
(define-type CoC5-Bnd-Lambda* (Listof CoC5-Bnd-Lambda))
(define-type CoC5-Bnd-Data  (Pairof Uid CoC5-Expr))
(define-type CoC5-Bnd-Data* (Listof CoC5-Bnd-Data))
