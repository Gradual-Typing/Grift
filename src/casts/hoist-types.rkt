#lang typed/racket
#|-----------------------------------------------------------------------------+
|Pass: src/casts/hoist-types                                                   |
+------------------------------------------------------------------------------+
|Author: Deyaaeldeen Almahallawi (dalmahal@indiana.edu)                        |
+------------------------------------------------------------------------------+
|Description: It hoist all types to the top of the program, and put pointer to |
|to their definition in each use, so that allocation of them happens once.     |
+------------------------------------------------------------------------------+
|Input Grammar Cast3-Language                                                  |
|Output Grammar Cast-with-lifted-types-Language                                |
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast-or-coerce3.rkt")

;; Only the pass is provided by this module
(provide hoist-types)

(: hoist-types (Cast-or-Coerce3-Lang Config -> Cast-or-Coerce3.1-Lang))
(define (hoist-types prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let* ([next : (Boxof Nat) (box next)]
           [type-table  (empty-type-table)]
           [type-index  (empty-type-index)]
           [exp  : CoC3.1-Expr (ht-expr next type-table type-index exp)]
           [type* (type-table->list type-table)]
           [next : Nat (unbox next)])
      (Prog (list name next type) (LetT* type* exp)))))

;; Type Index maps types to the identifiers for there binding site
(define-type Type-Index (HashTable Compact-Type (TypeId Uid)))

(: empty-type-index (-> Type-Index))
(define (empty-type-index) (make-hash))

;; Type-Table is essentially a mutable map that keeps types sorted by
;; there dependency rank.
(define-type Type-Table (Boxof (Vectorof CoC3.1-Bnd-Type*)))

(: empty-type-table (-> Type-Table))
(define (empty-type-table) (box (make-vector 10 (ann '() CoC3.1-Bnd-Type*))))

(: type-table->list (Type-Table -> CoC3.1-Bnd-Type*))
(define (type-table->list tt)
  ;; This is essentially and append-map over the vector
  (for*/list : (Listof CoC3.1-Bnd-Type)
             ([b* : CoC3.1-Bnd-Type* (in-vector (unbox tt))]
              [b  : CoC3.1-Bnd-Type  (in-list b*)])
    b))

(: ht-expr ((Boxof Nat) Type-Table Type-Index CoC3-Expr -> CoC3.1-Expr))
(define (ht-expr next type-table type-index exp)
  ;; create a new unique identifier
  (: next-uid! (String -> Uid))
  (define (next-uid! s)
    (let ([n (unbox next)])
      (set-box! next (+ n 1))
      (Uid s n)))

  ;; Add a binding to the type-table
  ;; possibly extending the type table it is not large enough
  (: type-table-add! (Nat CoC3.1-Bnd-Type -> Void))
  (define (type-table-add! n b)
    (let* ((v (unbox type-table))
           (l (vector-length v)))
      (if (< n l)
          (vector-set! v n (cons b (vector-ref v n))) 
          ;; There is a subtle argument to the correctness of this bit
          ;; essentially because of the nature of the the indicies used
          ;; each new n can at most equal to l
          (let ([new-v : (Vectorof CoC3.1-Bnd-Type*) (make-vector (* 2 l) '())])
            (vector-copy! new-v 0 v 0 l)
            (set-box! type-table new-v)
            (vector-set! new-v n (cons b '()))))))

  ;; If a Identical type has already been created find the name it is
  ;; bound to.
  (: find-type-identity! (Compact-Type Nat -> (values Prim-Type Nat)))
  (define (find-type-identity! t n)
    (let ([n+1 (add1 n)]
          [r? (hash-ref type-index t #f)])
      (if r?
          (values r? n+1)
          (let* ([u (next-uid! "typeid")]
                 [tid (TypeId u)])
            (type-table-add! n+1 (cons u t))
            (hash-set! type-index t tid)
            (values tid n+1)))))

  ;; Make all types of equal structure the same structure
  ;; this is essentially a compile time hash-consing of the types
  (: identify-type (Schml-Type -> Prim-Type))
  (define (identify-type t)
    ;; Here the Nonnegative Integer creates a partial order which
    ;; Identifies the order in which types can be constructed.
    ;; It can be seen as counting the number of types that must be
    ;; constructed before this type can be constructed.
    (: recur (Schml-Type -> (Values Prim-Type Nat)))
    (define (recur t)
      (match t
        [(Dyn)  (values DYN-TYPE  0)]
        [(Unit) (values UNIT-TYPE 0)]
        [(Int)  (values INT-TYPE  0)]
        [(Bool) (values BOOL-TYPE 0)]
        [(Fn ar (app recur* t* m) (app recur t n))
         (find-type-identity! (Fn ar t* t) (max m n))]
        [(GRef (app recur t n))
         (find-type-identity! (GRef t) n)]
        [(MRef (app recur t n))
         (find-type-identity! (MRef t) n)]
        [(GVect (app recur t n))
         (find-type-identity! (GVect t) n)]
        [(MVect (app recur t n))
         (find-type-identity! (MVect t) n)]
        [other (error 'hoist-types/identify-types "unmatched ~a" other)]))
    (: recur* ((Listof Schml-Type) -> (Values (Listof Prim-Type) Nat)))
    (define (recur* t*)
      (match t*
        ['() (values '() 0)]
        [(cons (app recur t m) (app recur* t* n))
         (values (cons t t*) (max n m))]))
    (let-values ([(t^ _) (recur t)])
      (logging identify-type (All) "~v ~v" t t^)
      t^))

  (: ht-coercion (Schml-Coercion -> Coercion/Prim-Type))
  (define (ht-coercion c)
    (: ht-coercion* (Schml-Coercion* -> Coercion/Prim-Type*))
    (define (ht-coercion* c*) (map ht-coercion c*))
    (match c
      ;; Just Return
      [(Identity) (Identity)]
      [(Failed l) (Failed l)]
      ;; Create New with Prim-Type
      [(Project (app identify-type t) l)
       (Project t l)]
      [(Inject (app identify-type t))
       (Inject t)]
      ;; Just Recur
      [(Sequence (app ht-coercion f) (app ht-coercion s))
       (Sequence f s)]
      [(Fn i (app ht-coercion* a*) (app ht-coercion r))
       (Fn i a* r)]
      [(Ref (app ht-coercion r) (app ht-coercion w))
       (Ref r w)]
      [(MonoRef (app identify-type t)) (MonoRef t)]
      [other (error 'hoist-types/coercion "unmatched ~a" other)]))
  
  ;; Recur through expression replacing types with their primitive counterparts
  (: recur (CoC3-Expr -> CoC3.1-Expr))
  (define (recur exp)
    (match exp
      ;; interesting cases
      [(Type (app identify-type t))
       (Type t)]
      [(Mbox (app recur e) (app identify-type t)) (Mbox e t)]
      [(Mvector (app recur e1) (app recur e2) (app identify-type t)) (Mvector e1 e2 t)]
      ;; Every other case is just a boring flow agnostic tree traversal
      [(Code-Label u)
       (Code-Label u)]
      [(Labels (app recur-bndc* b*) (app recur e))
       (Labels b* e)]
      [(App-Code (app recur e) (app recur* e*))
       (App-Code e e*)]
      [(Lambda f* (Castable c (app recur e)))
       (Lambda f* (Castable c e))]
      [(Fn-Caster (app recur e))
       (Fn-Caster e)]
      [(App-Fn (app recur e) (app recur* e*))
       (App-Fn e e*)]
      [(App-Fn-or-Proxy u (app recur e) (app recur* e*))
       (App-Fn-or-Proxy u e e*)]
      [(Fn-Proxy i (app recur e1) (app recur e2))
       (Fn-Proxy i e1 e2)]
      [(Fn-Proxy-Huh (app recur e))
       (Fn-Proxy-Huh e)]
      [(Fn-Proxy-Closure (app recur e))
       (Fn-Proxy-Closure e)]
      [(Fn-Proxy-Coercion (app recur e))
       (Fn-Proxy-Coercion e)]
          ;; Coercions
      [(Quote-Coercion (app ht-coercion c))
       (Quote-Coercion c)]
      [(Compose-Coercions (app recur e1) (app recur e2))
       (Compose-Coercions e1 e2)]
      [(Id-Coercion-Huh (app recur e))
       (Id-Coercion-Huh e)]
      [(Fn-Coercion-Huh (app recur e))
       (Fn-Coercion-Huh e)]
      [(Make-Fn-Coercion u (app recur e1)(app recur e2)(app recur e3))
       (Make-Fn-Coercion u e1 e2 e3)]
      [(Compose-Fn-Coercion u (app recur e1) (app recur e2))
       (Compose-Fn-Coercion u e1 e2)]
      [(Fn-Coercion (app recur* e*)(app recur e))
       (Fn-Coercion e* e)]
      [(Fn-Coercion-Arg (app recur e1)(app recur e2))
       (Fn-Coercion-Arg e1 e2)]
      [(Fn-Coercion-Return (app recur e))
       (Fn-Coercion-Return e)]
      [(Ref-Coercion (app recur e1) (app recur e2))
       (Ref-Coercion e1 e2)]
      [(Ref-Coercion-Huh (app recur e))
       (Ref-Coercion-Huh e)]
      [(Ref-Coercion-Read (app recur e))
       (Ref-Coercion-Read e)]
      [(Ref-Coercion-Write (app recur e))
       (Ref-Coercion-Write e)]
      [(Sequence-Coercion (app recur e1) (app recur e2))
       (Sequence-Coercion e1 e2)]
      [(Sequence-Coercion-Huh (app recur e))
       (Sequence-Coercion-Huh e)]
      [(Sequence-Coercion-Fst (app recur e))
       (Sequence-Coercion-Fst e)]
      [(Sequence-Coercion-Snd (app recur e))
       (Sequence-Coercion-Snd e)]
      [(Project-Coercion (app recur e1) (app recur e2))
       (Project-Coercion e1 e2)]
      [(Project-Coercion-Huh (app recur e))
       (Project-Coercion-Huh e)]
      [(Project-Coercion-Type (app recur e))
       (Project-Coercion-Type e)]
      [(Project-Coercion-Label (app recur e))
       (Project-Coercion-Label e)]
      [(Inject-Coercion (app recur e))
       (Inject-Coercion e)]
      [(Inject-Coercion-Type (app recur e))
       (Inject-Coercion-Type e)]
      [(Inject-Coercion-Huh (app recur e))
       (Inject-Coercion-Huh e)]
      [(Failed-Coercion (app recur e))
       (Failed-Coercion e)]
      [(Failed-Coercion-Huh (app recur e))
       (Failed-Coercion-Huh e)]
      [(Failed-Coercion-Label (app recur e))
       (Failed-Coercion-Label e)]
      [(Type-Dyn-Huh (app recur e))
       (Type-Dyn-Huh e)] 
      [(Type-Fn-arg (app recur e) (app recur i))
       (Type-Fn-arg e i)]
      [(Type-Fn-return (app recur e))
       (Type-Fn-return e)]
      [(Type-Fn-arity (app recur e))
       (Type-Fn-arity e)]
      [(Type-Fn-Huh (app recur e))
       (Type-Fn-Huh e)]
      [(Type-GRef-Of (app recur e))
       (Type-GRef-Of e)]
      [(Type-GVect-Of (app recur e))
       (Type-GVect-Of e)]
      [(Type-GRef-Huh (app recur e))
       (Type-GRef-Huh e)]
      [(Type-GVect-Huh (app recur e))
       (Type-GVect-Huh e)]
      [(Type-Tag (app recur e))
       (Type-Tag e)]
      [(Tag s)
       (Tag s)]
      [(Dyn-tag (app recur e))
       (Dyn-tag e)]
      [(Dyn-immediate (app recur e))
       (Dyn-immediate e)]
      [(Dyn-type (app recur e))
       (Dyn-type e)]
      [(Dyn-value (app recur e))
       (Dyn-value e)]
      [(Dyn-make (app recur e1) (app recur e2))
       (Dyn-make e1 e2)]
      [(Letrec (app recur-bnd* b*) (app recur e))
       (Letrec b* e)]
      [(Let (app recur-bnd* b*) (app recur e))
       (Let b* e)]
      [(Var i)
       (Var i)]
      [(If (app recur t) (app recur c) (app recur a))
       (If t c a)]
      [(Begin (app recur* e*) (app recur e))
       (Begin e* e)]
      [(Repeat i (app recur e1) (app recur e2) (app recur e3))
       (Repeat i e1 e2 e3)]
      [(Op p (app recur* e*))
       (Op p e*)]
      [(Quote k)
       (Quote k)]
      [(Blame (app recur e))
       (Blame e)]
      [(Observe (app recur e) t)
       (Observe e t)]
      [(Unguarded-Box (app recur exp))
       (Unguarded-Box exp)]
      [(Unguarded-Box-Ref (app recur exp))
       (Unguarded-Box-Ref exp)]
      [(Unguarded-Box-Set! (app recur exp1) (app recur exp2))
       (Unguarded-Box-Set! exp1 exp2)]
      [(Unguarded-Vect (app recur exp1) (app recur exp2))
       (Unguarded-Vect exp1 exp2)]
      [(Unguarded-Vect-Ref (app recur exp1) (app recur exp2))
       (Unguarded-Vect-Ref exp1 exp2)]
      [(Unguarded-Vect-Set! (app recur exp1) (app recur exp2) (app recur exp3))
       (Unguarded-Vect-Set! exp1 exp2 exp3)]
      [(Guarded-Proxy-Huh (app recur exp))
       (Guarded-Proxy-Huh exp)]
      [(Guarded-Proxy (app recur e1) r)
       (match r
         [(Twosome (app recur e2) (app recur e3) (app recur e4))
          (Guarded-Proxy e1 (Twosome e2 e3 e4))]
         [(Coercion (app recur e2))
          (Guarded-Proxy e1 (Coercion e2))])]
      [(Guarded-Proxy-Ref (app recur exp))
       (Guarded-Proxy-Ref exp)]
      [(Guarded-Proxy-Source (app recur exp))
       (Guarded-Proxy-Source exp)]
      [(Guarded-Proxy-Target (app recur exp))
       (Guarded-Proxy-Target exp)]
      [(Guarded-Proxy-Blames (app recur exp))
       (Guarded-Proxy-Blames exp)]
      [(Guarded-Proxy-Coercion (app recur exp))
       (Guarded-Proxy-Coercion exp)]
      [(CastedValue-Huh (app recur exp))
       (CastedValue-Huh exp)]
      [(CastedValue (app recur e1) r)
       (match r
         [(Twosome (app recur e2) (app recur e3) (app recur e4))
          (CastedValue e1 (Twosome e2 e3 e4))]
         [(Coercion (app recur e2))
          (CastedValue e1 (Coercion e2))])]
      [(CastedValue-Value (app recur exp))
       (CastedValue-Value exp)]
      [(CastedValue-Source (app recur exp))
       (CastedValue-Source exp)]
      [(CastedValue-Target (app recur exp))
       (CastedValue-Target exp)]
      [(CastedValue-Blames (app recur exp))
       (CastedValue-Blames exp)]
      [(CastedValue-Coercion (app recur exp))
       (CastedValue-Coercion exp)]
      [(Mbox-val-set! (app recur e1) (app recur e2)) (Mbox-val-set! e1 e2)]
      [(Mbox-val-ref (app recur e)) (Mbox-val-ref e)]
      [(Mbox-rtti-set! u (app recur e)) (Mbox-rtti-set! u e)]
      [(Mbox-rtti-ref u) (Mbox-rtti-ref u)]
      [(Mvector-val-set! (app recur e1) (app recur e2) (app recur e3)) (Mvector-val-set! e1 e2 e3)]
      [(Mvector-val-ref (app recur e1) (app recur e2)) (Mvector-val-ref e1 e2)]
      [(Mvector-rtti-set! u (app recur e)) (Mvector-rtti-set! u e)]
      [(Mvector-rtti-ref u) (Mvector-rtti-ref u)]
      [(Make-Fn-Type e1 (app recur e2) (app recur e3)) (Make-Fn-Type e1 e2 e3)]
      [(MRef-Coercion-Huh (app recur e)) (MRef-Coercion-Huh e)]
      [(MRef-Coercion-Type (app recur e)) (MRef-Coercion-Type e)]
      [(MRef-Coercion (app recur e)) (MRef-Coercion e)]
      [(Type-GRef (app recur e)) (Type-GRef e)]
      [(Type-GVect (app recur e)) (Type-GVect e)]
      [(Type-MRef (app recur e)) (Type-MRef e)]
      [(Type-MRef-Huh (app recur e)) (Type-MRef-Huh e)]
      [(Type-MRef-Of (app recur e)) (Type-MRef-Of e)]
      [(Error (app recur e)) (Error e)]
      [other (error 'hoist-types/expr "unmatched ~a" other)]))
  ;; Recur through other type containing ast forms
  (: recur* (CoC3-Expr* -> CoC3.1-Expr*))
  (define (recur* e*) (map recur e*))
  
  (: recur-bnd* (CoC3-Bnd* -> CoC3.1-Bnd*))
  (define (recur-bnd* b*)
    (map
     (lambda ([b : CoC3-Bnd])
       (cons (car b) (recur (cdr b))))
     b*))

  (: recur-bndc* (CoC3-Bnd-Code* -> CoC3.1-Bnd-Code*))
  (define (recur-bndc* b*)
    (map
     (lambda ([b : CoC3-Bnd-Code])
       (match-let ([(cons a (Code u* (app recur e))) b])
         (cons a (Code u* e))))
     b*))
  
  ;; Body of ht-expr just start the expression traversal
  (recur exp))
