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
         "../language.rkt")

;; Only the pass is provided by this module
(provide hoist-types)

(: hoist-types (Cast3-Lang Config -> Cast-with-hoisted-types))
(define (hoist-types prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let* ([next : (Boxof Nat) (box next)]
           [type-table  (empty-type-table)]
           [type-index  (empty-type-index)]
           [exp  : C/LT-Expr (ht-expr next type-table type-index exp)]
           [type* (type-table->list type-table)]
           [next : Nat (unbox next)])
      (Prog (list name next type) (LetT* type* exp)))))

;; Type Index maps types to the identifiers for there binding site
(define-type Type-Index (HashTable Compact-Type (TypeId Uid)))

(: empty-type-index (-> Type-Index))
(define (empty-type-index) (make-hash))

;; Type-Table is essentially a mutable map that keeps types sorted by
;; there dependency rank.
(define-type Type-Table (Boxof (Vectorof C/LT-BndT*)))

(: empty-type-table (-> Type-Table))
(define (empty-type-table) (box (make-vector 10 (ann '() C/LT-BndT*))))

(: type-table->list (Type-Table -> C/LT-BndT*))
(define (type-table->list tt)
  ;; This is essentially and append-map over the vector
  (for*/list : (Listof C/LT-BndT)
             ([b* : C/LT-BndT* (in-vector (unbox tt))]
              [b  : C/LT-BndT  (in-list b*)])
    b))

(: ht-expr ((Boxof Nat) Type-Table Type-Index C3-Expr -> C/LT-Expr))
(define (ht-expr next type-table type-index exp)
  ;; create a new unique identifier
  (: next-uid! (String -> Uid))
  (define (next-uid! s)
    (let ([n (unbox next)])
      (set-box! next (+ n 1))
      (Uid s n)))

  ;; Add a binding to the type-table
  ;; possibly extending the type table it is not large enough
  (: type-table-add! (Nat C/LT-BndT -> Void))
  (define (type-table-add! n b)
    (let* ((v (unbox type-table))
           (l (vector-length v)))
      (if (< n l)
          (vector-set! v n (cons b (vector-ref v n))) 
          ;; There is a subtle argument to the correctness of this bit
          ;; essentially because of the nature of the the indicies used
          ;; each new n can at most equal to l
          (let ([new-v : (Vectorof C/LT-BndT*) (make-vector (* 2 l) '())])
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
    (: recur* ((Listof Schml-Type) -> (Values (Listof Prim-Type) Nat)))
    (define (recur* t*)
      (match t*
        ['() (values '() 0)]
        [(cons (app recur t m) (app recur* t* n))
         (values (cons t t*) (max n m))]))
    (logging identify-type (All) "~v" t)
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
    (let-values ([(t _) (recur t)])
      t))

  ;; Recur through expression replacing types with their primitive counterparts
  (: recur (C3-Expr -> C/LT-Expr))
  (define (recur exp)
    (match exp
      ;; The only expression of any inportance in the pass
      [(Type (app identify-type t))
       (Type t)]
      ;; Every other case is just a boring flow agnostic tree traversal
      [(Lambda f* (Castable c (app recur e)))
       (Lambda f* (Castable c e))]
      [(Letrec (app recur-bnd* b*) (app recur e))
       (Letrec b* e)]
      [(Let (app recur-bnd* b*) (app recur e))
       (Let b* e)]
      [(App (app recur e) (app recur* e*))
       (App e e*)]
      [(Op p (app recur* e*))
       (Op p e*)]
      [(If (app recur t) (app recur c) (app recur a))
       (If t c a)]
      [(Begin (app recur* e*) (app recur e))
       (Begin e* e)]
      [(Repeat i (app recur e1) (app recur e2) (app recur e3))
       (Repeat i e1 e2 e3)]
      [(Tag s)
       (Tag s)]
      [(Var i)
       (Var i)]
      [(Quote k)
       (Quote k)]
      [(Fn-Caster (app recur e))
       (Fn-Caster e)]
      [(Type-tag (app recur e))
       (Type-tag e)]
      [(Type-Fn-arg (app recur e) (app recur i))
       (Type-Fn-arg e i)]
      [(Type-Fn-return (app recur e))
       (Type-Fn-return e)]
      [(Type-Fn-arity (app recur e))
       (Type-Fn-arity e)]
      [(Type-GRef-to (app recur e))
       (Type-GRef-to e)]
      [(Type-GVect-to (app recur e))
       (Type-GVect-to e)]
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
      [(Blame (app recur e))
       (Blame e)]
      [(Observe (app recur e) t)
       (Observe e t)]
      [(UGbox (app recur exp))
       (UGbox exp)]
      [(UGbox-ref (app recur exp))
       (UGbox-ref exp)]
      [(UGbox-set! (app recur exp1) (app recur exp2))
       (UGbox-set! exp1 exp2)]
      [(UGvect (app recur exp1) (app recur exp2))
       (UGvect exp1 exp2)]
      [(UGvect-ref (app recur exp1) (app recur exp2))
       (UGvect-ref exp1 exp2)]
      [(UGvect-set! (app recur exp1) (app recur exp2) (app recur exp3))
       (UGvect-set! exp1 exp2 exp3)]
      [(GRep-proxied? (app recur exp))
       (GRep-proxied? exp)]
      [(Gproxy (app recur e1) (app recur e2) (app recur e3) (app recur e4))
       (Gproxy e1 e2 e3 e4)]
      [(Gproxy-for (app recur exp))
       (Gproxy-for exp)]
      [(Gproxy-from (app recur exp))
       (Gproxy-from exp)]
      [(Gproxy-to (app recur exp))
       (Gproxy-to exp)]
      [(Gproxy-blames (app recur exp))
       (Gproxy-blames exp)]
      [other (error 'hoist-types/expr "unmatched ~a" other)]))
  ;; Recur through other type containing ast forms
  (: recur* (C3-Expr* -> C/LT-Expr*))
  (define (recur* e*) (map recur e*))
  
  (: recur-bnd* (C3-Bnd* -> C/LT-Bnd*))
  (define (recur-bnd* b*)
    (map
     (lambda ([b : C3-Bnd])
       (cons (car b) (recur (cdr b))))
     b*))
  ;; Body of ht-expr just start the expression traversal
  (recur exp))
