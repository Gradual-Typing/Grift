#lang typed/racket/base
#|-----------------------------------------------------------------------------+
|Pass: src/casts/hoist-types-and-coercions                                     |
+------------------------------------------------------------------------------+
|Author:                                                                       |
| (original hoist-types)  Deyaaeldeen Almahallawi (dalmahal@indiana.edu)       |
| (hoist coercions added) Andre Kuhlenschmidt (akuhlens@indiana.edu)           |
+------------------------------------------------------------------------------+
|Description: Hoist the allocation of all types and static coercions to the 
| initialization of the program. Unify there allocations so that any two
| structurally similar structures are the same runtime object. Coercions may
| still allocate new coercions that are equivalent to the hoisted, so runtime
| hashconsing of types is still needed to ensure pointer
| equality is the same as type equality. This pass greatly reduces the amount
| of runtime type allocation. 
+------------------------------------------------------------------------------+
| Input Grammar Cast or Coercion Language 3
| Output Grammar Cast or Coercion Language 3.1
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
(require
 (submod  "../logging.rkt" typed)
 "../growable-vector.rkt"
 "../unique-counter.rkt"
 "../errors.rkt"
 "../type-equality.rkt"
 "../language/cast-or-coerce3.rkt"
 "../language/cast-or-coerce3.1.rkt"
 racket/match
 racket/list)

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

(assert-subtype? (HT&C+ CoC3.1-Expr) CoC3.1-Expr)
(assert-subtype? (HT&C= CoC3.1-Expr) CoC3.1-Expr)
(assert-subtype? (U (HT&C= CoC3.1-Expr) (HT&C+ CoC3.1-Expr))
                 CoC3.1-Expr)
(assert-subtype? CoC3.1-Expr
                 (U (HT&C= CoC3.1-Expr) (HT&C+ CoC3.1-Expr)))
(assert-subtype? (HT&C- CoC3-Expr) CoC3-Expr)
(assert-subtype? (HT&C= CoC3-Expr) CoC3-Expr)
(assert-subtype? (U (HT&C- CoC3-Expr) (HT&C= CoC3-Expr))
                 CoC3-Expr)
(assert-subtype? CoC3-Expr
                 (U (HT&C- CoC3-Expr) (HT&C= CoC3-Expr)))


(define-type Immediate-Coercion* (Listof Immediate-Coercion))

;; Only the pass is provided by this module
(provide hoist-types-and-coercions)

(: hoist-types-and-coercions : Cast-or-Coerce3-Lang -> Cast-or-Coerce3.1-Lang)
(define (hoist-types-and-coercions prgm)
  (match-define (Prog (list prgm-name next-unique-number prgm-type)
                  (Static* (list bnd-const*) exp))
    prgm)

  ;; All mutable state of the pass
  (define unique-state : Unique-Counter (make-unique-counter next-unique-number))
  (define type-table   : Type-Table   (make-type-table))
  (define crcn-table   : Crcn-Table   (make-crcn-table))
  
  (define type->immediate (make-identify-type! type-table))
  (define coercion->immediate
    (make-identify-coercion! type->immediate crcn-table))

  (: new-exp CoC3.1-Expr)
  (: new-bnd-const* CoC3.1-Bnd*)
  (define-values (new-exp new-bnd-const*)
    ;; Map through the expression mutating the above state to collect
    ;; all type and coercions.
    (parameterize ([current-unique-counter unique-state])
      (: map-expr : CoC3-Expr -> CoC3.1-Expr)
      (define (map-expr e)
        (map-hoisting-thru-Expr type->immediate coercion->immediate e))
      (values
       (map-expr exp)
       (for/list ([b bnd-const*])
         (match-define (cons id exp) b)
         (cons id (map-expr exp))))))

  (define bnd-mu-type* (table-mus type-table))
  (define bnd-type* (table->list type-table))
  (define bnd-mu-crcn* (table-mus crcn-table))
  (define bnd-crcn* (table->list crcn-table))
  ;; Compose the mapping with extracting the state of the computation
  ;; to build a new program.  
  (Prog (list prgm-name (unique-counter-next! unique-state) prgm-type)
    (Static* (list
              bnd-mu-type*
              bnd-type*
              bnd-mu-crcn*
              bnd-crcn*
              new-bnd-const*)
             new-exp)))

;; Type Cache maps grift types to an immediate-type that is the
;; runtime representation of that type

;; Mu-Type are the only Back Edges in types Currently so by
;; declaring them first they become accessable to initialize
;; other runtime types. Their initialization can then be performed
;; after all other types have been declared. If we wanted to
;; generallize this we could treat type building like closure
;; creation, where all types get allocated and then are stiched
;; together. We could then generalized to doing so for strongly
;; connected components. 

;; Sorted-Bnd-Type then stores types in a partial order that
;; that will ensure that all prerequisites to build a type are
;; already declared.

;; The Crcn-Table is a mirror of this but for coercions. 

(define-type (Table Src Cpt Imm)
  (List (Cache Src Imm) (Mu!* Imm) (Sorted-Bnd* Cpt)))

(define-type (Cache Src Imm) (HashTable Src (Pair Imm Nat)))

(define-type (Mu!* Imm) (Boxof (Listof (Pair Uid (Mu Imm)))))

(define-type (Sorted-Bnd* Cpt) (GVectorof (Listof (Pair Uid Cpt))))

(define-type Type-Table (Table Grift-Type Compact-Type Immediate-Type))

(define-type Type-Cache (Cache Grift-Type Immediate-Type))

(define-type Mu-Type* (Mu!* Immediate-Type))

(define-type Sorted-Bnd-Type* (Sorted-Bnd* Compact-Type))

(define-type Crcn-Table
  (Table Mixed-Coercion Compact-Coercion Immediate-Coercion))

(define-type Crcn-Cache (Cache Mixed-Coercion Immediate-Coercion))

(define-type Mu-Crcn* (Mu!* Immediate-Coercion))

(define-type Sorted-Bnd-Crcn* (Sorted-Bnd* Compact-Coercion))


(: make-type-table : -> Type-Table)
(define (make-type-table)
  (list (ann (make-hash) Type-Cache)
        (ann (box '())   Mu-Type*)
        (ann (make-gvector 8 '()) Sorted-Bnd-Type*)))

(: make-crcn-table : -> Crcn-Table)
(define (make-crcn-table)
  (list (ann (make-hash) Crcn-Cache) 
        (ann (box '()) Mu-Crcn*)
        (ann (make-gvector 8 '()) Sorted-Bnd-Crcn*)))

(: table->list
   (All (Src Cpt Imm)
        (Table Src Cpt Imm) -> (Listof (Pair Uid Cpt))))
(define (table->list tt)
  (append* (gvector->list (car (cdr (cdr tt))))))

(: table-mus
   (All (Src Cpt Imm)
        (Table Src Cpt Imm) -> (Listof (Pair Uid (Mu Imm)))))
(define (table-mus tt)
  (unbox (car (cdr tt))))

(: sorted-bnd*-add! (All (A) (Sorted-Bnd* A) Nat (Pair Uid A) -> Void))
(define (sorted-bnd*-add! bnd* rank new-bnd)
  (define old-bnd* : (Listof (Pair Uid A)) 
    (if (< rank (gvector-length bnd*))
        (gvector-ref bnd* rank)
        '()))
  (gvector-set! bnd* rank (cons new-bnd old-bnd*)))

(: boxed-list-cons! (All (A) A (Boxof (Listof A)) -> Void))
(define (boxed-list-cons! a d)
  (set-box! d (cons a (unbox d))))

;; Convert a compact repr to an immediate repr by naming it.
;; This adds the repr to a list of bindings that are going to
;; be allocated at the start of the program.
(: name! (All (A) (Sorted-Bnd* A) String A Nat -> (values (Static-Id Uid) Nat)))
(define (name! sb* name type rank)
  (define uid (next-uid! name))
  (sorted-bnd*-add! sb* rank (cons uid type))
  (values (Static-Id uid) (+ 1 rank)))

(: make-recur*
   (All (Src Imm)
        (Src -> (Values Imm Nat))
        -> (Listof Src) -> (Values (Listof Imm) Nat)))
(define ((make-recur* recur) t*)
  (define-values (i* r*)
    (for/lists ([i* : (Listof Imm)] [r : (Listof Nat)])
               ([t t*])
      (recur t)))
  (values i* (apply max (cons 0 r*))))

;; This function memoizes the fixpoint of the compile type
;; hash consing which ensures that calls containing the
;; structuraly equal grift values get the same runtime
;; value.
(: make-identify! :
   (All (Src Cpt Imm)
        (Table Src Cpt Imm)
        ((Mu!* Imm) (Sorted-Bnd* Cpt) (Src -> (Values Imm Nat))
         -> (Src -> (Values Imm Nat)))
        -> (Src -> Imm)))
(define (make-identify! table make-src->imm)
  (match-define (list c mu* sb*) table)
  ;; this function is the one that needs to be called at all
  ;; recursive calls of f that memoizes the this ensures that
  ;; all syntactically identical types end up being contructed
  ;; as the same time. We still need to come up with some way
  ;; of compile type hashconsing unrolled versions of types.
  (: recur : Src -> (Values Imm Nat))
  (define (recur t)
    (cond
      [(hash-ref c t #f)
       =>
       (lambda ([p : (Pair Imm Nat)])
         (values (car p) (cdr p)))]
      [else
       (define-values (ret-i ret-r) (src->imm t))
       (hash-set! c t (cons ret-i ret-r))
       (values ret-i ret-r)]))

  (: src->imm : Src -> (Values Imm Nat))
  (define src->imm (make-src->imm mu* sb* recur))
  
  (lambda ([t : Src])
    (let-values ([(i _) (recur t)])
      i)))

;; Make all types of equal structure the same structure
;; this is essentially a compile time hash-consing of the types
(: make-identify-type! : Type-Table -> (Grift-Type -> Immediate-Type))
(define (make-identify-type! tt)
  (: make-id-type! :
     (Mu!* Immediate-Type)
     (Sorted-Bnd* Compact-Type)
     (Grift-Type -> (Values Immediate-Type Nat))
     -> (Grift-Type -> (Values Immediate-Type Nat)))
  (define (make-id-type! mu* sb* recur)
    (define recur* (make-recur* recur))
    (lambda ([t : Grift-Type])
      (match t
        [(Mu s)
         (define uid (next-uid! "static_mu_type"))
         (define t (grift-type-instantiate s (FVar uid)))
         (define-values (i _) (recur t))
         (boxed-list-cons! (cons uid (Mu i)) mu*)
         (values (Static-Id uid) 0)]
        [(FVar id) (values (Static-Id id) 0)]
        [(Dyn)  (values DYN-TYPE 0)]
        [(Unit) (values UNIT-TYPE 0)]
        [(Int)  (values INT-TYPE 0)]
        [(Bool) (values BOOL-TYPE 0)]
        [(Float) (values FLOAT-TYPE 0)]
        [(Character) (values CHAR-TYPE 0)]
        [(Fn ar (app recur* t* m) (app recur t0 n))
         (define rank (max m n))
         (define ty (Fn ar t* t0))
         (name! sb* "static_fn_type" ty rank)]
        [(GRef (app recur t n))
         (name! sb* "static_gref_type" (GRef t) n)]
        [(MRef (app recur t n))
         (name! sb* "static_mref_type" (MRef t) n)]
        [(GVect (app recur t n))
         (name! sb* "static_gvect_type" (GVect t) n)]
        [(MVect (app recur t n))
         (name! sb* "static_mvect_type" (MVect t) n)]
        [(STuple arity (app recur* t* n))
         (name! sb* "static_tuple_type" (STuple arity t*) n)])))
  (make-identify! tt make-id-type!))

(: make-identify-coercion! :
   (Grift-Type -> Immediate-Type) Crcn-Table
   -> (Mixed-Coercion -> Immediate-Coercion))
(define (make-identify-coercion! idt! ct)
  (: make-id-coercion! :
     (Mu!* Immediate-Coercion)
     (Sorted-Bnd* Compact-Coercion)
     (Mixed-Coercion -> (Values Immediate-Coercion Nat))
     -> (Mixed-Coercion -> (Values Immediate-Coercion Nat)))
  (define (make-id-coercion! mu* sb* recur)
    (: recur* : Mixed-Coercion* -> (Values Immediate-Coercion* Nat))
    (define recur* (make-recur* recur))
    (define name-crcn! (inst name! Compact-Coercion))
    ;; This is hacky we should do better
    (define env (make-hasheq))
    (lambda ([c : Mixed-Coercion])
      (match c
        [(CRec uid c)
         (define-values (i _) (recur c))
         (boxed-list-cons! (cons uid (Mu i)) mu*)
         (values (Static-Id uid) 0)]
        [(CVar uid) (values (Static-Id uid) 0)]
        [(Identity)
         (values (Identity) 0)]
        [(Failed l)
         (name-crcn! sb* "static_failed_crcn" (Failed l) 0)]
        [(Project (app idt! t) l)
         (name-crcn! sb* "static_project_crcn" (Project t l) 0)]
        [(Inject (app idt! t)) 
         (name-crcn! sb* "static_inject_crcn" (Inject t) 0)]
        [(Sequence (app recur f m) (app recur s n))
         (name-crcn! sb* "static_seq_crcn" (Sequence f s) (max m n))]
        [(HC p? (app idt! t1) l? i? (app idt! t2) (app recur m n))
         (name-crcn! sb* "static_hc" (HC p? t1 l? i? t2 m) n)]
        [(Fn i (app recur* a* m) (app recur r n))
         (name-crcn! sb* "static_fn_crcn" (Fn i a* r) (max m n))]
        [(Ref (app recur r m) (app recur w n) flag)
         (name-crcn! sb* "static_ref_crcn" (Ref r w flag) (max m n))]
        [(MonoRef (app idt! t))
         (name-crcn! sb* "static_mref_crcn" (MonoRef t) 0)]
        [(MonoVect (app idt! t))
         (name-crcn! sb* "static_mvect_crcn" (MonoVect t) 0)]
        [(CTuple i (app recur* a* m))
         (name-crcn! sb* "static_tuple_crcn" (CTuple i a*) m)])))
  (make-identify! ct make-id-coercion!))

(require typed/racket/unsafe)
(unsafe-require/typed
 "../language/form-map.rkt"
 [form-map ((HT&C= CoC3-Expr)
            (CoC3-Expr -> CoC3.1-Expr)
            -> (HT&C= CoC3.1-Expr))])

(: map-hoisting-thru-Expr :
   (Grift-Type     -> Immediate-Type)
   (Mixed-Coercion -> Immediate-Coercion)
   CoC3-Expr
   -> CoC3.1-Expr)
;; Recur through all valid language forms collecting the types
;; in the type table and replacing them with references to the
;; global identifiers
(define (map-hoisting-thru-Expr type->imdt crcn->imdt exp)
  ;; Recur through expression replacing types with their primitive counterparts
  (: recur (CoC3-Expr -> CoC3.1-Expr))
  (define (recur exp)
    (cond
      [(Type? exp)
       (match-define (Type (app type->imdt t)) exp)
       (Type t)]
      [(Quote-Coercion? exp)
       (match-define (Quote-Coercion (app crcn->imdt c)) exp)
       (Quote-Coercion c)]
      [(Quote-HCoercion? exp)
       (match-define (Quote-HCoercion (app crcn->imdt c)) exp)
       (Quote-Coercion c)]
      [(Mvector? exp)
       (match-define (Mvector (app recur e1) (app recur e2) (app type->imdt t))
         exp)
       (Mvector e1 e2 t)]
      [(Mbox? exp)
       (match-define (Mbox (app recur e) (app type->imdt t)) exp)
       (Mbox e t)]
      [else (form-map exp recur)]))
  ;; Body of ht-expr just start the expression traversal
  (recur exp))

(module+ test
  (require typed/rackunit)
  (define f1 (Fn 0 '() DYN-TYPE))
  (define f2 (Fn 0 '() f1))
  (define tt : Type-Table (make-type-table))
  (define t->i (make-identify-type! tt))
  (current-unique-counter (make-unique-counter 0))
  (void (t->i f2)) 
  (check-equal? (table->list tt)
                (list
                 (cons (Uid "static_fn_type" 0) (Fn 0 '() (Dyn)))
                 (cons
                  (Uid "static_fn_type" 1)
                  (Fn 0 '() (Static-Id (Uid "static_fn_type" 0))))))
  )
