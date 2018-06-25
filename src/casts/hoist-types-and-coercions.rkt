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
  (match-define (Prog (list prgm-name next-unique-number prgm-type) exp)
    prgm)

  ;; All mutable state of the pass
  (define unique-state : Unique-Counter (make-unique-counter next-unique-number))
  (define type-table   : Type-Table   (make-type-table))
  (define crcn-table   : Crcn-Table   (make-crcn-table))
  
  (define type->immediate (make-identify-type! type-table))
  (define coercion->immediate
    (make-identify-coercion! type->immediate crcn-table))

  (define new-exp : CoC3.1-Expr
    ;; Map through the expression mutating the above state to collect
    ;; all type and coercions.
    (parameterize ([current-unique-counter unique-state])
      (map-hoisting-thru-Expr type->immediate coercion->immediate exp)))

  (define bnd-mu-type* (table-mus type-table))
  (define bnd-type* (table->list type-table))
  (define bnd-mu-crcn* (table-mus crcn-table))
  (define bnd-crcn* (table->list crcn-table))
  #;(debug std bnd-mu-type* bnd-type* bnd-mu-crcn* bnd-crcn*)
  ;; Compose the mapping with extracting the state of the computation
  ;; to build a new program.  
  (Prog (list prgm-name (unique-counter-next! unique-state) prgm-type)
    (Let-Static* bnd-mu-type* bnd-type*
                 bnd-mu-crcn* bnd-crcn*
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
    (define p? (hash-ref c t #f))
    (cond
      [p? (values (car p?) (cdr p?))]
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
    (lambda ([c : Mixed-Coercion])
      (match c
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
    ;;(printf "ht: ~a\n" exp) (flush-output (current-output-port))
    (match exp
      ;; Interesting cases
      [(Type (app type->imdt t)) (Type t)]
      [(Quote-Coercion (app crcn->imdt c)) (Quote-Coercion c)]
      [(Quote-HCoercion (app crcn->imdt c)) (Quote-Coercion c)]
      [(Mvector (app recur e1) (app recur e2) (app type->imdt t))
       (Mvector e1 e2 t)]
      [(Mbox (app recur e) (app type->imdt t)) (Mbox e t)]
      ;; Every other case is just a boring flow agnostic tree traversal
      [(Construct t v (app recur* e*))
       (Construct t v e*)]
      [(Access t f (app recur e) i?)
       (Access t f e (if i? (recur i?) #f))]
      [(Check t p (app recur e) (app recur* e*))
       (Check t p e e*)]
      [(Code-Label u)
       (Code-Label u)]
      [(Labels (app recur-bnd-code* b*) (app recur e))
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
      #|
      [(Compose-Coercions (app recur e1) (app recur e2))
       (Compose-Coercions e1 e2)]
      |#
      [(HC (app recur p?) (app recur t1) (app recur lbl)
           (app recur i?) (app recur t2)
           (app recur m))
       (HC p? t1 lbl i? t2 m)]
      [(HC-Inject-Huh (app recur h)) (HC-Inject-Huh h)]
      [(HC-Project-Huh (app recur h)) (HC-Project-Huh h)]
      [(HC-Identity-Huh (app recur h)) (HC-Identity-Huh h)]
      [(HC-Label (app recur h)) (HC-Label h)]
      [(HC-T1 (app recur h)) (HC-T1 h)]
      [(HC-T2 (app recur h)) (HC-T2 h)]
      [(HC-Med (app recur h)) (HC-Med h)]
      [(Id-Coercion-Huh (app recur e))
       (Id-Coercion-Huh e)]
      [(Fn-Coercion-Huh (app recur e))
       (Fn-Coercion-Huh e)]
      [(Make-Fn-Coercion u (app recur e1)(app recur e2)(app recur e3))
       (Make-Fn-Coercion u e1 e2 e3)]
      [(Fn-Coercion (app recur* e*)(app recur e))
       (Fn-Coercion e* e)]
      [(Fn-Coercion-Arity (app recur e))
       (Fn-Coercion-Arity e)]
      [(Fn-Coercion-Arg (app recur e1)(app recur e2))
       (Fn-Coercion-Arg e1 e2)]
      [(Fn-Coercion-Return (app recur e))
       (Fn-Coercion-Return e)]
      [(Id-Fn-Coercion (app recur a)) (Id-Fn-Coercion a)]
      [(Fn-Coercion-Arg-Set! (app recur f) (app recur i) (app recur a))
       (Fn-Coercion-Arg-Set! f i a)]
      [(Fn-Coercion-Return-Set! (app recur f) (app recur r))
       (Fn-Coercion-Return-Set! f r)]
      [(Tuple-Coercion-Item-Set! (app recur t) (app recur i) (app recur e))
       (Tuple-Coercion-Item-Set! t i e)]
      [(Id-Tuple-Coercion (app recur a))
       (Id-Tuple-Coercion a)]
      [(Ref-Coercion (app recur e1) (app recur e2) (app recur flag))
       (Ref-Coercion e1 e2 flag)]
      [(Ref-Coercion-Huh (app recur e))
       (Ref-Coercion-Huh e)]
      [(Ref-Coercion-Read (app recur e))
       (Ref-Coercion-Read e)]
      [(Ref-Coercion-Write (app recur e))
       (Ref-Coercion-Write e)]
      [(Ref-Coercion-Ref-Huh (app recur e))
       (Ref-Coercion-Ref-Huh e)]
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
      [(Type-Mu-Huh (app recur e))
       (Type-Mu-Huh e)]
      [(Type-Mu-Body (app recur e))
       (Type-Mu-Body e)]
      #| 
      [(Type-Tag (app recur e))
      (Type-Tag e)]
      [(Tag s)
      (Tag s)] 
      |#
      [(Letrec (app recur-bnd* b*) (app recur e))
       (Letrec b* e)]
      [(Let (app recur-bnd* b*) (app recur e))
       (Let b* e)]
      [(Var i)
       (Var i)]
      [(Global s)
       (Global s)]
      [(Assign u/s (app recur e))
       (Assign u/s e)]
      [(If (app recur t) (app recur c) (app recur a))
       (If t c a)]
      [(Switch e c* d)
       (: recur-case : (Switch-Case CoC3-Expr) -> (Switch-Case CoC3.1-Expr))
       (define/match (recur-case c)
         [((cons l r)) (cons l (recur r))])
       (Switch (recur e) (map recur-case c*) (recur d))]
      [(Begin (app recur* e*) (app recur e))
       (Begin e* e)]
      [(Repeat i (app recur e1) (app recur e2) a (app recur e3) (app recur e4))
       (Repeat i e1 e2 a e3 e4)]
      [(Break-Repeat) (Break-Repeat)]
      [(Op p (app recur* e*))
       (Op p e*)]
      [(Quote k) (Quote k)]
      [(Blame (app recur e))
       (Blame e)]
      [(Observe (app recur e) t)
       (Observe e t)]
      [(and nop (No-Op)) nop]
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
      [(Unguarded-Vect-length e) (Unguarded-Vect-length (recur e))]
      [(Mbox-val-set! (app recur e1) (app recur e2)) (Mbox-val-set! e1 e2)]
      [(Mbox-val-ref (app recur e)) (Mbox-val-ref e)]
      [(Mbox-rtti-set! (app recur addr) (app recur e))
       (Mbox-rtti-set! addr e)]
      [(Mbox-rtti-ref (app recur addr))
       (Mbox-rtti-ref addr)]
      [(Make-GLB-Two-Fn-Types e1 (app recur e2) (app recur e3))
       (Make-GLB-Two-Fn-Types e1 e2 e3)]
      [(Make-GLB-Two-Tuple-Types e1 (app recur e2) (app recur e3))
       (Make-GLB-Two-Tuple-Types e1 e2 e3)]
      [(MRef-Coercion-Huh (app recur e)) (MRef-Coercion-Huh e)]
      [(MRef-Coercion-Type (app recur e)) (MRef-Coercion-Type e)]
      [(MRef-Coercion (app recur e)) (MRef-Coercion e)]
      [(Type-GRef (app recur e)) (Type-GRef e)]
      [(Type-GVect (app recur e)) (Type-GVect e)]
      [(Type-MRef (app recur e)) (Type-MRef e)]
      [(Type-MRef-Huh (app recur e)) (Type-MRef-Huh e)]
      [(Type-MRef-Of (app recur e)) (Type-MRef-Of e)]
      [(Mvector-val-set! (app recur e1) (app recur e2) (app recur e3) mode)
       (Mvector-val-set! e1 e2 e3 mode)]
      [(Mvector-val-ref (app recur e1) (app recur e2) mode)
       (Mvector-val-ref e1 e2 mode)]
      [(Mvector-rtti-set! (app recur addr) (app recur e))
       (Mvector-rtti-set! addr e)]
      [(Mvector-rtti-ref (app recur addr))
       (Mvector-rtti-ref addr)]
      [(Type-MVect e) (Type-MVect (recur e))]
      [(Type-MVect-Huh e) (Type-MVect-Huh (recur e))]
      [(Type-MVect-Of e) (Type-MVect-Of (recur e))]
      [(MVect-Coercion-Huh e) (MVect-Coercion-Huh (recur e))]
      [(MVect-Coercion-Type e) (MVect-Coercion-Type (recur e))]
      [(MVect-Coercion e) (MVect-Coercion (recur e))]
      [(Mvector-length e) (Mvector-length (recur e))]
      [(Error (app recur e)) (Error e)]
      [(Create-tuple (app recur* e*))
       (Create-tuple e*)]
      [(Tuple-proj e i) (Tuple-proj (recur e) (recur i))]
      [(Tuple-Coercion-Huh e) (Tuple-Coercion-Huh (recur e))]
      [(Tuple-Coercion-Num e) (Tuple-Coercion-Num (recur e))]
      [(Tuple-Coercion-Item e i) (Tuple-Coercion-Item (recur e) (recur i))]
      [(Coerce-Tuple uid e1 e2) (Coerce-Tuple uid (recur e1) (recur e2))]
      [(Coerce-Tuple-In-Place uid e1 e2 e3 e4 e5)
       (Coerce-Tuple-In-Place uid (recur e1) (recur e2) (recur e3) (recur e4) (recur e5))]
      [(Cast-Tuple uid e1 e2 e3 e4) (Cast-Tuple uid (recur e1) (recur e2) (recur e3) (recur e4))]
      [(Cast-Tuple-In-Place uid e1 e2 e3 e4 e5 e6 e7)
       (Cast-Tuple-In-Place uid (recur e1) (recur e2) (recur e3) (recur e4)
                            (recur e5) (recur e6) (recur e7))]
      [(Type-Tuple-Huh e) (Type-Tuple-Huh (recur e))]
      [(Type-Tuple-num e) (Type-Tuple-num (recur e))]
      [(Type-Tuple-item e i) (Type-Tuple-item (recur e) (recur i))]
      [(Make-Tuple-Coercion uid t1 t2 lbl)
       (Make-Tuple-Coercion uid (recur t1) (recur t2) (recur lbl))]
      [(Mediating-Coercion-Huh e) (Mediating-Coercion-Huh (recur e))]
      [(? string? x) (error 'hoist-types/string)]))
  ;; Recur through other type containing ast forms
  (: recur* (CoC3-Expr* -> CoC3.1-Expr*))
  (define (recur* e*) (map recur e*))
  
  (: recur-bnd* (CoC3-Bnd* -> CoC3.1-Bnd*))
  (define (recur-bnd* b*)
    (map (lambda ([b : CoC3-Bnd])
           (cons (car b) (recur (cdr b))))
         b*))

  (: recur-bnd-code* (CoC3-Bnd-Code* -> CoC3.1-Bnd-Code*))
  (define (recur-bnd-code* b*)
    (map (lambda ([b : CoC3-Bnd-Code])
           (match-let ([(cons a (Code u* e)) b])
             (cons a (Code u* (recur e)))))
         b*))
  
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
