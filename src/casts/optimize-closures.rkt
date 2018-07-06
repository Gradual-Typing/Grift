#lang typed/racket/base
#|------------------------------------------------------------------------------+
|Pass: src/casts/convert-closures                                               |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass performs closure conversion and optimization based off
of the paper Optimizing closures in O(n) time.

- Closure Conversion is performed by the function `uncover-closure-ops`:
 - Records the free varibales of each closure in the `Closure` form.
 - Reduces the number of free variables by eliminating free variables that are
   known to be :
   - bound to constants (constant propagation)
   - aliases of other free variables (copy propagation)
   - references to the current closure (self reference)
 - Generates names for the closure self-parameter and closure code
 - Calls function code directly when a closure is known (direct call optimization)
- Closure optimization is performed by the function `optimize-closures`:
  - Identifies closures with the same lifetime 
    - uses tarjan's algorithm to identify strongly connected components formed by
      variable capture.
  - When one or more well-known closures exist with the same life 
    we share a single closure for the well-known closures.
  - When a closure is not well known but has no free variables
    then we statically allocate the closure and eliminate free
    variables that reference it.
  - Take into account all free-variables eliminated by sharing,
    statically allocating closures, and eliminating closure
    allocations in the next step.
  - Select representations for closures that minimize space, 
    maximize sharing, without extending the lifetime of any
    closure.
    - *Well-Known w/ No Free Variable* No Closure
    - *Well-Known w/ 1 Free Variable* Lambda Lift the one variable
    - *Well-Known w/ more than 1 Free Variables* allocate free variable in a tuple
    - *Not Well-Known w/ No Free Variable* Static Allocated Closure
    - *Otherwise* allocate closure normally

Terminology:
- a closure is *well-known* if we know where all of its call sites are.
  - we conservatively estimate this property by checking to see if the
    closure is found in any argument possition.
- a closure is *known* if we know that a variable binds it at a particular
  location. 

+-------------------------------------------------------------------------------+
TODO Integrate Closure Optimization
TODO Start optimize closures
TODO introduce static procedure form

Notes to be deleted after implementation
;;   (define select-share (optimize-shared-closures prgm))
;;   ;;4. Determine the required free variables for each closure, leaving
;;   ;;out those that are unnecessary.

;;   ;; Constant propogate statically allocated closure and 
;;   ;; Constant propogate small constants
;;   ;; Take into account free variables eliminated by closure conversion
;;   ;; Copy propogation (Alias Propagation)
;;   ;; 
  
;;   ;;5. Select the appropriate representation for each closure and
;;   ;;whether it can share space with a closure from some outer strongly
;;   ;;connected set of bindings.
  
  
;;   ;;6. Rebuild the code based on these selections.

+------------------------------------------------------------------------------|#

(require 
 racket/match
 racket/hash
 "../configuration.rkt"
 "../language/cast-or-coerce3.1.rkt"
 "../language/cast-or-coerce4.rkt"
 "../language/syntax.rkt"
 (submod "../logging.rkt" typed)
 #;"../language/cast-or-coerce5.rkt"
 #;"../language/cast-or-coerce6.rkt"
 "../type-equality.rkt")

(provide
 convert-closures
 #;
 (all-from-out
  "../language/cast-or-coerce6.rkt"))

(: convert-closures (Cast-or-Coerce4-Lang -> Any))
(define (convert-closures prgm)
  (match prgm
    [(Prog (list name next type)
       (Let-Static* bnd-mu-type*
                    bnd-type*
                    bnd-mu-crcn*
                    bnd-crcn*
                    main-expr))
     
     (define uc (make-unique-counter next))

     (define main-expr^
       (parameterize ([current-unique-counter uc])
         (define cast-rep (cast-representation))
         (define fn-proxy-rep (fn-proxy-representation))
         (uncover-closure-ops main-expr)))

     (Prog (list name (unique-counter-next! uc) type)
       (Let-Static* bnd-mu-type*
                    bnd-type*
                    bnd-mu-crcn*
                    bnd-crcn*
                    main-expr^))]))

(define (unbound-variable-error u)
  (error 'convert-closures.rkt "unbound variable ~a" u))

(require typed/racket/unsafe)

(unsafe-require/typed
 "../language/form-map.rkt"
 ;; One common cause of type-checking time explosion is having type
 ;; errors involving this type.
 [form-map
  (case->
   [(U= CoC4-Expr) (CoC4-Expr -> CoC6-Expr) -> (U= CoC6-Expr)]
   [CoC6-Expr (CoC6-Expr -> CoC6-Expr) -> CoC6-Expr])])

;; TODO This should go in configuration.rkt
(define-type Fn-Proxy-Representation (U 'Hybrid 'Data))
(: fn-proxy-representation (Parameterof Fn-Proxy-Representation))
(define fn-proxy-representation
  (make-parameter 'Hybrid))

(struct (E) Let-Closures form
  ([bindings : (Listof (Closure E))]
   [body : E])
  #:transparent)

(struct (E) Closure form:simple-branch
  ([name : Uid]
   [well-known? : Boolean]
   [code-label : Uid]
   [self-parameter : Uid]
   [caster : (Option Uid)]
   [free-vars : Uid*]
   [parameters : Uid*]
   [code : E])
  #:transparent)

(struct (E) Closure-Code form:simple-branch
  ([arg : E])
  #:transparent)

(struct (E) Closure-Caster form:simple-branch
  ([arg : E])
  #:transparent)

(struct (E) Closure-Ref form:leaf
  ([arg : Uid] [key : Uid])
  #:transparent)

(struct (E) Closure-App form:simple-branch
  ([code : E]
   [closure : E]
   [arguments : (Listof E)])
  #:transparent)

(define-type (Closure-Ops E)
  (U (LetP (Bnd* (Procedure Uid Uid* Uid (Option Uid) Uid* E)) E)
     (Let-Closures E)
     (Closure-Code E)
     (Closure-Caster E)
     Closure-Ref
     (Closure-App E)))

(define-type (Hybrid-Fn-Proxy-Forms E)
  (U (Hybrid-Proxy Uid E E)
     (Hybrid-Proxy-Huh E)
     (Hybrid-Proxy-Closure E)
     (Hybrid-Proxy-Coercion E)))

(define-type (Data-Fn-Proxy-Forms E)
  (U (Fn-Proxy Index E E)
     (Fn-Proxy-Huh E)
     (Fn-Proxy-Closure E)
     (Fn-Proxy-Coercion E)))

(define-type CoC6-Expr
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

;; Debugging assertions (Are our types in the correct ballpark)
(assert-subtype? (U- CoC4-Expr) CoC4-Expr)
(assert-subtype? (U= CoC4-Expr) CoC4-Expr)
(assert-subtype? (U+ CoC6-Expr) CoC6-Expr)
(assert-subtype? (U= CoC6-Expr) CoC6-Expr)

;; INVARIANT CoC4 = U= + U-
;;We check this by checking subtyping in both directions because it is
;;easier to know what is broken when this fails.
(assert-subtype? (U (U= CoC4-Expr) (U- CoC4-Expr)) CoC4-Expr)
(assert-subtype? CoC4-Expr (U (U= CoC4-Expr) (U- CoC4-Expr)))

;; INVARIANT CoC6 = U= + U+
;;We check this by checking subtyping in both directions because it is
;;easier to know what is broken when this fails.
(assert-subtype? (U (U= CoC6-Expr) (U+ CoC6-Expr)) CoC6-Expr)
(assert-subtype? CoC6-Expr (U (U= CoC6-Expr) (U+ CoC6-Expr)))

(struct Bound-Var
  ([uid : Uid]
   [var : (Var Uid)]
   [depth : Nat])
  #:transparent)

(define-type Propagate
  (U Bound-Var (Quote Cast-Literal)))

(: uncover-closure-ops
   (->* (CoC4-Expr)
        (#:cast-rep Cast-Representation
         #:fn-proxy-rep Fn-Proxy-Representation)
        CoC6-Expr))
(define (uncover-closure-ops
         e
         #:cast-rep [cast-rep (cast-representation)]
         #:fn-proxy-rep [fn-proxy-rep (fn-proxy-representation)])
  (define-type Proc6 (Procedure Uid Uid* Uid (Option Uid) Uid* CoC6-Expr))
  (define apply-proc-bindings : (HashTable Integer (cons Uid Proc6))
    (make-hash '()))
  (: get-apply-uid! (Integer Uid -> Uid))
  (define (get-apply-uid! i cast-uid)
    (define apply-bnd? (hash-ref apply-proc-bindings i #f))
    (cond
      [apply-bnd? => (inst car Uid Proc6)]
      [else
       ;; The following code is kinda tricky but the Procedure
       ;; form gets lowered to only generating the code but not
       ;; instantiation of a closure.
       ;; TODO change this form's name to be less generic.
       ;; TODO change this form's fields to match Closure
       (define apply-name (string-append "apply_hybrid_" (number->string i)))
       (define apply-uid (next-uid! apply-name))
       (define self (next-uid! "hybrid_closure_self"))
       (define a* (build-list i (lambda (a) (next-uid! "arg"))))
       (define v* (map (inst Var Uid) a*))
       (define clos-field (next-uid! "closure_field"))
       (define crcn-field (next-uid! "corcion_field"))
       (define bnd
         (cons
          apply-uid
          (Procedure
           self a* apply-uid #f
           ;; The order of these two elements determines closure layout
           ;; which needs to be know for the hybrid representation
           ;; we could make the caster call closure specific code
           ;; that is also generated by the procedure form.
           (list clos-field crcn-field)
           (let$ ([hybrid-proxy-closure (Closure-Ref self clos-field)]
                  [hybrid-proxy-coercion (Closure-Ref self crcn-field)])
             (cast-apply-cast cast-uid
                              hybrid-proxy-closure
                              v*
                              hybrid-proxy-coercion)))))
       (hash-set! apply-proc-bindings i bnd)
       apply-uid]))

  (: cmap (All (A B) (A -> B) -> ((Listof A) -> (Listof B))))
  (define ((cmap f) xs) (map f xs))

  (define rec/env-return : CoC6-Expr
    (let rec/env ([current-e : CoC4-Expr e]
                  ;; Immutable map from function (closure) names to code labels
                  [code-env : (HashTable Uid Uid) (hash)]
                  ;; Map from variable names to representative constant or variable
                  [opt-env : (HashTable Uid Propagate) (hash)]
                  ;; The current number of bindings between `e` and top-level
                  [current-binding-depth : Nat 0]
                  ;; The name that the closure is bound in the body of the letrec
                  [current-closure-name : (Option Uid) #f]
                  ;; The self parameter of the closure being processed
                  [current-closure-self : (Option Uid) #f]
                  ;; The depth at which the current closure is bound
                  [current-closure-depth : Nat 0]
                  ;; Mutable set of free variables in the current closure
                  ;; this is an out parameter.
                  [free-variables : (MSet Uid) (mutable-set)])
      (let rec ([current-e current-e])
        (cond
          [(Var? current-e)
           (match-define (Var u) current-e)
           ;; Folding Constants and Aliases can reduce closure size
           ;; particularly when we rely on the c compiler to perform
           ;; these operations for use
           (define propagate (hash-ref opt-env u))
           (cond
             [(Bound-Var? propagate)
              (match-define (Bound-Var u v binding-depth) propagate)
              (cond
                ;; - If the variable is bound at a lesser depth
                ;;   than the closure closes over then it is free.
                ;; - If the variable is bound at the same depth
                ;;   then it is another closure and we will try
                ;;   to optimize it in optimize-closures pass,
                ;;   but for he time being the variable is free.
                [(<= binding-depth current-closure-depth)
                 (unless current-closure-self
                   (error 'convert-closures.rkt
                          "free variable without current closure: ~a" u))
                 (cond
                   ;; Optimize Self Reference
                   ;; If a free variable is refering to the outside/recusive
                   ;; name of the current-closure then replace it with the
                   ;; a reference to the closure self parameter.
                   [(eq? u current-closure-name) (Var current-closure-self)]
                   [else
                    ;; Otherwise the variable is indeed free and we need
                    ;; to put it in the closure, and refer to it from the
                    ;; the closure.
                    (set-add! free-variables u)
                    (Closure-Ref current-closure-self u)])]
                ;; The bound var is not free so just use the variable
                [else v])] 
             ;; The variable was bound to a constant that can be propogated
             [else propagate])]
          [(Letrec? current-e)
           (match-define (Letrec b* e) current-e)
           (define letrec-binding-depth (add1 current-binding-depth))
           (: code-label* Uid*)
           (: self-parameter* Uid*)
           (define-values (code-label* self-parameter*)
             (for/lists ([code-label* : Uid*]
                         [self-parameter* : Uid*])
                        ([b b*])
               (match-define (cons clos-name _) b)
               (values (format-uid! "~a_code" clos-name)
                       (format-uid! "~a_closure_self" clos-name))))
           ;; Extend the code-env bindings for Optimize Direct Call
           ;; Extend opt-env without trying to eliminate bindings.
           (define-values (code-env^ opt-env^) 
             (for/fold ([code-env^ : (HashTable Uid Uid) code-env]
                        [opt-env^ : (HashTable Uid Propagate) opt-env])
                       ([code-label code-label*]
                        [b b*])
               (match-define (cons x _) b)
               (define bv (Bound-Var x (Var x) letrec-binding-depth))
               (values (hash-set code-env^ x code-label)
                       (hash-set opt-env^  x bv))))
           (define c* : (Listof (Closure CoC6-Expr))
             (for/list ([b b*]
                        [code-label code-label*]
                        [self-parameter self-parameter*])
               (match-define (cons closure-name (Lambda p* (Castable c? e))) b)
               (define lambda-binding-depth (add1 letrec-binding-depth))
               (define fvs : (MSet Uid) (mutable-set))
               (define code-env^^
                 (hash-set code-env^ self-parameter code-label))
               (define opt-env^^
                 (for/fold ([env : (HashTable Uid Propagate) opt-env^])
                           ([p p*])
                   (hash-set env p (Bound-Var p (Var p) lambda-binding-depth))))
               (define e^
                 (rec/env e code-env^^ opt-env^^
                          lambda-binding-depth
                          closure-name
                          self-parameter
                          letrec-binding-depth
                          fvs))
               ;; We have to check each free variable at this depth
               ;; to see if they need added to the current free variable set.
               ;; TODO find a way not to perfom this iteration/lookup again
               (define fv* (set->list fvs))
               (for ([u fv*])
                 (match (hash-ref opt-env^ u)
                   [(Bound-Var _ _ bd)
                    #:when (<= bd current-closure-depth)
                    (set-add! free-variables u)]
                   [other (void)]))
               (Closure closure-name #f code-label self-parameter c? fv* p* e^)))

           (Let-Closures c* (rec/env e code-env^ opt-env^
                                     letrec-binding-depth
                                     current-closure-name
                                     current-closure-self
                                     current-closure-depth
                                     free-variables))]
          [(Let? current-e)
           (match-define (Let b* e) current-e)
           (define let-binding-depth (add1 current-binding-depth))
           (define-values (b*^ opt-env^)
             (for/fold ([b*^ : (Bnd* CoC6-Expr) '()]
                        [env : (HashTable Uid Propagate) opt-env])
                       ([b b*])
               (match-define (cons u e) b)
               ;; Invoking `rec` here would cause free variables that
               ;; are aliased but not used to be recorded as being free.
               (define possibly-free-vars : (MSet Uid) (mutable-set))
               ;; Notes on recursion:
               ;; - recuring on the rhs `e` of binding
               
               ;; - current-depth isn't incremented because the binding hasn't occurred
               ;; - current-closure-* remains the same
               ;; - possibly-free-vars will be added to free variables if
               ;;   the binding isn't eliminated.
               (match (rec/env e code-env opt-env
                               current-binding-depth
                               current-closure-name
                               current-closure-self
                               current-closure-depth
                               possibly-free-vars)
                 ;; Eliminate aliases
                 [(Var u^)
                  (values b*^ (hash-set env u (hash-ref opt-env u^)))]
                 ;; Propogate Constants
                 [(and (Quote _) lit) 
                  (values b*^ (hash-set env u lit))]
                 [e
                  ;; Can't eliminate this binding
                  (define bv (Bound-Var u (Var u) let-binding-depth))
                  ;; Need to merge the free variables of this expression
                  (set-union! free-variables possibly-free-vars)
                  (values (cons (cons u e) b*^)
                          (hash-set env u bv))])))
           (define e^
             (rec/env e code-env
                      ;; May have added propagatable bindings
                      opt-env^
                      ;; Going under a binding
                      let-binding-depth
                      current-closure-name
                      current-closure-self
                      current-closure-depth
                      free-variables))
           (cond
             [(null? b*^) e^]
             [else (Let b*^ e^)])]
          ;; Castable Function Operations
          [(Fn-Caster? current-e)
           (match-define (Fn-Caster e) current-e)
           (Closure-Caster (rec e))]
          [(App-Fn? current-e)
           (: let-clos-app : CoC6-Expr (Listof CoC6-Expr) -> CoC6-Expr) 
           (define (let-clos-app e e*)
             (let$ ([closure e]) (clos-app closure e*)))
           (: clos-app : (Var Uid) (Listof CoC6-Expr) -> CoC6-Expr) 
           (define (clos-app v e*) (Closure-App (Closure-Code v) v e*))
           (match-define (App-Fn e (app (cmap rec) e*)) current-e)
           (match (rec e)
             [(and v (Var u))
              (cond
                [(hash-ref code-env u #f) =>
                 (lambda ([u : Uid])
                   ;; Invariant: The latter closure optimization passes
                   ;; identify known function application by the presence
                   ;; of a code-label literal in code position.
                   (Closure-App (Code-Label u) v e*))]
                [else (clos-app v e*)])]
             [(and cr (Closure-Ref _ u))
              (cond
                [(hash-ref code-env u #f) =>
                 (lambda ([u : Uid])
                   ;; Invariant: The latter closure optimization passes
                   ;; identify known function application by the presence
                   ;; of a code-label literal in code position.
                   (Closure-App (Code-Label u) cr e*))]
                [else (let-clos-app cr e*)])]
             [e (let-clos-app e e*)])]
          ;; Fn-Proxy Operations
          ;; App-Fn-or-Proxy is very similar to Fn-App
          [(App-Fn-or-Proxy? current-e)
           (: cast-clos-app : Uid CoC6-Expr (Listof CoC6-Expr) -> CoC6-Expr)
           (define (cast-clos-app cast v e*)
             (If (Fn-Proxy-Huh v)
                 (let$ ([data-fn-proxy-closure (Fn-Proxy-Closure v)]
                        [data-fn-proxy-coercion (Fn-Proxy-Coercion v)])
                   (cast-apply-cast cast data-fn-proxy-closure e*
                                    data-fn-proxy-coercion)) 
                 (Closure-App (Closure-Code v) v e*)))
           (: let-cast-clos-app :
              Uid CoC6-Expr (Listof CoC6-Expr) -> CoC6-Expr)
           (define (let-cast-clos-app cast e e*)
             (let$ ([maybe-data-fn-proxy e])
               (cast-clos-app cast e e*)))
           (match-define (App-Fn-or-Proxy cast-uid e e*) current-e)
           (case cast-rep
             [(Coercions Hyper-Coercions)
              (case fn-proxy-rep
                ;; If it is a hybrid representation then proxies and closures
                ;; are applied the same way.
                [(Hybrid) (rec (App-Fn e e*))]
                [(Data)
                 (define e*^ (map rec e*))
                 (match (rec e)
                   [(and v (Var u))
                    (cond
                      ;; If it a data representation and it is a
                      ;; known definition site then it can't be a
                      ;; proxy because we can see the lambda
                      [(hash-ref code-env u #f) =>
                       (lambda ([u : Uid])
                         ;; Invariant: The latter closure optimization passes
                         ;; identify known function application by the presence
                         ;; of a code-label literal in code position.
                         (Closure-App (Code-Label u) v e*^))]
                      [else (cast-clos-app cast-uid v e*^)])]
                   [(and cr (Closure-Ref _ u))
                    (cond
                      ;; If it a data representation and it is a
                      ;; known definition site then it can't be a
                      ;; proxy because we can see the lambda
                      [(hash-ref code-env u #f) =>
                       (lambda ([u : Uid])
                         ;; Invariant: The latter closure optimization passes
                         ;; identify known function application by the presence
                         ;; of a code-label literal in code position.
                         (Closure-App (Code-Label u) cr e*^))]
                      [else (let-cast-clos-app cast-uid cr e*^)])]
                   [e^ (let-cast-clos-app cast-uid e^ e*^)])]
                [else (error 'covert-closures "Unkown Fn-Proxy Representaion")])]
             [else (error 'covert-closures "Unkown Cast Representaion")])]
          [(Fn-Proxy? current-e)
           (match-define
             (Fn-Proxy `(,i ,cast) (app rec e1)(app rec e2))
             current-e)
           (case cast-rep
             [(Coercions Hyper-Coercions)
              (case fn-proxy-rep              
                [(Hybrid) (Hybrid-Proxy (get-apply-uid! i cast) e1 e2)]
                [(Data)   (Fn-Proxy i e1 e2)]
                [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
             [else (error 'convert-closures "unkown cast representation")])]
          [(Fn-Proxy-Huh? current-e)
           (match-define (Fn-Proxy-Huh (app rec e)) current-e) 
           (case cast-rep
             [(Coercions Hyper-Coercions)
              (case fn-proxy-rep
                [(Hybrid) (Hybrid-Proxy-Huh e)]
                [(Data)   (Fn-Proxy-Huh e)]
                [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
             [else (error 'convert-closures "unkown cast representation")])]
          [(Fn-Proxy-Closure? current-e)
           (match-define (Fn-Proxy-Closure (app rec e)) current-e)
           (case cast-rep
             [(Coercions Hyper-Coercions)
              (case fn-proxy-rep
                [(Hybrid) (Hybrid-Proxy-Closure e)]
                [(Data)   (Fn-Proxy-Closure e)]
                [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
             [else (error 'convert-closures "unkown cast representation")])]
          [(Fn-Proxy-Coercion? current-e)
           (match-define (Fn-Proxy-Coercion (app rec e)) current-e) 
           (case cast-rep
             [(Coercions Hyper-Coercions)
              (case fn-proxy-rep
                [(Hybrid) (Hybrid-Proxy-Coercion e)]
                [(Data)   (Fn-Proxy-Coercion e)]
                [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
             [else (error 'convert-closures "unkown cast representation")])] 
          [else (form-map current-e rec)]))))
  (define apply-proc-bnd* (hash-values apply-proc-bindings))
  (cond
    [(null? apply-proc-bnd*) rec/env-return]
    [else (LetP apply-proc-bnd* rec/env-return)]))

(module+ test
  (require
   (for-syntax
    racket/base
    racket/syntax)
   typed/rackunit)

  (define (refresh!)
    (current-unique-counter (make-unique-counter 0)))

  (define-syntax (define-u/v stx)
    (syntax-case stx ()
      [(_ u (c v) s n)
       #'(begin
           (define u (Uid s n))
           (define v (c u)))]))

  (define-syntax (define-u/v* stx)
    (syntax-case stx ()
      [(_ u (c v) s n)
       (with-syntax ([((u* v* n*) ...)
                      (for/list ([i (in-range (syntax->datum #'n))])
                        (list (format-id #'u "~a~a" #'u i)
                              (format-id #'v "~a~a" #'v i)
                              i))])
         #'(begin
             (define-u/v u* (c v*) s n*)
             ...))]))

  (define-u/v* u (Var v) "" 10)
  (define-u/v* p (Var pv) "arg" 10)
  (define-u/v* code (Code-Label label) "_code" 10)
  (define-u/v* self (Var selfv) "_closure_self" 10)

  
  ;; TODO Check these tests on optimize closures
  ;; TODO Write tests for most of the features of optimize closures
  
  ;; This need to be something that won't propagate for the tests to
  ;; be correct
  (define non-propagant (op$ + (Quote 1) (Quote 1)))
  ;; constant propagation
  (define input1 (Let (list (cons u0 (Quote 1))) v0))
  (refresh!)
  (test-equal?
   "constant propagation"
   (uncover-closure-ops input1) (Quote 1))
  
  ;; copy propagation
  (define input2 (Let (list (cons u0 non-propagant))
                   (Let (list (cons u1 v0))
                     v1)))
  (define expect2 (Let (list (cons u0 non-propagant)) v0))
  (refresh!)
  (test-equal?
   "copy propogation"
   (uncover-closure-ops input2) expect2)

  ;; correct free variable discovery for single closure
  (define input3
    (Let (list (cons u0 non-propagant))
      (Letrec (list (cons u1 (Lambda '() (Castable #f v0))))
        v1)))
  (define expect3
    (Let (list (cons u0 non-propagant))
      (Let-Closures
       (list (Closure
              u1 #f code0 self1 #f (list u0) '()
              (Closure-Ref self1 u0)))
       v1)))
  (refresh!)
  (test-equal?
   "free variables"
   (uncover-closure-ops input3) expect3)

  ;; correct free variable discovery for nested closure 1
  ;; TODO
  (define input4 (void))

  ;; TODO
  ;; correct free variable discovery for nested closure 2
  (define input5 (void))

  ;; Mutually Recusive Closures
  (define input6
    (Letrec (list (cons u0 (Lambda '() (Castable #f (App-Fn v1 '()))))
                  (cons u1 (Lambda '() (Castable #f (App-Fn v0 '())))))
      v1))
  (define expect6
    (Let-Closures
     (list (Closure u0 #f code0 self1 #f (list u1) '()
                    (Closure-App (Code-Label code2)
                                 (Closure-Ref self1 u1)
                                 '()))
           (Closure u1 #f code2 self3 #f (list u0) '()
                    (Closure-App (Code-Label code0)
                                 (Closure-Ref self3 u0)
                                 '())))
     v1))
  (refresh!)
  (test-equal?
   "mutually recursive free-variables"
   (uncover-closure-ops input6) expect6)

  ;; Chaining of optimization leads to less free variables
  (define input7
    (Let (list (cons u0 (Quote 1)))
      (Letrec (list (cons u1 (Lambda (list u2) (Castable #f v0))))
        (Let (list (cons u3 v1))
          (Letrec
              (list
               (cons u4 (Lambda '() (Castable #f (App-Fn v1 (list v4))))))
            v4)))))
  (define expect7
    (Let-Closures
     (list (Closure u1 #f code0 self1 #f '() `(,u2) (Quote 1)))
     (Let-Closures
      (list (Closure u4 #f code2 self3 #f `(,u1) '()
                     (Closure-App label0
                                  (Closure-Ref self3 u1)
                                  (list selfv3))))
      v4)))
  (refresh!)
  (test-equal?
   "free-var optimization chaining"
   (uncover-closure-ops input7) expect7)

  ;; Optimization works with proxied function applications where
  ;; possible
  (define input8
    (Letrec
        (list
         (cons
          u0
          (Lambda (list p0)
            (Castable
             u1
             (If (op$ = v0 pv0)
                 (App-Fn-or-Proxy u2 v0 (list pv0))
                 (App-Fn-or-Proxy u2 pv0 (list v0)))))))
      (App-Fn v0 (list v0))))
  (define expect8
    (Let-Closures
     (list
      (Closure
       u0 #f code0 self1 u1 '() (list p0)
       (If (op$ = selfv1 pv0)
           (Closure-App label0 selfv1 (list pv0))
           (Closure-App (Closure-Code pv0) pv0 (list selfv1)))))
     (Closure-App label0 v0 (list v0))))
  (refresh!)
  (test-equal?
   "proxied function app"
   (uncover-closure-ops input8) expect8))

;; Analyzes closures to determine if they are well-know
;; and seperates closures into sets of mutually recursive
;; definitions.
(: analyze-closures : CoC6-Expr -> CoC6-Expr)
(define (analyze-closures e)
  (define well-known-closures (mutable-set))
  (let rec/cc ([e e]
               [current-closure : (Option Uid) #f]
               [current-closure-self : (Option Uid) #f])
    (let rec ([e e])
      (match e
        [(Let-Closures c* b)
         (define this-binding (mutable-set))
         ;; Initially assume a closure is well-known. If it ever
         ;; reaches the `(Var u)` case remove it from this set, because
         ;; it must be used as an argument somewhere.
         ;; The `(Closure-App (Code-Label _) self e*)` case prevents
         ;; Variables that are used as the self parameter from matching
         ;; this case.
         (for ([c c*])
           (define n (Closure-name c))
           (set-add! this-binding n)
           (set-add! well-known-closures n))

         ;; Recursively process the body before deciding well-knowness
         ;; of closures in this binding.
         (define b^ (rec b))

         ;; Recur into each closure but keep all the meta data
         ;; associated with the result.  All closures must be
         ;; recursively processes before we can decide the well-knowness
         ;; of anything in this binding
         (define bxc* : (Listof (Pairof CoC6-Expr (Closure CoC6-Expr)))
           (for/list ([c c*])
             (match-define (Closure name _ _ self _ _ _ code) c)
             (cons (rec/cc code name self) c)))

         ;; A graph representation of the closures to their free
         ;; variables of that are bound in this binding.
         ;; This graph is populated in the next step. 
         (define g : (Graph Uid) (make-graph '()))
         
         (define u->c-hash
           (for/hasheq : (HashTable Uid (Closure CoC6-Expr)) ([bxc bxc*])
             (match-define (cons b (Closure name _ label self c? f* p* _)) bxc)
             ;; Only after we have done every recursive call can we know
             ;; if the closure is well-kown.
             (define wk? (set-member? well-known-closures name))
             (define c (Closure name wk? label self c? f* p* b))
             (add-vertex! g name)
             (for ([f f*])
               ;; we only care about the free variables that are bound
               ;; in this binding. Filtering these here prevents `scc`
               ;; from having to deal with them, and awkward code later
               ;; when we reconstruct the bindings.
               (when (set-member? this-binding f)
                 (add-edge! g name f)))
             (values name c)))
         
         (: u->c : Uid -> (Closure CoC6-Expr))
         (define (u->c x) (hash-ref u->c-hash x))

         ;; Compute the strongly connected Components
         ;; i.e. The sets of mutually recusive bindings.
         ;; Tarjans algorithm also topologically sorts the bindings
         (define scc* : (Listof Uid*)
           ;; This loop reverses the list and sorts each scc
           ;; NOTE `scc` iterates on hash-tables which isn't
           ;; deterministic. Sorting here restores determinism.
           (for/fold ([scc* : (Listof Uid*) '()])
                     ([scc (in-list (scc g))])
             (cons (sort scc uid<?) scc*)))

         ;; Build nested letrecs were only mutually recursive bindings
         ;; appear in the same letrec.
         (let loop ([scc* scc*])
           (match scc*
             ['() b^]
             [(cons scc scc*)
              (Let-Closures (map u->c scc) (loop scc*))]))]
        ;; Keep the variable in the self possition of known closure
        ;; applications from counting towards being not well-known.
        [(Closure-App (and cl (Code-Label _)) self e*)
         (Closure-App cl self (map rec e*))]
        ;; All variable or free-variable references outside of
        ;; known-closure application indicate that the variable may
        ;; have escaped. The current self variable is a known binding
        ;; to the current closure and when it escapes the
        ;; current-closure escapes. This can occur due to the
        ;; optimization of self references in closure conversion.
        [(or (Var u) (Closure-Ref _ u))
         (set-remove! well-known-closures u)
         (when (eq? u current-closure-self) 
           (unless current-closure
             (error
              'analyze-closure
              (string-append "current-closure must be a Uid "
                             "when current-closure-self is a Uid")))
           (set-remove! well-known-closures current-closure))
         e]
        [otherwise (form-map e rec)])))) 

(module+ test
  (define expect-ac-3
    (Let (list (cons u0 non-propagant))
      (Let-Closures
       (list (Closure
              u1 #f code0 self1 #f (list u0) '()
              (Closure-Ref self1 u0)))
       v1)))
  (refresh!)
  (test-equal?
   "single escaping closure"
   (analyze-closures expect3)
   expect-ac-3)

  (define expect-ac-6
    (Let-Closures
     (list (Closure u0 #t code0 self1 #f (list u1) '()
                    (Closure-App (Code-Label code2)
                                 (Closure-Ref self1 u1)
                                 '()))
           (Closure u1 #f code2 self3 #f (list u0) '()
                    (Closure-App (Code-Label code0)
                                 (Closure-Ref self3 u0)
                                 '())))
     v1))
    (refresh!)
  (test-equal?
   "1 wk / 1 !wk"
   (analyze-closures expect6)
   expect-ac-6)

  (define input9
    (Letrec
        (list (cons u0 (Lambda '() (Castable #f (App-Fn v1 '()))))
              (cons u1 (Lambda '() (Castable #f (App-Fn v0 '()))))
              (cons u2 (Lambda '() (Castable #f (App-Fn v1 '()))))
              (cons u3 (Lambda '() (Castable #f (App-Fn v2 '())))))
      v3))
  (define expect-ac-9
    (Let-Closures
     (list (Closure u0 #t code0 self1 #f (list u1) '()
                    (Closure-App (Code-Label code2)
                                 (Closure-Ref self1 u1)
                                 '()))
           (Closure u1 #t code2 self3 #f (list u0) '()
                    (Closure-App (Code-Label code0)
                                 (Closure-Ref self3 u0)
                                 '())))
     (Let-Closures
      (list (Closure u2 #t code4 self5 #f (list u1) '()
                     (Closure-App (Code-Label code2)
                                  (Closure-Ref self5 u1)
                                  '())))
      (Let-Closures
       (list (Closure u3 #f code6 self7 #f (list u2) '()
                      (Closure-App (Code-Label code4)
                                   (Closure-Ref self7 u2)
                                   '())))
       v3))))
  (refresh!)
  (test-equal?
   "3 wk / 1 !wk / 2 mr"
   (analyze-closures (uncover-closure-ops input9))
   expect-ac-9))

#;(: optimize-closures : CoC6-Expr -> CoC6-Expr)
#;
(define (optimize-closures e)
  (let rec ([current-e e])
    (cond
      [(Let-Closures? current-e)
       (match-define (Let-Closures c* e) current-e)
       ;; simultaneously add closures to the well-know
       ;; closure
       (define fv-als )
       (for/list ([c c*]))
       (for/hasheq ([c c*])
         (match-define (Closure name code self c? f* p* b) c)
         (for ([fv f*]) (graph-add-edge! g name fv))
         
         
         )]
      )))

;; Helpers that belong here

;; create a casted fn application call site
(: cast-apply-cast (Uid (Var Uid) (Listof CoC6-Expr) (Var Uid) -> CoC6-Expr))
(define (cast-apply-cast cast-uid fun arg* crcn)
  (define cast-label (Code-Label cast-uid))
  (define-values (v* b*)
    (for/lists ([v* : (Listof (Var Uid))]
                [b* : (Bnd* CoC6-Expr)])
               ([arg : CoC6-Expr arg*] [i (in-range 0)])
      (define u (next-uid! (format "fn-cast-arg~a" i)))
      (define arg-crcn (Fn-Coercion-Arg crcn (Quote i)))
      (values (Var u)
              (cons u (App-Code cast-label (list arg arg-crcn ZERO-EXPR))))))
  (define clos-app (Closure-App (Closure-Code fun) fun v*))
  (App-Code cast-label (list clos-app (Fn-Coercion-Return crcn) ZERO-EXPR)))





;; Helpers that should be moved elsewhere
(: format-uid! : String Uid -> Uid)
(define (format-uid! fmt x)
  (next-uid! (format fmt (Uid-prefix x))))

;; Typed Racket Doesn't come with mutable set support
;; Wrap these to make an actual boundary
(define-type (MSet T) (HashTable T True))

(: mutable-set : (All (T) -> (MSet T)))
(define (mutable-set) (make-hasheq '()))

(: set->list (All (T) (MSet T) -> (Listof T)))
(define (set->list st) (hash-keys st))

(: set-add! (All (T) (MSet T) T -> Void))
(define (set-add! st e) (hash-set! st e #t))

(: set-remove! (All (T) (MSet T) T -> Void))
(define (set-remove! st e) (hash-remove! st e))

(: set-member? (All (T) (MSet T) T -> Boolean))
(define (set-member? s e) (hash-has-key? s e))

(: set-union! (All (T) (MSet T) (MSet T) -> Void))
(define (set-union! st0 st1)
  (for ([k (in-hash-keys st1)])
    (set-add! st0 k)))

(define-type (Stackof A) (Stack A))
(struct (A) Stack
  ([list : (Listof A)])
  #:mutable)

(: make-stack (All (A) (Listof A) -> (Stackof A)))
(define (make-stack ls) (Stack ls))

(: stack-push! (All (A) (Stackof A) A -> Void))
(define (stack-push! stk e)
  (set-Stack-list! stk (cons e (Stack-list stk))))

(: stack-pop! : (All (A) (Stackof A) -> A))
(define (stack-pop! stk)
  (define l (Stack-list stk))
  (set-Stack-list! stk (cdr l))
  (car l))

(define-type (G N) (Mutable-HashTable N (G N)))
;; Graph is a directed eq? graph with explicit
;; support for backtracking edges.
;; to vertices, and walking the graph.
;; - `to` = inverted graph represented as edges to each vertice
;; - `from` = regular graph represented as edges from vertices
;; invarients:
;; - (v, e) in `to` <-> (v, e) in `from`
(struct (N) Graph ([from : (G N)] [to : (G N)])
  #:transparent)

(: make-graph : (All (N) (Listof (Pairof N (Listof N))) -> (Graph N)))
(define (make-graph als)
  (define to   : (G N) (make-hasheq))
  (define from : (G N) (make-hasheq))
  (for ([row als])
    (match-define (cons v e*) row)
    (define-values (e*-from-v e*-to-v) (%add-vertex! from to v))
    (for ([w (in-list e*)])
      (define-values (e*-from-w e*-to-w) (%add-vertex! from to w))
      (hash-set! e*-from-v w e*-from-w)
      (hash-set! e*-to-w v e*-to-w)))
  (Graph from to))

;; add a vertice to the graph if it hasn't be done yet
(: %add-vertex! : (All (N) (G N) (G N) N -> (values (G N) (G N))))
(define (%add-vertex! from to v)
  (: mk : -> (G N))
  (define (mk) (make-hasheq))
  (values (hash-ref! from v mk)
          (hash-ref! to   v mk)))

(define graph? Graph?)

(module+ test
  (define als '((v w) (w v x) (x) (z)))
  (define g1 : (Graph Symbol) (make-graph als))
  (test-equal? "graph? true"  (graph? g1) #t)
  (test-equal? "graph? false" (graph? als) #f))

(: has-vertex? : (All (N) (Graph N) N -> Boolean))
(define (has-vertex? g v) (hash-has-key? (Graph-from g) v))

#;(: in-edges-of-vertex : (All (N) (Graph N) N -> (Sequenceof N (G N))))
#;
(define/match (in-edges-of-vertex g v)
  [((Graph from _) v) (in-hash (hash-ref from v))])

#;(: in-edges-to-vertex : (All (N) (Graph N) N -> (Sequenceof N (G N))))
#;
(define/match (in-edges-to-vertex g v)
  [((Graph _ to) v) (in-hash to)])

#;(: in-vertices : (All (N) (Graph N) N -> (Sequenceof N)))
#;
(define/match (in-vertices G)
  [((Graph from _)) (in-hash-keys from)])

#;(: in-edges : (All (N) (Graph N) -> (Sequenceof N (G N))))
#;
(define/match (in-edges G)
  [((Graph from _)) (in-hash from)])

(module+ test
  (test-equal? "has-vertex? true" (has-vertex? g1 'v) #t)
  (test-equal? "has-vertex? false" (has-vertex? g1 'a) #f))

(: has-edge? (All (A) (Graph A) A A -> Boolean))
(define (has-edge? g u v)
  (define e*-from-u? (hash-ref (Graph-from g) u #f))
  (and e*-from-u? (hash-ref e*-from-u? v #f) #t))

(module+ test
  (test-equal? "has-edge? true 1" (has-edge? g1 'v 'w) #t)
  (test-equal? "has-edge? true 2" (has-edge? g1 'w 'x) #t)
  (test-equal? "has-edge? false 1" (has-edge? g1 'a 'x) #f)
  (test-equal? "has-edge? false 2" (has-edge? g1 'x 'w) #f))

(: %add-edge! (All (A) (G A) (G A) A A -> Void))
(define (%add-edge! from to u v)
  (define-values (e*-from-u e*-to-u) (%add-vertex! from to u))
  (define-values (e*-from-v e*-to-v) (%add-vertex! from to v))
  (hash-set! e*-from-u v e*-from-v)
  (hash-set! e*-to-v u e*-to-u))

(: add-edge! (All (A) (Graph A) A A -> Void))
(define (add-edge! g u v)
  (%add-edge! (Graph-from g) (Graph-to g) u v))

(: remove-edge! (All (A) (Graph A) A A -> Void))
(define (remove-edge! g u v)
  (define edges-from-u? (hash-ref (Graph-from g) u #f))
  (cond
    [edges-from-u?
     (define edges-to-v (hash-ref (Graph-to g) v))
     (hash-remove! edges-from-u? v)
     (hash-remove! edges-to-v u)]))

(: add-vertex! (All (A) (Graph A) A -> Void))
(define (add-vertex! g v)
  (%add-vertex! (Graph-from g) (Graph-to g) v)
  (void))

(: get-vertices (All (A) (Graph A) -> (Listof A)))
(define (get-vertices g) (hash-keys (Graph-from g)))

(: in-vertices (All (A) (Graph A) -> (Sequenceof A)))
(define (in-vertices g) (in-mutable-hash-keys (Graph-from g)))

(: in-edges-from (All (A) (Graph A) A -> (Sequenceof A)))
(define (in-edges-from g a)
  (in-mutable-hash-keys (hash-ref (Graph-from g) a)))

(struct node
  ([index : Natural]
   [low-link : Natural]
   [on-stack? : Boolean])
  ;; I really want mutable nongenerative
  #:mutable
  #:transparent)

;; Tarjan's algorithm for strongly connected components
(: scc : (All (N) (Graph N) -> (Listof (Listof N))))
(define (scc G)
  (define nodes : (Mutable-HashTable N node) (make-hasheq))
  (define index : Natural 0) ;; Preorder index
  (define stack : (Listof (Pairof N node)) '())
  (define sccs  : (Listof (Listof N)) '())
  
  (for ([v (in-vertices G)])
    (unless (hash-has-key? nodes v)
      (let strong-connect : node ([v : N v])
        (define node-v (node index index #t))
        (hash-set! nodes v node-v)
        ;; Push v on stack
        (set! stack (cons (cons v node-v) stack))
        (set! index (add1 index))
        (for ([w (in-edges-from G v)])
          (define node-w (hash-ref nodes w #f))
          (cond
            [(not node-w)
             (define node-w (strong-connect w))
             (define w.ll (node-low-link node-w))
             (define v.ll (node-low-link node-v))
             (set-node-low-link! node-v (min w.ll v.ll))]
            [(node-on-stack? node-w)
             ;; Successor w is in stack S and hence in the current SCC
             ;; Note: The next line may look odd - but is correct.  It
             ;; says w.index not w.lowlink; that is deliberate and
             ;; from the original paper
             (define v.ll (node-low-link node-v))
             (define w.i  (node-index node-w))
             (set-node-low-link! node-v (min v.ll w.i))]
            ;; If w is not on stack, then (v, w) is a cross-edge in
            ;; the DFS tree and must be ignored
            [else (void)]))
        
        (when (= (node-low-link node-v) (node-index node-v))
          (define c : (Listof N)
            (let loop ([c : (Listof N) '()])
              ;; Pop w off of stack
              (match-define (cons w w-node) (car stack))
              (set! stack (cdr stack))
              (set-node-on-stack?! w-node #f)
              (let ([c (cons w c)])
                (if (eq? v w) c (loop c)))))
          (set! sccs (cons c sccs)))
        node-v)))
  sccs)

(: uid<? : Uid Uid -> Boolean)
(define (uid<? x y)
  (and (not (eq? x y))
       (or (string<? (Uid-prefix x) (Uid-prefix y))
           (< (Uid-suffix x) (Uid-suffix y)))))

