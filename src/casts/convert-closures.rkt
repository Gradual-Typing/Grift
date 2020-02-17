#lang typed/racket/base/no-check
#|------------------------------------------------------------------------------+
|Pass: src/casts/convert-closures                                               |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenschmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass performs closure conversion and optimization based off
of the paper Optimizing closures in O(n) time. A cursory analysis shows that
we don't quite get O(n) time, instead I think we are O(n^3). At this point
I am not really concerned about the runtime. 


- Closure Conversion is performed by the function `uncover-closure-ops`:
 - Records the free variables of each closure in the `Closure` form.
 - Reduces the number of free variables by eliminating free variables that are
   known to be :
   - bound to constants (constant propagation)
   - aliases of other free variables (copy propagation)
   - references to the current closure (self reference)
 - Generates names for the closure self-parameter and closure code
 - Calls function code directly when a closure is known (direct call optimization)
- Closure optimization is performed by the function `optimize-closures`:
  - Identifies closures with the same lifetime 
    - uses Tarjan algorithm to identify strongly connected components formed by
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
    closure is found in any argument position.
- a closure is *known* if we know that a variable binds it at a particular
  location. 

+-------------------------------------------------------------------------------+
TODO Investigate adding a stateful payload to Uids this could remove several
     hash table lookups.
TODO We can generate better code in this pass for function casts.

+------------------------------------------------------------------------------|#

(provide
 (rename-out
  ;; The `convert-closures` pass converts functions to an abstract
  ;; closure model that references free variables in a dictionary like
  ;; interface. This interface assumes no particular closure-model. If
  ;; the parameter `optimize-closures` is set to true then the pass
  ;; also removes some closures where it is safe. The removed closures
  ;; sometimes use a flat closure model, but are converted to memory
  ;; operations here to avoid any conflict with choosing a different
  ;; representation.
  [convert-closures-pass convert-closures]))

(require 
 racket/hash
 racket/list
 racket/match
 racket/set 
 "./cast-profiler.rkt"
 "../configuration.rkt"
 "../language/forms.rkt"
 "../language/syntax.rkt"
 "../lib/function.rkt"
 "../lib/dgraph.rkt"
 "../lib/mutable-set.rkt"
 "../logging.rkt"
 "../language/form-map.rkt")

(module+ test
  (require
   (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    syntax/location)
   rackunit))

;;  TODO move this to configuration and make these passes optional.
(define closure-optimizations : (Parameterof (Setof Symbol))
  (make-parameter
   (seteq 'self-reference
          'direct-call
          'propagate
          'static-allocation
          'degenerate-lambda-lifting
          'tuple-closures
          'recursive-sharing
          'lexical-sharing)))

(define currently-testing? : (Parameterof Boolean)
  (make-parameter #t))

(: convert-closures-pass (Cast-or-Coerce4-Lang -> Cast-or-Coerce6-Lang))
(define (convert-closures-pass prgm)
  (match prgm
    [(Prog
      (list name next type)
      (Static*
       (list bnd-mu-type* bnd-type* bnd-mu-crcn* bnd-crcn* bnd-const*) 
       main-expr))
     
     (define uc (make-unique-counter next))

     (define-values (static-closure* bnd-const*^ main-expr^)
       (parameterize ([current-unique-counter uc])
         (convert-closures bnd-const* main-expr)))
     (debug static-closure* bnd-const*^ main-expr^)
     
     (define-values (bnd-static-code* static-closure*^ bnd-const*^^ main-expr^^)
       (optimize-closures static-closure* bnd-const*^ main-expr^))

     (debug bnd-static-code* static-closure*^ bnd-const*^^ main-expr^^)
          
     (Prog (list name (unique-counter-next! uc) type)
       (Static*
        (list
         bnd-mu-type* bnd-type*
         bnd-mu-crcn* bnd-crcn*
         bnd-static-code* static-closure*^
         bnd-const*^^)
        main-expr^^))]))

;; The depth field keeps track of the depth of the binding that binds
;; this variable. If the depth is less than the current-lambda's
;; binding depth then the variable is a free variable of the lambda.
(struct Bound-Var
  ([uid : Uid]
   [var : (Var Uid)]
   [depth : Nat])
  #:transparent)

;; Here I use `Propagant` as things that are propagated
(define-type Propagant
  (U Bound-Var (Quote Cast-Literal)))

(define close-code-data-fn-proxy-application?
  (make-parameter #t))

(define (convert-closures
         const-bnd*
         main-expr
         #:propagate?      [propogate? (optimize? 'propagate)]
         #:direct-call?    [direct-call? (optimize? 'direct-call)]
         #:self-reference? [self-reference? (optimize? 'self-reference)]
         #:cast-rep [cast-rep (cast-representation)]
         #:fn-proxy-rep [fn-proxy-rep (fn-proxy-representation)]
         #:close-code-data-fn-app? [close-code-data-fn-app? (close-code-data-fn-proxy-application?)])
  (define well-known-closures : (MSet Uid)
    (mset))
  
  ;; hash that caches the results of calls to `make-apply-casted-closure!`
  (: arity->apply-casted-closure-closure
     (Mutable-HashTable Integer (Closure Uid CoC5-Expr)))
  (define arity->apply-casted-closure-closure (make-hash '()))
  (define arity->apply-casted-closure-code (make-hash '()))

  ;; TODO if we kept an immutable hash of symbol->uid we could
  ;; get away without storing the cast is every fn-proxy node.
  (define make-apply-casted-closure!
    (case fn-proxy-rep
      [(Data)
       (cond
         [close-code-data-fn-app?
          (lambda (arity cast-uid compose-uid)
            (match (hash-ref arity->apply-casted-closure-code arity #f)
              [(cons l _) (Code-Label l)]
              [_
               (define label (next-uid! (format "apply_proxied_closure_arity_~a" arity))) 
               ;; The uids for formal parameters of this function.
               (define p* (build-list arity (lambda (a) (next-uid! "arg"))))
               ;; and likewise variable for those arguments.
               (define v* (map Var p*))
               ;; The free-variables of this closure.
               (define closure (next-uid! "closure"))
               (define coercion (next-uid! "coercion"))
               (hash-set!
                arity->apply-casted-closure-code
                arity
                (cons
                 label
                 (Code `(,closure ,coercion . ,p*)
                       (cast-apply-cast cast-uid
                                        compose-uid
                                        (Var closure)
                                        v*
                                        (Var coercion)))))
               (Code-Label label)]))]
         [else
          (lambda _ (error 'make-apply-casted-closure! "this shouldn't be called"))])]
      [else
       (lambda (arity cast-uid compose-uid)
         (cond
           [(hash-ref arity->apply-casted-closure-closure arity #f) => values]
           [else
            ;; NOTE: The following code is tricky and fragile. Read the
            ;; comments and write/run tests when modified.
            
            ;; This is the name of the closure that is never allocated.
            ;; After closure conversion it should never be seen.
            (define name (next-uid! ""))
            ;; The label for the code that applies arbitrary casted closures
            ;; for functions of this arity.
            (define label (next-uid! (format "apply_casted_closure_arity~a" arity))) 
            ;; The self variable for the code
            (define self (next-uid! "hybrid_closure_self"))
            ;; The uids for formal parameters of this function.
            (define p* (build-list arity (lambda (a) (next-uid! "arg"))))
            ;; and likewise variable for those arguments.
            (define v* (map Var p*))
            ;; The free-variables of this closure.
            (define closure-field (next-uid! "closure-field"))
            (define coercion-field (next-uid! "coercion-field"))
            ;; TODO the apply line just uses the caster from here
            ;; code-gen cast-casted-closure and used that code
            ;; as the castor for this code.
            ;; TODO Can we speed up applications of this code?
            (define apply-casted-closure-i
              (Closure 
               name #f 'code-only label self
               #f (list closure-field coercion-field) p*
               (cast-apply-cast cast-uid
                                compose-uid
                                (Var closure-field)
                                v*
                                (Var coercion-field))))
            ;; Update the bindings that we have generate
            (hash-set! arity->apply-casted-closure-closure
                       arity
                       apply-casted-closure-i)
            apply-casted-closure-i]))]))
  
  (: convert-closures-in-expr : CoC4-Expr (HashTable Uid Propagant) -> CoC5-Expr)
  (define (convert-closures-in-expr expr static-env)
    (let rec/env ([current-expr : CoC4-Expr expr]
                  ;; Immutable map from function (closure) names to code labels
                  [code-env : (HashTable Uid Uid) (hasheq)]
                  ;; Map from variable names to representative constant or variable
                  [opt-env : (HashTable Uid Propagant) static-env]
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
                  [free-variables : (MSet Uid) (mset)])
      (: propogate/recognize : (->* (Uid) (#:rator? Boolean) CoC5-Expr))
      ;; - propagate representative values and their binding depth
      ;; - recognize free-variable occurrences
      ;; - recognize escaping closures 
      (define (propogate/recognize u #:rator? [rator? #f])
        ;; Folding Constants and Aliases can reduce closure size
        ;; particularly when we rely on the c compiler to perform
        ;; these operations for us.
        (debug off u)
        (define propagant (hash-ref opt-env u))
        (define p/r-ret
          (cond
            [(Bound-Var? propagant)
             (match-define (Bound-Var u v binding-depth) propagant)
             (unless rator?
               (debug off well-known-closures u)
               (mset-remove! well-known-closures u))
             (cond
               ;; If the variable is bound at level 0 it is static
               ;; and as a result doesn't have to be captured.
               [(= binding-depth 0) v]
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
                (mset-add! free-variables u)
                v]
               [else v])] 
            ;; The variable was bound to a constant that can be propogated
            [else propagant]))
        (debug off u p/r-ret))


      (let rec ([current-expr current-expr])
        (debug off current-expr)

        (: App-Fn-build-closure-app : (Var Uid) (Listof CoC5-Expr) -> CoC5-Expr) 
        (define (App-Fn-build-closure-app v e*) (Closure-App (Closure-Code v) v e*))

        (define (do-app-fn build-closure-app e e*)
          (: let-clos-app : CoC5-Expr (Listof CoC5-Expr) -> CoC5-Expr) 
          (define (let-clos-app e e*)
            (let$ ([closure e]) (build-closure-app closure e*)))
          (match e
            [(Var u)
             (match (propogate/recognize u #:rator? #t)
               [(and v (Var u))
                (define label? (hash-ref code-env u #f))
                (cond 
                  [label?
                   ;; Invariant: The latter closure optimization
                   ;; passes identify known function application by
                   ;; the presence of a code-label literal in code
                   ;; position.  Note: This code works even in the
                   ;; data representation of fn-proxies because if
                   ;; we know what lambda a variable maps to then
                   ;; we know it isn't casted.
                   (Closure-App (Code-Label label?) v e*)]
                  [else (build-closure-app v e*)])]
               [other
                ;; There are no propagants except variables who's types
                ;; are functions.
                (error 'convert-closures "this shouldn't happen: " other)])] 
            [e (let-clos-app (rec e) e*)]))

        (match current-expr
          ;; Variable found in non-application position
          ;; All variables found in application position are not recurred on
          [(Var u) (propogate/recognize u)]
          [(Letrec lr-bnd* lr-body)
           (debug off current-expr)
           (define letrec-binding-depth (add1 current-binding-depth))
           (define closures-in-this-binding : (MSet Uid) (mset))
           (define-values (code-label* self-parameter*)
             (for/lists ([code-label* : Uid*]
                         [self-parameter* : Uid*])
                        ([lr-bnd lr-bnd*])
               (match-define (cons name _) lr-bnd)
               (define label (format-uid! "~a_code" name))
               (define self (format-uid! "~a_closure_self" name))
               (mset-add! closures-in-this-binding name)
               ;; Letrecs only bind closures due to the previous
               ;; purify-letrec pass. Assume all closures are well
               ;; known. If the variable for this closure appears
               ;; outside of the operator position for function
               ;; application then remove it from this set.
               (mset-add! well-known-closures name)
               (mset-add! well-known-closures self)
               (values label self)))

           ;; Extend the code-env bindings for Optimize Direct Call
           ;; Extend opt-env without trying to eliminate bindings.
           (define-values (code-env^ opt-env^) 
             (for/fold ([code-env^ : (HashTable Uid Uid) code-env]
                        [opt-env^ : (HashTable Uid Propagant) opt-env])
                       ([code-label code-label*]
                        [lr-bnd lr-bnd*])
               (match-define (cons x _) lr-bnd)
               (define bv (Bound-Var x (Var x) letrec-binding-depth)) 
               (values (hash-set code-env^ x code-label)
                       (hash-set opt-env^  x bv))))

           ;; Note: before we decide if a closure is well known we
           ;; must process all the expression in which variables for
           ;; that closure may appear...
           ;; So we recur into the body...
           (define lr-converted-body
             (rec/env lr-body code-env^ opt-env^
                      letrec-binding-depth
                      current-closure-name
                      current-closure-self
                      current-closure-depth
                      free-variables))

           ;; And then we process all of the letrec bindings ...
           (define-values (lr-bnd-fv** lr-bnd-converted-body*)
             (for/lists ([lr-bnd-fv** : (Listof Uid*)]
                         [lr-bnd-converted-body* : (Listof CoC5-Expr)])
                        ([lr-bnd lr-bnd*]
                         [code-label code-label*]
                         [self self-parameter*])
               (match-define
                 (cons closure-name (Lambda p* (Castable c? lr-bnd-body)))
                 lr-bnd)
               (define lambda-binding-depth (add1 letrec-binding-depth))
               (define fvs : (MSet Uid) (mset))
               (define code-env^^ (hash-set code-env^ self code-label))
               ;; Optimize self reference
               (define self-bv (Bound-Var self (Var self) lambda-binding-depth))
               (define opt-env^2
                 ;; Rewrite name as self
                 (hash-set
                  (hash-set opt-env^ self self-bv)
                  closure-name self-bv)) 
               (define opt-env^3
                 (for/fold ([env : (HashTable Uid Propagant) opt-env^2])
                           ([p p*])
                   (hash-set env p (Bound-Var p (Var p) lambda-binding-depth))))

               (define lr-bnd-converted-body
                 (rec/env lr-bnd-body code-env^^ opt-env^3
                          lambda-binding-depth
                          closure-name
                          self
                          letrec-binding-depth
                          fvs))

               ;; We have to check each free variable at this depth
               ;; to see if they need added to the current free variable set.
               ;; TODO find a way not to perform this iteration/lookup again
               ;; The sort here makes the ordering of the list deterministic.

               (define lr-bnd-body-fv* (sort (mset->list fvs) uid<?))

               (for ([u lr-bnd-body-fv*])
                 (match (hash-ref opt-env^ u #f)
                   [(Bound-Var _ _ bd)
                    #:when (<= bd current-closure-depth)
                    (mset-add! free-variables u)]
                   [other (void)]))
               
               (values lr-bnd-body-fv*
                       lr-bnd-converted-body)))

           ;; Only after we have recurred on all of the bindings
           ;; and the body can we decide if the closure is well known
           ;; and populate all the metadata.

           ;; - Rebuild each closure
           ;; - map each closure name to the closure
           ;; - Build graph, `g`, of capture relationship of these bindings 
           (define captures : (DGraph Uid) (make-dgraph))
           (define u->c-hash
             (for/hasheq : (HashTable Uid (Closure Uid CoC5-Expr))
                 ([lr-bnd lr-bnd*]
                  [label code-label*]
                  [self self-parameter*]
                  [lr-bnd-fv* lr-bnd-fv**]
                  [lr-bnd-converted-body  lr-bnd-converted-body*])
               (match-define (cons name (Lambda fp* (Castable c? _))) lr-bnd)
               ;; Only after we have done every recursive call can we know
               ;; if the closure is well-kown.
               (define wk?
                 (and (mset-member? well-known-closures name)
                      (mset-member? well-known-closures self)))
               (debug off well-known-closures name self wk?)
               (define c
                 (Closure name wk? 'regular label self c?
                          lr-bnd-fv* fp* lr-bnd-converted-body))
               (dgraph-add-vertex! captures name)
               (for ([fv lr-bnd-fv*])
                 ;; we only care about the free variables that are bound
                 ;; in this binding. Filtering these here prevents `scc`
                 ;; from having to deal with them, and awkward code later
                 ;; when we reconstruct the bindings.
                 (when (mset-member? closures-in-this-binding fv)
                   ;; add-edge adds `name` as a vertex if it hasn't be
                   ;; added yet.
                   (dgraph-add-edge! captures name fv)))
               (values name c)))

           (flush-output (current-output-port))
           (: u->c : Uid -> (Closure Uid CoC5-Expr))
           (define (u->c x) (hash-ref u->c-hash x))

           ;; Compute the strongly connected components (scc*)
           ;; i.e. The sets of mutually recursive bindings.
           ;; Tarjan's algorithm also topologically sorts the bindings
           (define scc* : (Listof Uid*)
             (for/list ([scc (reverse (dgraph-scc captures))])
               (sort scc uid<?)))

           (let loop ([scc* scc*])
             (match scc*
               ['() lr-converted-body]
               [(cons scc scc*) (Let-Closures (map u->c scc) (loop scc*))]))]
          [(Labels b* b)
           (define code-binding-depth (add1 current-binding-depth))
           (define rec-b* : (Bnd* (Fun CoC5-Expr))
             (for/list ([b b*])
               (match-define (cons l (Code p* e)) b)
               (define opt-env^
                 (for/fold ([e : (HashTable Uid Propagant) opt-env])
                           ([u p*])
                   (hash-set e u (Bound-Var u (Var u) code-binding-depth))))
               (cons l (Code p*
                             (rec/env e (hasheq) opt-env^
                                      code-binding-depth
                                      current-closure-name
                                      current-closure-self
                                      current-closure-depth
                                      (mset))))))
           (Labels rec-b* (rec b))]
          [(Let b* e)
           (define let-binding-depth (add1 current-binding-depth))
           (define-values (b*^ opt-env^)
             (for/fold ([b*^ : (Bnd* CoC5-Expr) '()]
                        [env : (HashTable Uid Propagant) opt-env])
                       ([b b*])
               (match-define (cons u e) b)
               ;; Invoking `rec` here would cause free variables that
               ;; are aliased but not used to be recorded as being free.
               (define possibly-free-vars : (MSet Uid) (mset))
               ;; Notes on recursion:
               ;; - recuring on the rhs `e` of binding
               
               ;; - current-depth isn't incremented because the binding hasn't occurred
               ;; - current-closure-* remains the same
               ;; - possibly-free-vars will be added to free variables if
               ;;   the binding isn't eliminated.
               (match e
                 ;; Eliminate aliases
                 [(Var u^)
                  (values b*^ (hash-set env u (hash-ref opt-env u^)))]
                 ;; Propogate Constants
                 [(and (Quote _) lit) 
                  (values b*^ (hash-set env u lit))]
                 [e
                  (define rec-e
                    (rec/env e code-env opt-env
                             current-binding-depth
                             current-closure-name
                             current-closure-self
                             current-closure-depth
                             possibly-free-vars))
                  ;; Can't eliminate this binding
                  (define bv (Bound-Var u (Var u) let-binding-depth))
                  ;; Need to merge the free variables of this expression
                  (mset-union! free-variables possibly-free-vars)
                  (values (cons (cons u rec-e) b*^)
                          (hash-set env u bv))]))) 

           (define rec-e : CoC5-Expr
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
             [(null? b*^) rec-e]
             [else (Let b*^ rec-e)])]
          [(Repeat i e1 e2 a e3 e4)
           (define rec-e1 (rec e1))
           (define rec-e2 (rec e2))
           (define rec-e3 (rec e3))
           (define repeat-binding-depth (add1 current-binding-depth))
           (define opt-env^
             (hash-set
              (hash-set opt-env i (Bound-Var i (Var i) repeat-binding-depth))
              a (Bound-Var a (Var a) repeat-binding-depth)))
           (define rec-e4
             (rec/env e4 code-env opt-env^
                      repeat-binding-depth
                      current-closure-name
                      current-closure-self
                      current-closure-depth
                      free-variables))
           (Repeat i rec-e1 rec-e2 a rec-e3 rec-e4)]
          ;; Castable Function Operations
          [(Fn-Caster e)
           ;; TODO If e is well known variable then we should be able
           ;; to know its arity and thus the static code that this
           ;; gets resolved to.
           (Closure-Caster (rec e))]
          [(App-Fn e (app (cmap rec) e*))
           (do-app-fn App-Fn-build-closure-app e e*)]
          ;; Fn-Proxy Operations
          ;; App-Fn-or-Proxy is very similar to Fn-App
          ;; If we were lowering forms (Fn-Cast e t1 t2 l)
          ;; and (Fn-Coercion e c) then we might be able
          ;; to do better here, because we have kept track
          ;; of if we can see the lambda.
          ;; But this also might be doable in a constant folding pass.
          [(App-Fn-or-Proxy cast-uid compose-uid e (app (cmap rec) e*))
           (case cast-rep
             [(Coercions Hyper-Coercions)
              (case fn-proxy-rep
                ;; Note: this case should be precisely the same as
                ;; Fn-App
                [(Hybrid) (do-app-fn App-Fn-build-closure-app e e*)]
                [(Data)
                 (: cast-clos-app :
                    Uid Uid CoC5-Expr (Listof CoC5-Expr) -> CoC5-Expr)
                 (define (cast-clos-app cast compose v v*)
                   (If (Fn-Proxy-Huh v)
                       (let$ ([data-fn-proxy-closure (Fn-Proxy-Closure v)]
                              [data-fn-proxy-coercion (Fn-Proxy-Coercion v)])
                             (cond
                         [close-code-data-fn-app?
                          (App-Code
                           (make-apply-casted-closure! (length v*) cast compose)
                           `(,data-fn-proxy-closure
                             ,data-fn-proxy-coercion
                             . ,v*))]
                         [else
                          (cast-apply-cast
                           cast compose data-fn-proxy-closure v* data-fn-proxy-coercion)])) 
                       (Closure-App (Closure-Code v) v v*)))
                 (: let-cast-clos-app :
                    Uid Uid CoC5-Expr (Listof CoC5-Expr) -> CoC5-Expr)
                 (define (let-cast-clos-app cast compose e e*)
                   (let$ ([maybe-data-fn-proxy e])
                         (cast-clos-app cast compose e e*)))
                 (define e*^ (map rec e*))
                 (match e
                   [(and v (Var u))
                    (cond
                      ;; If it a data representation and it is a
                      ;; known definition site then it can't be a
                      ;; proxy because we can see the lambda
                      [(hash-ref code-env u #f)
                       =>
                       (lambda ([u : Uid])
                         ;; Invariant: The latter closure optimization passes
                         ;; identify known function application by the presence
                         ;; of a code-label literal in code position.
                         (Closure-App (Code-Label u) v e*^))]
                      [else (cast-clos-app cast-uid compose-uid v e*^)])] 
                   [e^ (let-cast-clos-app cast-uid compose-uid (rec e^) e*^)])])])]
          [(Fn-Proxy `(,i ,cast ,compose) (app rec clos) (app rec crcn))
           (case cast-rep
             [(Coercions Hyper-Coercions)
              (case fn-proxy-rep              
                ;; For the hybrid representation we allocate a closure
                ;; That contains the free variables in the correct order. 
                [(Hybrid)
                 ;; There is an invarient here that Fn-Proxy is never
                 ;; applied to anything that isn't a raw closure. 
                 (define name (next-uid! (format "casted-closure-~a" i)))
                 ;; The free variables are how closures capture so here
                 ;; we have to give the closure free variable that match
                 ;; the order of the closure code that we are invoking
                 (define closure (next-uid! "closure-field"))
                 (define coercion (next-uid! "coercion-field"))
                 (match-define (Closure _ _ _ label _ caster? _ p* _)
                   (make-apply-casted-closure! (if (enable-crcps?) (+ i 1) i) cast compose))
                 (Let (list (cons closure clos) (cons coercion crcn))
                      (Let-Closures
                       ;; The layout of this closure is very specific
                       ;; the closure must be the first free variable
                       ;; the coercions the second
                       ;; any change to this requires an update to the
                       ;; constants in constants-and-codes.rkt
                       (list (Closure name #f 'closure-only label name caster? 
                                      (list closure coercion) p* ZERO-EXPR))
                       (Closure-Proxy (Var name))))]
                [(Data) (Fn-Proxy i clos crcn)]
                [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
             [else (error 'convert-closures "unkown cast representation")])]
    [(Fn-Proxy-Huh (app rec e))
     (case cast-rep
       [(Coercions Hyper-Coercions)
        (case fn-proxy-rep
          [(Hybrid) (Hybrid-Proxy-Huh e)]
          [(Data)   (Fn-Proxy-Huh e)]
          [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
       [else (error 'convert-closures "unkown cast representation")])]
    ;; The Hybrid cases that follow allow specify representation
    ;; to exploit the layout chosen for closure representation
    ;; to reference the closure and coercion fields outside of
    ;; the closures.
    [(Fn-Proxy-Closure (app rec e))
     (case cast-rep
       [(Coercions Hyper-Coercions)
        (case fn-proxy-rep
          [(Hybrid) (Hybrid-Proxy-Closure e)]
          [(Data)   (Fn-Proxy-Closure e)]
          [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
       [else (error 'convert-closures "unkown cast representation")])]
    [(Fn-Proxy-Coercion (app rec e)) 
     (case cast-rep
       [(Coercions Hyper-Coercions)
        (case fn-proxy-rep
          [(Hybrid) (Hybrid-Proxy-Coercion e)]
          [(Data)   (Fn-Proxy-Coercion e)]
          [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
       [else (error 'convert-closures "unkown cast representation")])] 
    [else (form-map current-expr rec)]))))

  (define static-env 
    (for/hasheq : (HashTable Uid Propagant)
                ([b const-bnd*])
      (match-define (cons id expr) b)
      ;; Static bindings are the only binding with binding depth 0
      (values id (Bound-Var id (Var id) 0))))

  (define new-const-bnd* : (Bnd* CoC5-Expr)
    (for/list ([b const-bnd*])
      (match-define (cons id const-expr) b)
      (cons id (convert-closures-in-expr const-expr static-env))))
  
  (define closure-coverted-main-expr : CoC5-Expr
    (convert-closures-in-expr main-expr static-env))

  (define code* (sort (hash-values arity->apply-casted-closure-code) bnd-code<?))
  
  (define clos* (sort (hash-values arity->apply-casted-closure-closure) closure<?))

  (values clos*
          new-const-bnd*
          (if (null? code*)
              closure-coverted-main-expr
              (Labels code* closure-coverted-main-expr))))

(struct shared-tuple ([closure : Uid][free-variables : Uid*]))
(struct shared-closure ([closure : Uid][free-variables : Uid*]))
(define-type Shared-Closure (U shared-tuple shared-closure))

(: optimize-closures :
   (Closure* Uid CoC5-Expr) (Bnd* CoC5-Expr) CoC5-Expr
   -> (Values (Bnd* (Fun CoC6-Expr))
              (Closure* CoC6-Expr CoC6-Expr)
              (Bnd* CoC6-Expr)
              CoC6-Expr))
(define (optimize-closures c* const-bnd* e)
  ;; Closures that are not well-known but contain no free-variables
  ;; can be statically allocated and refered to by there constants name.
  ;; Since they have no free variables, and thus no dependencies, the order of
  ;; these bindings do not matter.
  (define static-code : (Bnd* (Code Uid* CoC6-Expr)) '())
  (define static-closures : (Closure* CoC6-Expr CoC6-Expr) '())

  ;; These uids no longer need to be captured because they now refer
  ;; to static bindings or all references to the variable have been
  ;; eliminated.
  (define closures-eliminated : (MSet Uid) (mset))
  (: eliminated? : Uid -> Boolean)
  (define (eliminated? x) (mset-member? closures-eliminated x))

  ;; This type describes how the `Closure-App` form needs to be compiled
  (define-type Closure-Status
    (U 'regular 'free-variables 'eliminated))
  (define label->closure-status : (Mutable-HashTable Uid Closure-Status)
    (make-hasheq))

  (define-type Uid->Set.Clos
    (Mutable-HashTable Uid (Pairof (Setof Uid) Shared-Closure)))
  (define-type Set->Clos (Immutable-HashTable (Setof Uid) Shared-Closure))
  (define uid->fv-set.clos : Uid->Set.Clos (make-hash))
  (: filter-shared-closures : (Listof Uid) -> Set->Clos)
  (define (filter-shared-closures fv-ls)
    (for/fold ([s->c : Set->Clos (hash)])
              ([fv fv-ls])
      (match (hash-ref uid->fv-set.clos fv #f)
        [#f s->c]
        [(cons fv-set sc) (hash-set s->c fv-set sc)])))

  (: rec/env :
     CoC5-Expr (HashTable Uid Uid) (HashTable Uid CoC6-Expr) Set->Clos
     -> CoC6-Expr)
  (define (rec/env e uid->uid fv->expr fv-set->shared-closure)
    (let rec ([e e])
      (debug off e uid->uid fv->expr fv-set->shared-closure label->closure-status)
      (match e
        [(Let-Closures '() e) (rec e)] 
        [(Let-Closures c* e)
         (define wk? (inst Closure-well-known? Uid CoC5-Expr))
         (define-values (wk-c* nwk-c*) (partition wk? c*))

         (define wk-name* (map (inst Closure-name Uid CoC5-Expr) wk-c*))
         (define wk-name-set (list->seteq wk-name*))
         
         (match nwk-c*
           ;; |!wk| = 0 -> share a closure among the well known
           ;; - no need to capture any of variables in this binding
           ;;   because everone will end up passing self to each other.
           ['()
            (define-values (fv-set/this-binding uid->uid^)
              ;; remove any eliminated closures
              ;; and merge any closures that where renamed
              (for/fold ([fv-set : (Setof Uid) (seteq)]
                         [u->u : (HashTable Uid Uid) uid->uid])
                        ([c wk-c*])
                (resolve-free-variables
                 eliminated? u->u (Closure-free-vars c) fv-set)))
            
            (define fv-set (set-subtract fv-set/this-binding wk-name-set))
            
            (define fv-ls (sort (set->list fv-set) uid<?))

            (match fv-ls
              ;; No free variables -> No Closure
              ['() 
               (for ([c wk-c*])
                 (mset-add! closures-eliminated (Closure-name c))
                 (mset-add! closures-eliminated (Closure-self c))
                 (hash-set! label->closure-status
                            (Closure-code-label c)
                            'eliminated))


               ;; There are no fvs in this code to have closures
               (define empty-fv-set->clos : Set->Clos (hash))
               ;; These closures will have no free variables to rewrite
               (define empty-fv->expr : (HashTable Uid CoC6-Expr) (hasheq))
               
               ;; Change each closure to a non-capturing function
               (for ([c wk-c*])
                 (match-define (Closure _ _ _ label _ _ _ p* e) c)
                 ;; We know the caster isn't needed because the closure
                 ;; never got passed as an argument (or else it wouldn't
                 ;; be well-known).
                 (define rec-e (rec/env e uid->uid^
                                        empty-fv->expr
                                        empty-fv-set->clos))
                 (set! static-code (cons (cons label (Code p* rec-e))
                                         static-code)))
               
               (rec/env e uid->uid^ fv->expr fv-set->shared-closure)] 

              ;; Well known closures with only 1 free variable
              [(list fv)
               (for ([c wk-c*])
                 (hash-set! label->closure-status
                            (Closure-code-label c)
                            'free-variables))
               
               (define fv-set->clos (filter-shared-closures (list fv)))
               
               (for ([c wk-c*])
                 (match-define (Closure _ _ _ label self _ _ p* e) c)
                 ;; Rewrite all occurences of fv as the self parameter.
                 ;; Note this is done with uid->uid so that if the
                 ;; fv and self are captured then only a single variable
                 ;; is captured.
                 (define uid->uid^^
                   (map-uid*-to uid->uid^ (cons fv wk-name*) self))
                 
                 (define rec-e (rec/env e uid->uid^^ fv->expr fv-set->clos))
                 (set!
                  static-code
                  `([,label . ,(Code `(,self . ,p*) rec-e)] . ,static-code)))
               
               (define uid->uid^^ (map-uid*-to uid->uid^ wk-name* fv)) 
               
               (rec/env e uid->uid^^ fv->expr fv-set->shared-closure)]
              ;; All wk w/ 1 < fvs
              [fv-ls
               ;; We need a structure storing the free-variables, so
               ;; check to see if there is a closure with these
               ;; free-variables in scope.
               (match (hash-ref fv-set->shared-closure fv-set #f)
                 ;; There are no closures in scope with these free
                 ;; variables so we allocate a tuple to share amongst
                 ;; all the wk closures.
                 [#f
                  ;; There has to be one closure, because there are
                  ;; free variables.
                  (match-define (cons fst rst) wk-name*)
                  
                  (for ([c wk-c*])
                    (hash-set! label->closure-status
                               (Closure-code-label c)
                               'free-variables))
                  
                  (define fv-set->clos (filter-shared-closures fv-ls))
                  
                  (closures-borrow-tuple wk-c* fv-ls wk-name* uid->uid^
                                         fv-set->clos fv-set)
                  ;; The first closure gets bound to a tuple all the
                  ;; rest get renamed to be the first closure.
                  (define uid->uid^^ (map-uid*-to uid->uid^ rst fst))
                  ;; Add this closure to the filter set so that it can
                  ;; be used as the scope narrows.
                  (define sc (shared-tuple fst fv-ls))
                  ;; TODO write a test for this code path
                  (hash-set! uid->fv-set.clos fst (cons fv-set sc))
                  ;; Allocate a tuple with the free-variables
                  (Let `([,fst . ,(Create-tuple (fv*->expr* fv->expr fv-ls))])
                    (rec/env e uid->uid^^ fv->expr
                             (hash-set fv-set->clos fv-set sc)))]
                 ;; There is another well-known closure that
                 ;; has the same free variable set, we use that
                 ;; free
                 [(shared-tuple tuple-name fv-ls)
                  ;; There has to be one closure, because there are
                  ;; free variables.
                  (for ([c wk-c*])
                    (hash-set! label->closure-status
                               (Closure-code-label c)
                               'free-variables))

                  
                  (define fv-set->clos (filter-shared-closures fv-ls))

                  (closures-borrow-tuple wk-c* fv-ls wk-name* uid->uid^
                                         fv-set->clos fv-set)

                  ;; The tuple gets passed as the closure for all the
                  ;; closures.
                  (define uid->uid^^ (map-uid*-to uid->uid^ wk-name* tuple-name))

                  (rec/env e uid->uid^^ fv->expr fv-set->clos)]
                 [(shared-closure clos-name fv-ls)
                  (for ([c wk-c*])
                    (hash-set! label->closure-status
                               (Closure-code-label c)
                               'regular))
                  
                  (closures-borrow-closure wk-c* fv-ls fv-set '() uid->uid^
                                           (filter-shared-closures fv-ls))

                  (define uid->uid^^ (map-uid*-to uid->uid^ wk-name* clos-name))
                  
                  (rec/env e uid->uid^^ fv->expr fv-set->shared-closure)])])]
           ;; At least one not-well-known closure
           ;; -> add the fvs of wk to !wk and share !wk closure with wk
           [not-wk-c*
            ;; First approximation pick arbitrary !wk to lend to wks
            (match-define (Closure fst-name
                                   #f fst-mode
                                   fst-label fst-self
                                   fst-ctr? fst-fv-ls fst-fp-ls fst-e)
              (car not-wk-c*))

            (when (or (eq? fst-mode 'code-only)
                      (eq? fst-mode 'closure-only))
              (unless (null? (cdr not-wk-c*))
                (error 'grift/convert-closures
                       (string-append
                        "Invariant Violated:"
                        "This code avoids code duplication by relying on the fact "
                        "that we know that code-only or closure-only closures always "
                        "appear in a singleton binding."))))
                        
            (hash-set! label->closure-status fst-label 'regular)
            
            (define rst-nwk* (cdr not-wk-c*))


            (define-values (fst-fv-set uid->uid^)
              (resolve-free-variables eliminated? uid->uid fst-fv-ls))

            ;; Add fvs of wk closure to fvs of
            ;; - don't add fst-name if present
            ;; TODO use best uid->uid here 
            (define-values (fv-set/wk-names/fst-name _)
              (for*/fold ([fv-set : (Setof Uid) fst-fv-set]
                          [u->u : (HashTable Uid Uid) uid->uid])
                         ([c wk-c*])
                (resolve-free-variables eliminated? u->u
                                        (Closure-free-vars c)
                                        fv-set)))

            (define fv-set/wk-names
              (set-remove fv-set/wk-names/fst-name fst-name))
            
            (define fv-set (set-subtract fv-set/wk-names wk-name-set))
            
            (define fv-ls (sort (set->list fv-set) uid<?))

            ;; Have to figure out sharable closures before recurring
            (define fst-ext-sc (shared-closure fst-name fv-ls))
            (define fst-int-sc (shared-closure fst-self fv-ls))
            (hash-set! uid->fv-set.clos fst-name (cons fv-set fst-ext-sc))
            (hash-set! uid->fv-set.clos fst-self (cons fv-set fst-int-sc))

            ;; In the other !wk closures and the body the
            ;; well-known closure just use the fst closure
            (define uid->uid^^ (map-uid*-to uid->uid^ wk-name* fst-name))

            
            (define-values (rst-fv-ls* rst-fv-set* rst-int-sc* rst-ext-sc*)
              ;; TODO consider threading the uid map through
              (for/lists ([fv-ls* : (Listof Uid*)]
                          [fv-set* : (Listof (Setof Uid))]
                          [int-sc* : (Listof Shared-Closure)]
                          [ext-sc* : (Listof Shared-Closure)])
                         ([c rst-nwk*])
                (match-define (Closure name #f 'regular label self
                                       ctr? f* p* rec-e)
                  c)
                (hash-set! label->closure-status label 'regular)
                (define-values (fv-set _) 
                  (resolve-free-variables eliminated? uid->uid^^ f*))
                (define fv-ls (sort (set->list fv-set) uid<?))
                (define int-sc (shared-closure self fv-ls))
                (define ext-sc (shared-closure name fv-ls))
                (hash-set! uid->fv-set.clos name (cons fv-set ext-sc))
                (hash-set! uid->fv-set.clos self (cons fv-set int-sc))
                (values fv-ls fv-set int-sc ext-sc)))
            
            ;; The shared closures for the lending and wk closures
            ;; this should come after all the fv-set.clos registration
            (define fv-set->clos (filter-shared-closures fv-ls))

            (define all-fv-set
              (for*/fold ([fv-set : (Setof Uid) fv-set])
                         ([fv-set^ rst-fv-set*])
                (set-union fv-set fv-set^)))

            (define all-names (map (inst Closure-name Uid CoC5-Expr) c*))
            (define all-fv-not-bound-here : (Setof Uid)
              (set-subtract all-fv-set (list->seteq all-names)))
            
            (cond
              ;; A special case is when there are no free variables besides
              ;; the names that are actually bound in this environment.
              ;; In that case we still need closures for the !wk closures
              ;; but they can be statically allocated and thus eliminated
              ;; from the free variables of other closures. 
              [(set-empty? all-fv-not-bound-here)
               ;; We eliminate all names so that even remapped names
               ;; don't appear in closures, they are now static
               ;; and available at the global level.
               (for ([name all-names])
                 (mset-add! closures-eliminated name))
               
               (for ([c wk-c*])
                 (hash-set! label->closure-status (Closure-code-label c) 'eliminated))
               (hash-set! label->closure-status fst-label 'regular)
               (for ([c rst-nwk*])
                 (hash-set! label->closure-status (Closure-code-label c) 'regular))
               
               (define uid->uid^^ : (HashTable Uid Uid)
                 (for/fold ([u->u uid->uid^])
                           ([c c*]) 
                   (hash-set u->u (Closure-self c) (Closure-name c))))

               (define rec-e (rec/env fst-e uid->uid^^ (hasheq) (hash)))
               
               (set!
                static-closures
                (cons
                 (Closure
                  fst-name #f fst-mode fst-label fst-self fst-ctr?
                  '() fst-fp-ls rec-e)
                 static-closures))

               
               (for ([c wk-c*])
                 (match-define (Closure _ _ _ label _ _ _ p* e) c)
                 ;; We know the caster isn't needed because the closure
                 ;; never got passed as an argument (or else it wouldn't
                 ;; be well-known).
                 (define rec-e (rec/env e uid->uid^ fv->expr fv-set->clos))
                 (set! static-code (cons (cons label (Code p* rec-e))
                                         static-code)))
               
               ;; The rest get mostly regular closure conversion
               (for ([nwk-c    rst-nwk*]
                     [fv-ls  rst-fv-ls*]
                     [fv-set rst-fv-set*]
                     [sc     rst-int-sc*])
                 (match-define
                   (Closure name #f 'regular label self ctr? _ p* nwk-c-body)
                   nwk-c)
                 (define fv-init* (fv*->expr* fv->expr fv-ls))
                 (Closure
                  name #f 'regular label self ctr? fv-init* p*
                  (rec/env nwk-c-body
                           uid->uid^^
                           (map-fv->closure-ref self fv-ls)
                           (filter-shared-closures fv-ls))))
               
               (define fv-set->clos^
                 (hash-set fv-set->shared-closure fv-set fst-ext-sc))
               
               (define fv-set->clos^^
                 (for/fold ([s->c : Set->Clos fv-set->clos^])
                           ([s rst-fv-set*] [c rst-ext-sc*])
                   (hash-set s->c s c))) 

               (rec/env e uid->uid^^ fv->expr fv-set->clos^^)]
              ;; Otherwise we generate code to allocate closures and
              ;; caputure the free-variables of all !wk closures.
              [else 
               (for ([c wk-c*])
                 (hash-set! label->closure-status (Closure-code-label c) 'regular))
               ;; Map all wk closures to self in this closure 
               (define rec-fst
                 (let ()
                   (define uid->uid^ (map-uid*-to uid->uid wk-name* fst-self))
                   (define fv->expr^ (map-fv->closure-ref fst-self fv-ls))
                   (define fv-set->clos^ (hash-set fv-set->clos fv-set fst-int-sc))
                   (define fst-init* (fv*->expr* fv->expr fv-ls))
                   (Closure fst-name #f fst-mode fst-label fst-self fst-ctr?
                            fst-init* fst-fp-ls
                            (rec/env fst-e uid->uid^ fv->expr^ fv-set->clos^))))

               ;; (closures-borrow-closure wk-c*)
               ;; needs a uid*-mapped-to-self parameter
               (closures-borrow-closure wk-c* fv-ls fv-set
                                        (cons fst-name wk-name*)
                                        uid->uid
                                        fv-set->clos)
               
               ;; the rest is mostly regular closure conversion
               (define rec-rst : (Closure* CoC6-Expr CoC6-Expr)
                 (for/list ([c      rst-nwk*]
                            [fv-ls  rst-fv-ls*]
                            [fv-set rst-fv-set*]
                            [sc     rst-int-sc*])
                   (match-define
                     (Closure name #f 'regular label self ctr? _ p* e)
                     c)
                   (define fv-init* (fv*->expr* fv->expr fv-ls))
                   (Closure
                    name #f 'regular label self ctr? fv-init* p*
                    (rec/env e uid->uid^
                             (map-fv->closure-ref self fv-ls)
                             (filter-shared-closures fv-ls)))))
               
               (define fv-set->clos^
                 (hash-set fv-set->shared-closure fv-set fst-ext-sc))
               
               (define fv-set->clos^^
                 (for/fold ([s->c : Set->Clos fv-set->clos^])
                           ([s rst-fv-set*] [c rst-ext-sc*])
                   (hash-set s->c s c))) 
               (Let-Closures
                (cons rec-fst rec-rst)
                (rec/env e uid->uid^ fv->expr fv-set->clos^^))])])] 
        [(Var u)
         (define-values (u^ _) (find-representative uid->uid u))
         (cond
           [(hash-ref fv->expr u^ #f) => values]
           [else (Var u^)])]
        [(Closure-App (and l (Code-Label u)) s (app (cmap rec) a*))
         (match (hash-ref label->closure-status u #f)
           ['eliminated      (App-Code l a*)]
           ['free-variables  (App-Code l (cons (rec s) a*))]
           ['regular         (Closure-App l (rec s) a*)]
           [#f (error 'grift/convert-closures/optimize-closures
                      "Label, ~a, in closure application wasn't recorded as belonging to a closure"
                      (uid->string u))])]
        [other (form-map other rec)])))

  (: closures-borrow-tuple :
     (Closure* Uid CoC5-Expr) Uid* Uid* (HashTable Uid Uid) Set->Clos
     (Setof Uid)
     -> Void)
  (define (closures-borrow-tuple c*
                                 fv-ls
                                 uid*-mapped-to-self
                                 uid->uid
                                 fv-set->shared-closure
                                 fv-set) 
    (for ([c c*])
      (match-define (Closure _ _ _ label self _ _ p* e) c)
      (define sc (shared-tuple self fv-ls))
      ;; save the fv-set and sc pair so that subsequent closures
      ;; can filter by free variables.
      (hash-set! uid->fv-set.clos self (cons fv-set sc))
      (define rec-e
        (rec/env e
                 (map-uid*-to uid->uid uid*-mapped-to-self self)
                 (map-fv->tuple-ref self fv-ls)
                 (hash-set fv-set->shared-closure fv-set sc)))
      (set!
       static-code
       `([,label . ,(Code `(,self . ,p*) rec-e)] . ,static-code))))
  
  (: closures-borrow-closure :
     (Closure* Uid CoC5-Expr) Uid* (Setof Uid) Uid*
     (HashTable Uid Uid) Set->Clos
     -> Void)
  (define (closures-borrow-closure c* fv-ls fv-set
                                   uid*-mapped-to-self
                                   uid->uid
                                   fv-set->clos)  
    (for ([c c*])
      (match-define (Closure name #t 'regular label self _ _ p* e) c)    
      (define sc (shared-closure self fv-ls))
      (hash-set! uid->fv-set.clos self (cons fv-set sc))
      (define rec-e
        (rec/env
         e
         (map-uid*-to uid->uid uid*-mapped-to-self self)
         (map-fv->closure-ref self fv-ls)
         (hash-set fv-set->clos fv-set sc)))
      (set! static-closures
            (cons (Closure name #t 'code-only label self
                           #f '() p* rec-e)
                  static-closures))))
  
    (for ([c c*])
      (match c
        [(Closure name #f 'code-only label self c? f* p* e)
         ;; There isn't any point of participating in the sharing
         ;; here, because at this point only the apply casted closure
         ;; code hits this line, and it doesn't allocate closures.         
         (define rec-c
           (Closure
            ;; code-only suppresses closure creation so empty inits
            ;; is fine here. 
            name #f 'code-only label self c? '() p*
            (rec/env e (hasheq) (map-fv->closure-ref self f*) (hash))))
         (set! static-closures (cons rec-c static-closures))]))      

    (: rec! : CoC5-Expr -> CoC6-Expr)
    (define (rec! e) (rec/env e (hasheq) (hasheq) (hash)))
    
    (define rec-const-bnd* : (Bnd* CoC6-Expr) (map-bnd* rec! const-bnd*))
    
    (define rec-e : CoC6-Expr (rec! e))

    (values
     (sort static-code bnd-code<?)
     (sort static-closures closure<?)
     rec-const-bnd*
     rec-e))

;; Helpers that belong here

;; create a casted fn application call site
(: cast-apply-cast (Uid Uid (Var Uid) (Listof CoC5-Expr) (Var Uid) -> CoC5-Expr))
(define (cast-apply-cast cast-uid compose-uid fun arg* crcn)
  (define suspend-monotonic-heap-casts? do-not-suspend-monotonic-heap-casts)
  (define (let-bind bnd* body)
    (if (null? bnd*) body (Let bnd* body)))
  (define cast-cl (Code-Label cast-uid))
  (define compose-cl (Code-Label compose-uid))
  (define-values (v* b*)
    (for/lists ([v* : (Listof (Var Uid))]
                [b* : (Bnd* CoC5-Expr)])
               ([arg (if (enable-crcps?) (cdr arg*) arg*)]
                [i (in-naturals)])
      (define u (next-uid! (format "fn-cast-arg~a" i)))
      (define arg-crcn (Fn-Coercion-Arg crcn (Quote i)))
      (define args (list arg arg-crcn suspend-monotonic-heap-casts?))
      (values (Var u) (cons u (App-Code cast-cl args)))))
  (cond
    [(enable-crcps?)
     (define passed-crcn (car arg*))
     (define composed-crcn
       (App-Code compose-cl (list (Fn-Coercion-Return crcn) passed-crcn ZERO-EXPR ZERO-EXPR)))
     (define clos-app (Closure-App (Closure-Code fun) fun (cons composed-crcn v*)))
     (let-bind b* clos-app)]
    [else ;; without crcps
     (define clos-app (Closure-App (Closure-Code fun) fun v*))
     (define args
       (list clos-app (Fn-Coercion-Return crcn) suspend-monotonic-heap-casts?))
     (let-bind b* (App-Code cast-cl args))]))

(: bnd-code<? : (Pairof Uid Any) (Pairof Uid Any) -> Boolean)
(define (bnd-code<? x y) (uid<? (car x) (car y)))

(: closure<? : (Closure Any Any) (Closure Any Any) -> Boolean)
(define (closure<? x y) (uid<? (Closure-name x) (Closure-name y)))


(: map-fv->arbitrary-ref :
   (CoC6-Expr Integer -> CoC6-Expr) -> (Uid Uid* -> (HashTable Uid CoC6-Expr)))
(define ((map-fv->arbitrary-ref construct-ref) self fv-ls)
  (define selfv (Var self))
  (for/fold ([f->e : (HashTable Uid CoC6-Expr) (hasheq)])
            ([fv fv-ls] [i (in-naturals)])
    (hash-set f->e fv (construct-ref selfv i))))

(: map-fv->closure-ref : Uid Uid* -> (HashTable Uid CoC6-Expr))
(define map-fv->closure-ref (map-fv->arbitrary-ref Closure-Ref))

(: tuple-ref : CoC6-Expr Integer -> CoC6-Expr)
(define (tuple-ref t i) (Tuple-proj t (Quote i)))

(: map-fv->tuple-ref : Uid Uid* -> (HashTable Uid CoC6-Expr))
(define map-fv->tuple-ref (map-fv->arbitrary-ref tuple-ref))

;; I really should use a functional union find here
(: map-uid*-to : (HashTable Uid Uid) Uid* Uid -> (HashTable Uid Uid))
(define (map-uid*-to uid->uid from* to)
  (for/fold ([u->u : (HashTable Uid Uid) uid->uid])
            ([from from*])
    (hash-set u->u from to)))

(: find-representative :
   (HashTable Uid Uid) Uid -> (Values Uid (HashTable Uid Uid)))
(define (find-representative uid->uid x)
  (let loop ([x x] [y? (hash-ref uid->uid x #f)])
    (cond
      [y?
       (define-values (rep u->u) (loop y? (hash-ref uid->uid y? #f)))
       (values rep (hash-set u->u x rep))]
      [else (values x uid->uid)])))

(: resolve-free-variables :
   (->* ((Uid -> Boolean) (HashTable Uid Uid) Uid*) ((Setof Uid))
        (Values (Setof Uid) (HashTable Uid Uid))))
(define (resolve-free-variables eliminated? uid->uid fv*
                                [fv-set : (Setof Uid) (seteq)])
  (for*/fold ([fv-set : (Setof Uid) fv-set]
              [u->u : (HashTable Uid Uid) uid->uid])
             ([fv fv*])
    (define-values (rep u->u^) (find-representative u->u fv))
    (values
     (if (eliminated? rep)
         fv-set
         (set-add fv-set rep))
     u->u^)))

(: fv*->expr* : (HashTable Uid CoC6-Expr) Uid* -> (Listof CoC6-Expr))
(define (fv*->expr* fv->expr fv*)
  (for/list ([fv fv*])
    (or (hash-ref fv->expr fv #f) (Var fv))))

(module+ test
  ;; TODO add `refresh!` and the Uids to a test library file
  ;; Reset the state of the unique state counter to be 0
  (define (refresh!)
    (current-unique-counter (make-unique-counter 0)))

  ;; Make it slightly easier to define a uid and Var
  (define-syntax (define-u/v stx)
    (syntax-case stx ()
      [(_ u (c v) s n)
       #'(begin
           (define u (Uid s n))
           (define v (c u)))]))

  ;; Make it way easier to define a bunch of similar uids
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

  ;; Define a bunch of Uids for testing purposes
  (define-u/v* u (Var v) "" 10)
  (define-u/v* p (Var pv) "arg" 10)
  (define-u/v* code (Code-Label label) "_code" 10)
  (define-u/v* self (Var selfv) "_closure_self" 10)

  (define-for-syntax (syntax->srcloc stx)
    (make-srcloc
     (syntax-source stx)
     (syntax-line stx)
     (syntax-column stx)
     (syntax-position stx)
     (syntax-span stx)))
  
  (define-syntax (define/test-equal? stx)
    (syntax-parse stx 
      [(define/test-equal? defined-id:id description:str test:expr expect:expr)
       (with-syntax ([dscn (string-append (srcloc->string (syntax->srcloc stx))
                                          ":\n    "
                                          (syntax->datum #'description))]) 
         #`(begin
             (define defined-id expect)
             #,(quasisyntax/loc stx
                 (test-not-exn
                  dscn
                  (lambda ()
                    (let ([tmp test])
                      #,(quasisyntax/loc stx
                          (test-equal? dscn tmp defined-id))))))))]
      [(define/test-equal? def-id:id test:expr expect:expr)
       (with-syntax ([dsc (format "~a" (syntax->datum #'id))]) 
         (syntax/loc stx
           (define/test-equal? def-id dsc test expect)))]))


  (define cast (Uid "apply_coercion" 0))
  (define a0 (Uid "fn-cast-arg0" 0))
  (refresh!)
  ;; Test for cast apply cast
  (test-equal?
   "cast-apply-cast 1"
   (cast-apply-cast cast v0 (list v1) v2)
   (Let (list
         (cons
          a0
          (app-code$
           (Code-Label cast)
           v1
           (Fn-Coercion-Arg v2 (Quote 0)) do-not-suspend-monotonic-heap-casts)))
     (app-code$
      (Code-Label cast) 
      (Closure-App (Closure-Code v0) v0 (list (Var a0)))
      (Fn-Coercion-Return v2) do-not-suspend-monotonic-heap-casts)))
  
  
  ;; TODO Write tests for most of the features of optimize closures
  
  ;; This need to be something that won't propagate for the tests to
  ;; be correct
  (define non-propagant (op$ + (Quote 1) (Quote 1)))

  (define-syntax-rule (values->list p)
    (call-with-values (lambda () p) list))
  
  ;; test constant propagation
  (refresh!)
  (define/test-equal? cc-result1
    "convert-closures - constant propagation"
    (values->list
     (convert-closures
      '()
      (Let (list (cons u0 (Quote 1))) v0)))
    (list '() '()  (Quote 1)))
  (refresh!)
  (test-equal?
   "optimize-closures - constant"
   (values->list (apply optimize-closures cc-result1))
   (list '() '() '() (Quote 1)))

  ;; copy propagation
  (define/test-equal? cc-result2
    "convert closures - copy propagation"
    (values->list
     (convert-closures
      '()
      (Let (list (cons u0 non-propagant))
           (Let (list (cons u1 v0))
                v1))))
    (list '() '() (Let (list (cons u0 non-propagant)) v0)))
  (test-equal?
   "optimize closures - copy propagation"
   (values->list (apply optimize-closures cc-result2))
   (list '() '() '() (Let (list (cons u0 non-propagant)) v0)))
  
  ;; TODO test code forms
  
  ;; This is a known problem with the implementation.  Fixing it
  ;; requires too much code duplication for my taste.  The ideal
  ;; implementation would have a general purpose optimizer run before
  ;; this to propagate constantants and whatnot. In such a setup this
  ;; wouldn't be a problem.
  ;; TODO Figure out what this know problem is and document it.
  (refresh!)
  (define c2 (Uid "closure" 2))
  (define/test-equal? cc-result-known-problem
    "convert closures - known problem with implementation"
    (values->list
     (convert-closures
      '()
      (Letrec (list (cons u0 (Lambda '() (Castable #f (Quote 1)))))
              (App-Fn (Let (list (cons u1 v0)) v1) '()))))
    (list
     '()
     '()
     (Let-Closures
      (list
       (Closure u0 #f 'regular code0 self1 #f '() '() (Quote 1)))
      (Let (list (cons c2 v0))
           (Closure-App (Closure-Code (Var c2)) (Var c2) '())))))

  (test-equal?
   "optimize closures - known problem with copy propogation"
   (values->list (apply optimize-closures cc-result-known-problem))
   (list
    '()
    (list (Closure u0 #f 'regular code0 self1 #f '() '() (Quote 1)))
    '()
    (Let (list (cons c2 v0))
         (Closure-App (Closure-Code (Var c2)) (Var c2) '()))))

  
  ;; correct free variable discovery for single closure
  (refresh!)
  (define/test-equal? cc-result3
    "convert closures - single closure"
    (values->list
     (convert-closures
      '()
      (Let (list (cons u0 non-propagant))
           (Letrec (list (cons u1 (Lambda '() (Castable #f v0))))
                   v1))))
    (list
     '()
     '()
     (Let (list (cons u0 non-propagant))
          (Let-Closures
           (list (Closure u1 #f 'regular code0 self1 #f (list u0) '() v0))
           v1))))
  (test-equal?
   "optimize closures - single closure"
   (values->list (apply optimize-closures cc-result3))
   (list
    '()
    '()
    '()
    (Let (list (cons u0 non-propagant))
         (Let-Closures
          ;; ! wk w/ 1 fv -> arbitrary flat closure
          (list (Closure u1 #f 'regular code0 self1 #f (list v0) '()
                         (Closure-Ref selfv1 0)))
          v1))))
  
  ;; correct free variable discovery for nested closure 1
  (refresh!)
  (define/test-equal? cc-result4
    "convert closures - nested closure 1"
    (values->list
     (convert-closures
      '()
      (Let (list (cons u0 non-propagant))
           (Letrec
            (list 
             (cons
              u1
              (Lambda (list p1)
                      (Castable
                       #f
                       (Letrec
                        (list
                         (cons
                          u2
                          (Lambda '()
                                  (Castable
                                   #f
                                   (Create-tuple (list v0 (Tuple-proj pv1 (Quote 2))))))))
                        v2)))))
            v1))))
    (list
     '()
     '()
     (Let (list (cons u0 non-propagant))
          (Let-Closures
           (list
            (Closure
             u1 #f 'regular code0 self1 #f (list u0) (list p1)
             (Let-Closures
              (list
               (Closure
                u2 #f 'regular code2 self3 #f (list u0 p1) '()
                (Create-tuple 
                 (list v0 (Tuple-proj pv1 (Quote 2))))))
              v2)))
           v1))))
  (test-equal?
   "optimize closures - nested closure 1"
   (values->list (apply optimize-closures cc-result4))
   (list
    '() '() '()
    (Let (list (cons u0 non-propagant))
         (Let-Closures
          (list
           (Closure
            u1 #f 'regular code0 self1 #f (list v0) (list p1)
            (Let-Closures
             (list
              (Closure
               u2 #f 'regular code2 self3 #f
               (list (Closure-Ref selfv1 0) pv1) '()
               (Create-tuple
                (list (Closure-Ref selfv3 0)
                      (Tuple-proj (Closure-Ref selfv3 1)
                                  (Quote 2))))))
             v2)))
          v1))))
  
  ;; correct free variable discovery for nested closure 2
  ;; TODO test 5
  (refresh!)
  (define/test-equal? cc-result5
    "convert closures - nested closures self captured"
    (values->list
     (convert-closures
      '()
      (Letrec
       (list
        (cons
         u0
         (Lambda '()
                 (Castable
                  #f
                  (Letrec (list (cons u1 (Lambda '() (Castable #f v0))))
                          v1)))))
       v0)))
    (list
     '() '() 
     (Let-Closures
      (list (Closure
             u0 #f 'regular code0 self1 #f '() '()
             (Let-Closures
              (list
               (Closure u1 #f 'regular code2 self3 #f (list self1) '()
                        (Var self1)))
              v1)))
      v0)))

  (test-equal?
   "optimize closures - nested closures self captured"
   (values->list (apply optimize-closures cc-result5))
   (list
    '()
    (list (Closure u0 #f 'regular code0 self1 #f '() '() v1)
          (Closure u1 #f 'regular code2 self3 #f '() '() v0))
    '()
    v0))
  
  ;; Mutually Recusive Closures
  (refresh!)
  (define/test-equal? cc-result6
    "convert closures - 2 mutually recursive"
    (values->list
     (convert-closures
      '()
      (Letrec (list (cons u0 (Lambda '() (Castable #f (App-Fn v1 '()))))
                    (cons u1 (Lambda '() (Castable #f (App-Fn v0 '())))))
              v1)))
    (list
     '() '()
     (Let-Closures
      (list
       (Closure u0 #t 'regular code0 self1 #f (list u1) '()
                (Closure-App label2 v1 '()))
       (Closure u1 #f 'regular code2 self3 #f (list u0) '()
                (Closure-App label0 v0 '())))
      v1)))
  (test-equal?
   "optimize closures - 2 mutually recursive"
   (values->list (apply optimize-closures cc-result6))
   (list
    (list
     (cons code0 (Code '() (Closure-App label2 v1 '()))))
    (list 
     (Closure u1 #f 'regular code2 self3 #f '() '()
              (App-Code label0 '())))
    '()
    v1))
  

  ;; Chaining of optimization leads to less free variables
  (refresh!)
  (define/test-equal? cc-result7
    "convert closures - optimization chaining"
    (values->list
     (convert-closures
      '()
      (Let (list (cons u0 (Quote 1)))
           (Letrec (list (cons u1 (Lambda (list u2) (Castable #f v0))))
                   (Let (list (cons u3 v1))
                        (Letrec
                         (list
                          (cons u4 (Lambda '() (Castable #f (App-Fn v3 (list v4))))))
                         v4))))))
    (list
     '() '()
     (Let-Closures
      (list (Closure u1 #t 'regular code0 self1 #f '() `(,u2) (Quote 1)))
      (Let-Closures
       (list (Closure u4 #f 'regular code2 self3 #f `(,u1) '()
                      (Closure-App label0 v1 (list selfv3))))
       v4))))
  (test-equal?
   "optimize closures - optimization chaining"
   (values->list (apply optimize-closures cc-result7))
   (list
    (list (cons code0 (Code `(,u2) (Quote 1))))
    (list (Closure u4 #f 'regular code2 self3 #f '() '()
                   (App-Code label0 (list v4))))
    '()
    v4))

  ;; Optimization works with proxied function applications where
  ;; possible
  (refresh!)
  (define/test-equal? cc-result8
    "convert closures - proxied function application"
    (values->list
     (convert-closures
      '()
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
       (App-Fn v0 (list v0)))))
    (list
     '() '()
     (Let-Closures
      (list
       (Closure
        u0 #f 'regular code0 self1 u1 '() (list p0)
        (If (op$ = selfv1 pv0)
            (Closure-App label0 selfv1 (list pv0))
            (Closure-App (Closure-Code pv0) pv0 (list selfv1)))))
      (Closure-App label0 v0 (list v0)))))
  (test-equal?
   "optimize closures - proxied function application"
   (values->list (apply optimize-closures cc-result8))
   (list
    '()
    (list
     (Closure
      u0 #f 'regular code0 self1 u1 '() (list p0)
      (If (op$ = v0 pv0)
          (Closure-App label0 v0 (list pv0))
          (Closure-App (Closure-Code pv0) pv0 (list v0)))))
    '()
    (Closure-App label0 v0 (list v0))))
  
  (refresh!)
  (define/test-equal? cc-result9
    "convert closures - more complex 1"
    (values->list
     (convert-closures
      '()
      (Letrec (list (cons u0 (Lambda '() (Castable #f (App-Fn v1 '()))))
                    (cons u1 (Lambda '() (Castable #f (App-Fn v0 '()))))
                    (cons u2 (Lambda '() (Castable #f (App-Fn v1 '()))))
                    (cons u3 (Lambda '() (Castable #f (App-Fn v2 '())))))
              v3)))
    (list
     '() '()
     (Let-Closures
      (list (Closure u0 #t 'regular code0 self1 #f (list u1) '()
                     (Closure-App label2 v1 '()))
            (Closure u1 #t 'regular code2 self3 #f (list u0) '()
                     (Closure-App label0 v0 '())))
      (Let-Closures
       (list (Closure u2 #t 'regular code4 self5 #f (list u1) '()
                      (Closure-App label2 v1 '())))
       (Let-Closures
        (list (Closure u3 #f 'regular code6 self7 #f (list u2) '()
                       (Closure-App label4 v2 '())))
        v3)))))

  (test-equal? 
   "optimize closures - more complex 1"
   (values->list (apply optimize-closures cc-result9))
   (list
    (list
     (cons code0 (Code '() (App-Code label2 '())))
     (cons code2 (Code '() (App-Code label0 '())))
     (cons code4 (Code '() (App-Code label2 '()))))
    (list
     (Closure u3 #f 'regular code6 self7 #f '() '()
              (App-Code label4 '())))
    '()
    v3))

  (refresh!)  
  (define/test-equal? cc-result-letrec-6
    "convert closures - tests/suite/static/letrec6.rkt"
    (values->list
     (convert-closures
      '()
      (Letrec (list
               (cons u0 (Lambda '()
                                (Castable
                                 #f
                                 (op$ + (Quote 1) (App-Fn v2 '())))))
               (cons u2 (Lambda '() (Castable #f (Quote 0)))))
              (App-Fn v0 '()))))
    (list
     '() '()
     (Let-Closures 
      (list
       (Closure u2 #t 'regular code2 self3 #f '() '() (Quote 0)))
      (Let-Closures
       (list
        (Closure u0 #t 'regular code0 self1 #f (list u2) '()
                 (op$ + (Quote 1) (Closure-App label2 v2 '()))))
       (Closure-App label0 v0 '())))))

  ;; This is the example from Optimizing Closures in O(n) time.
  #;
  (lambda (x)
    (letrec ([f (lambda (a) (a x))]
             [g (lambda () (f (h x)))]
             [h (lambda (z) (g))]
             [q (lambda (y) (+ (length y) 1))])
      (q (g))))

  (refresh!)  
  (define/test-equal? cc-result10
    "convert closure - paper complex example"
    (values->list
     (convert-closures
      '()
      (Letrec
       (list
        (cons
         u0
         (Lambda (list p1)
                 (Castable
                  #f
                  (Letrec
                   (list
                    ;; x -> pv1
                    ;; f -> v2 
                    [cons
                     u2
                     (Lambda (list p2) (Castable #f (App-Fn pv2 (list pv1))))]
                    ;; g -> v3
                    ;; h -> v4
                    [cons
                     u3
                     (Lambda '()
                             (Castable #f (App-Fn v2 (list (App-Fn v4 (list pv1))))))]
                    [cons
                     u4
                     (Lambda (list p3)
                             (Castable #f (App-Fn v3 '())))]
                    ;; q -> u5
                    [cons
                     u5
                     (Lambda (list p4)
                             (Castable
                              #f
                              (op$ + (Unguarded-Vect-length pv4) (Quote 1))))])
                   (App-Fn v5 (list (App-Fn v4 '()))))))))
       v0)))
    (list
     '()
     '()
     (Let-Closures
      (list (Closure
             u0 #f 'regular code0 self1 #f '() (list p1)
             (Let-Closures
              (list (Closure
                     u5 #t 'regular code8 self9 #f '() (list p4)
                     (op$ + (Unguarded-Vect-length pv4) (Quote 1))))
              (Let-Closures 
               (list (Closure
                      u2 #t 'regular code2 self3 #f (list p1) (list p2)
                      (Closure-App (Closure-Code pv2) pv2 (list pv1))))
               (Let-Closures
                (list 
                 (Closure
                  u3 #t 'regular code4 self5 #f (list p1 u2 u4) '()
                  (Closure-App label2 v2
                               (list (Closure-App label6 v4 (list pv1)))))
                 (Closure
                  u4 #t 'regular code6 self7 #f (list u3) (list p3)
                  (Closure-App label4  v3 '())))
                (Closure-App label8 v5 (list (Closure-App label6 v4 '()))))))))
      v0)))
  (test-equal?
   "optimize closures - paper complex example"
   (values->list (apply optimize-closures cc-result10))
   (list
    (list
     (cons
      code2
      (Code (list self3 p2)
            (Closure-App (Closure-Code pv2) pv2 (list selfv3))))
     (cons
      code4
      (Code (list self5)
            (App-Code label2 (list selfv5 (App-Code label6 (list selfv5 selfv5))))))
     (cons code6 (Code (list self7 p3) (App-Code label4 (list selfv7))))
     (cons code8 (Code (list p4) (op$ + (Unguarded-Vect-length pv4) (Quote 1))))) 
    (list
     (Closure u0 #f 'regular code0 self1 #f '() (list p1)
              (App-Code label8 (list (App-Code label6 (list pv1))))))
    '()
    v0))

  (refresh!)  
  (define/test-equal? cc-result-count-from
    "conver-closure - tests/suite/program/sieve.100.grift count-from"
    (values->list
     (convert-closures
      '()
      (Letrec (list
               (cons
                u1
                (Lambda '() 
                        (Castable
                         #f
                         (Letrec (list
                                  (cons
                                   u2
                                   (Lambda '() (Castable #f (App-Fn v1 '())))))
                                 v2)))))
              (No-Op))))
    (list
     '() '()
     (Let-Closures
      (list
       (Closure
        u1 #t 'regular code0 self1 #f '() '()
        (Let-Closures
         (list
          (Closure u2 #f 'regular code2 self3 #f (list self1) '()
                   (Closure-App label0 selfv1 '())))
         v2)))
      (No-Op))))

  (test-equal? 
   "optimize-closures - tests/suite/program/sieve.100.grift count-from"
   (values->list (apply optimize-closures cc-result-count-from))
   (list
    (list (cons code0 (Code '() v2)))
    (list (Closure u2 #f 'regular code2 self3 #f '() '()
                   (App-Code label0 '())))
    '()
    (No-Op)))


  (refresh!)  
  (define/test-equal? cc-result-bug
    "convert-closure - bug in capture"
    (values->list
     (convert-closures
      '()
      (Letrec (list
               (cons
                u1
                (Lambda '() 
                        (Castable
                         #f
                         (Letrec (list
                                  (cons
                                   u2
                                   (Lambda '() (Castable #f (App-Fn v1 '())))))
                                 v2)))))
              v1)))
    (list
     '() '()
     (Let-Closures
      (list
       (Closure
        u1 #f 'regular code0 self1 #f '() '()
        (Let-Closures
         (list
          (Closure u2 #f 'regular code2 self3 #f (list self1) '()
                   (Closure-App label0 selfv1 '())))
         v2)))
      v1)))
  
  (test-equal? 
   "optimize-closures - bug in capture"
   (values->list (apply optimize-closures cc-result-bug))
   (list
    '()
    (list
     (Closure u1 #f 'regular code0 self1 #f '() '() v2)
     (Closure u2 #f 'regular code2 self3 #f '() '()
              (Closure-App label0 v1 '())))
    '()
    v1))

  (test-equal? 
   "optimize-closures - bug fix in invariants 1"
   (values->list
    (apply
     optimize-closures
     (list
      '()
      '()
      (Let-Closures
       (list
        (Closure
         u1 #f 'closure-only code0 self1 #f '() '() (Quote 1)))
       v1))))
   (list
    '()
    (list
     (Closure u1 #f 'closure-only code0 self1 #f '() '() (Quote 1)))
    '()
    v1))
  
  (test-equal? 
   "optimize-closures - bug fix in invarients 2"
   (values->list
    (apply
     optimize-closures
     (list
      '()
      '()
      (Let (list (cons u0 non-propagant))
           (Let-Closures
            (list
             (Closure
              u1 #f 'closure-only code0 self1 #f (list u0) '() (Quote 0)))
            v1)))))
   (list
    '()
    '()
    '()
    (Let (list (cons u0 non-propagant))
         (Let-Closures
          (list
           (Closure
            u1 #f 'closure-only code0 self1 #f (list v0) '() (Quote 0)))
          v1))))


  ;; loop5 -> u0
  ;; 
  (define loop5 (Uid "loop5" 1))
  (define loop5-code (Uid "loop5_code" 0))
  (define loop5-self (Uid "loop5_closure_self" 1))
  (define loop4 (Uid "loop4" 2))
  (define fn-cast (Uid "fn-cast-0" 442))
  (define apply-coercion (Uid "apply-coercion" 158))
  (define app-crcn-cl (Code-Label apply-coercion))
  (define tmp (Uid "tmp" 465))
  (define annon (Uid "annon" 467))
  (define annon-code (Uid "annon_code" 2))
  (define annon-self (Uid "annon_closure_self" 3))
  (define init (Uid "letrec-init" 30))
  (define tmp-closure2 (Uid "closure" 4))
  (define tmp-closure3 (Uid "closure" 5))
  (define tmp-closure1 (Uid "closure" 6))
  
  (refresh!)
  (: fft-bug-result (List Null Null CoC5-Expr))
  (define/test-equal? fft-bug-result
    "convert-closure - fft-bug"
    (values->list
     (convert-closures
      '()
      (Let
       (list
        (cons init (Unguarded-Box (Quote #f)))
        (cons loop4 (Unguarded-Box (Quote 0))))
       (Letrec
        (list
         (cons
          loop5
          (Lambda
           '()
           (Castable
            fn-cast
            (App-Code
             app-crcn-cl
             (list
              (Begin
                (list
                 (App-Fn-or-Proxy apply-coercion (Var loop5) '()))
                (App-Fn-or-Proxy
                 apply-coercion
                 (Unguarded-Box-Ref (Var loop4)) '()))
              (Quote-Coercion (Static-Id (Uid "static_project_crcn" 464)))
              do-not-suspend-monotonic-heap-casts))))))
        (Let
         (list
          (cons
           tmp
           (App-Code
            (Code-Label apply-coercion)
            (list
             (Letrec
              (list
               (cons
                annon
                (Lambda '()
                        (Castable
                         fn-cast
                         (App-Fn-or-Proxy
                          apply-coercion
                          (If (Unguarded-Box-Ref (Var init))
                              (Var loop5)
                              (Blame (Quote "")))
                          '())))))
              (Var annon))
             (Quote-Coercion (Static-Id (Uid "static_fn_crcn" 463)))
             do-not-suspend-monotonic-heap-casts))))            
         (Begin
           (list
            (Unguarded-Box-Set! (Var loop4) (Var tmp))
            (Unguarded-Box-Set! (Var init) (Quote #t)))
           (App-Fn-or-Proxy apply-coercion
                            (Unguarded-Box-Ref (Var loop4))
                            '())))))))
    (list
     '()
     '()
     (Let
      (list
       (cons
        loop4
        (Unguarded-Box (Quote 0)))
       (cons
        init
        (Unguarded-Box (Quote #f))))
      (Let-Closures
       (list
        (Closure
         loop5 #f 'regular loop5-code loop5-self
         fn-cast (list loop4) '()
         (ann
          (App-Code
           app-crcn-cl
           (list
            (Begin
              (list
               (Closure-App (Code-Label loop5-code)
                            (Var loop5-self)
                            '()))
              (Let (list (cons tmp-closure1 (Unguarded-Box-Ref (Var loop4))))
                   (Closure-App (Closure-Code (Var tmp-closure1))
                                (Var tmp-closure1)
                                '())))
            (Quote-Coercion (Static-Id (Uid "static_project_crcn" 464)))
            do-not-suspend-monotonic-heap-casts))
          CoC5-Expr)))
       (ann
        (Let
         (list
          (cons
           tmp
           (App-Code
            (Code-Label apply-coercion)
            (list
             (Let-Closures
              (list
               (Closure
                annon #f 'regular annon-code annon-self fn-cast
                (list loop5 init) '()
                (ann
                 (Let
                  (list
                   (cons
                    tmp-closure2
                    (If (Unguarded-Box-Ref (Var init))
                        (Var loop5)
                        (Blame (Quote "")))))
                  (Closure-App
                   (Closure-Code (Var tmp-closure2))
                   (Var tmp-closure2) '()))
                 CoC5-Expr)))
              (Var annon))
             (Quote-Coercion (Static-Id (Uid "static_fn_crcn" 463)))
             do-not-suspend-monotonic-heap-casts))))
         (ann
          (Begin
            (list
             (Unguarded-Box-Set! (Var loop4) (Var tmp))
             (Unguarded-Box-Set! (Var init) (Quote #t)))
            (Let (list (cons tmp-closure3 (Unguarded-Box-Ref (Var loop4))))
                 (Closure-App (Closure-Code (Var tmp-closure3))
                              (Var tmp-closure3)
                              '())))
          CoC5-Expr))
        CoC5-Expr)))))
  
  (test-equal? 
   "optimize-closures - bug in fft"
   (values->list (apply optimize-closures fft-bug-result))
   (list
    '()
    '()
    '()
    (Let (list (cons loop4 (Unguarded-Box (Quote 0)))
               (cons init (Unguarded-Box (Quote #f))))
         (Let-Closures
          (list
           (Closure
            loop5 #f 'regular loop5-code loop5-self fn-cast
            (list (Var loop4)) '()
            (App-Code
             (Code-Label apply-coercion)
             (list
              (Begin
                (list
                 (Closure-App (Code-Label loop5-code) (Var loop5-self) '()))
                (Let
                 (list
                  (cons
                   tmp-closure1 
                   (Unguarded-Box-Ref (Closure-Ref (Var loop5-self) 0))))
                 (Closure-App
                  (Closure-Code (Var tmp-closure1)) (Var tmp-closure1) '())))
              (Quote-Coercion
               (Static-Id (Uid "static_project_crcn" 464)))
              do-not-suspend-monotonic-heap-casts))))
          (Let
           (list
            (cons
             tmp
             (App-Code
              (Code-Label apply-coercion)
              (list
               (Let-Closures
                (list
                 (Closure
                  annon #f 'regular annon-code annon-self fn-cast
                  (list (Var loop5) (Var init)) '()
                  (Let (list
                        (cons
                         tmp-closure2 
                         ;; Why are these not closure forms
                         (If (Unguarded-Box-Ref (Closure-Ref (Var annon-self) 1))
                             (Closure-Ref (Var annon-self) 0)
                             (Blame (Quote "")))))
                       (Closure-App (Closure-Code (Var tmp-closure2))
                                    (Var tmp-closure2)
                                    '()))))
                (Var annon))
               (Quote-Coercion (Static-Id (Uid "static_fn_crcn" 463)))
               do-not-suspend-monotonic-heap-casts)))) 
           (Begin
             (list
              (Unguarded-Box-Set! (Var loop4) (Var tmp))
              (Unguarded-Box-Set! (Var init) (Quote #t)))
             (Let (list (cons tmp-closure3 (Unguarded-Box-Ref (Var loop4))))
                  (Closure-App (Closure-Code (Var tmp-closure3))
                               (Var tmp-closure3)
                               '()))))))))

  )

(: optimize? : Symbol -> Boolean)
(define (optimize? x)
  (set-member? (closure-optimizations) x))


(: map-bnd* (All (A B) (A -> B) (Bnd* A) -> (Bnd* B)))
(define (map-bnd* f b*)
  (for/list ([b b*]) (cons (car b) (f (cdr b)))))

; LocalWords:  Propagant uid fn uids arg letrec src Letrecs rator env
; LocalWords:  LocalWords rkt scc
