;; TODO test
;; (let ([x non-propagant])
;;  (letrec ([f (lambda (y) (lambda () (f x)))])
;;   (f x))
;;
;; (let-code ([f (code (self y)
;;                (let-closure ([a (closure (self) ()
;;                                  (f self self))])
;;                 a))))
;;   (f x x))


;; Just 1 no need to think of mutually recursive variables
#;[(list (Closure name wk? 'regular label self c? f* p* b))
   (define fv-set
     ;; remove any eliminated closures
     ;; and merge any closures that where renamed
     (for/fold ([fv-set : (Setof Uid) (seteq)])
               ([fv f*] #:when (not (eliminated? fv)))
       (set-add fv-set (hash-ref bound-var->uid fv fv))))
   (define fv-ls (set->list fv-set))
   (match* (wk? (length fv-ls))
     ;; !wk & 0 fv -> static allocation
     ;; - add to eliminated free vars set
     [(#f 0)
      (mset-add! closures-eliminated name)
      (set! static-closures
            (cons
             (Closure name wk? 'regular label self c? fv-ls p* (rec b))
             static-closures))
      (rec e)]
     ;;  !wk & 1 <= |fv-ls| -> regular allocation
     ;;  - register closure to be shared with well known closures
     [(#f n)
      ;; We make a new reusable closure map that only
      ;; contains thing in scope in the closure.
      ;; To share this closure another closure makes
      ;; themselve 'code-only, pass this closure at
      ;; all invocations in the body, and uses this
      ;; free variable list in there closure declaration.
      (define self-sc (shared-closure (Var self) fv-ls))
      (define self-fv-set.clos (cons fv-set self-sc))
      (hash-set! uid->fv-set.clos self self-fv-set.clos)
      (define self-fv-set->shared-closure
        (make-remapped-shared-closures self fv-set self-sc fv-ls))
      (define body-sc (shared-closure (Var name) fv-ls))
      (define body-fv-set.clos (cons fv-set body-sc))
      (hash-set! uid->fv-set.clos name body-fv-set.clos)
      (hash-set! fv-set->shared-closure fv-set body-sc)
      (Let-Closures
       (list (Closure name wk? 'regular label self c? fv-ls p*
                      (rec/env b self-fv-set->shared-closure)))
       (rec e))]
     ;; wk & 0 fv -> convert the function to code
     ;; - add the closure to eliminated free vars
     [(#t 0)
      (mset-add! closures-eliminated name)
      (hash-set! label->closure-status label 'eliminated)
      ;; We know the caster isn't needed because the closure
      ;; never got passed as an argument (or else it wouldn't
      ;; be well-known).  We pobably shouldn't recur with the
      ;; current environments, but because there are no free
      ;; variables nothing in the maps could be looked up.
      (set! static-code
            (cons (cons label (Code p* (rec b))) static-code))
      (rec e)]
     ;; wk & 1 fv -> covert the function to code
     [(#t 1)
      (define fv (car fv-ls))
      ;; Closure-Applications get compiled to regular function
      ;; calls with the first argument as the free variable.
      (hash-set! label->closure-status label 'free-variables)

      ;; Replace references to the free variable with
      ;; references to the self argument.
      (hash-set! free-var->expr (Closure-Ref self fv) (Var self)) 

      ;; Replace references to the closure with references
      ;; to the free variable.
      ;; BUG: If fv isn't bound in this scope then the var replaced
      ;; will be unbound.
      ;; TODO: find a test case that exposes this as a bug
      ;; FIXME: Don't expose Closure-Ref until after this pass.
      (hash-set! bound-var->uid name fv)
      (set! static-code
            (cons (cons label (Code (cons self p*) (rec b)))
                  static-code))
      (rec e)]
     [(#t n)
      (define fv-set (list->set fv-ls))
      (match (hash-ref fv-set->shared-closure fv-set #f)
        ;; wk & 1 < fvs & ¬∃c. fv(c) = fv-set
        ;; create a tuple to pass as a closure
        ;; - free-var references become tuple-refs
        ;; - allow this tuple to share with subsequent wk closures
        [#f
         ;; map free variable to possition in tuple
         (define fv->i : (HashTable Uid Nat)
           (for/hasheq ([fv fv-ls] [i (in-naturals)])
             (hash-set! free-var->expr
                        (Closure-Ref self fv)
                        (Tuple-proj self i))
             (hash-set! fv->i fv i)))
         
         (define self-sc (shared-tuple (Var self) fv->i))
         (define self-fv-set.clos (cons fv-set self-sc))
         (hash-set! uid->fv-set.clos self self-fv-set.clos)
         (define self-fv-set->shared-closure
           (make-remapped-shared-closures self fv-set self-sc fv-ls))

         (set! static-code
               (cons
                (cons
                 label
                 (Code (cons self p*)
                   (rec/env b closure-fv-set->shared-closure)))
                static-code))
         
         (define body-sc (shared-tuple (Var name) fv->i))
         (define body-fv-set.clos (cons fv-set body-sc))
         (hash-set! uid->fv-set.clos name body-fv-set.clos)
         (hash-set! fv-set->shared-closure fv-set body-sc)
         (Let ([name (Create-tuple fv-ls)]) (rec e))]
        ;; wk & 1 < fvs & ∃c. fv(c) = fv-set & wk(c)
        ;; There is already a tuple rewrite references to name as references
        ;;   to that tuple.
        ;; - free-var references become tuple-refs
        ;; - allow this tuple to share with subsequent wk closures
        [(shared-tuple v u->i)
         (for ([fv fv-ls])
           (hash-set! free-var->expr
                      (Closure-Ref self fv)
                      (Tuple-proj self (hash-ref u->i fv))))
         (set! static-code
               (cons (cons label (Code (cons self p*) (rec b)))
                     static-code)) 
         (Let (list (cons name v)) (rec e))]
        [(shared-closure v fv-ls)
         (error 'todo "barrow closure")])])]



                  
                  ;; (for ([c wk-c*])
                  ;;   (match-define (Closure _ _ _ label self _ _ p* e) c)
                  ;;   ;; Rewrite all occurences of fv as the self parameter.
                  ;;   ;; Note this is done with uid->uid so that if the
                  ;;   ;; fv and self are captured then only a single variable
                  ;;   ;; is captured.

                  ;;   (define uid->uid^ 
                  ;;     (for/fold ([u->u : (HashTable Uid Uid) uid->uid])
                  ;;               ([name name*])
                  ;;       (hash-set u->u name self)))

                  ;;   (define selfv (Var self))
                    
                  ;;   (define fv->expr
                  ;;     (for/fold ([f->e : (HashTable Uid CoC6-Expr) (hasheq)])
                  ;;               ([fv fv-ls]
                  ;;                [i (in-naturals)])
                  ;;       (hash-set f->e fv (Tuple-ref selfv i))))

                  ;;   (define sc (shared-tuple self fv-ls))
                    
                  ;;   (define rec-e
                  ;;     (rec/env e uid->uid^ fv->expr
                  ;;              (hash-set fv-set->clos fv-set sc)))

                  ;;   (set! static-code `([,label ,(Code `(,self . ,p*) rec-e)]
                  ;;                       . ,static-code)))

#;
(for ([c wk-c*])
  (match-define (Closure name _ _ label self _ _ p* e) c)

  (define selfv (Var self))
  
  (define fv->expr^ 
    (for/fold ([f->e : (HashTable Uid CoC3-Expr) (hasheq)])
              ([fv fv-ls] [i (in-naturals)])
      (hash-set u->u fv (Closure-Ref selfv i))))
  
  (define sc (shared-tuple self fv-ls))

  (define fv-set->clos^
    (hash-set fv-set->clos fv-set sc))

  (hash-set! uid->fv-set.clos self (cons fv-set sc))
  
  (define rec-e
    (rec/env e uid->uid^ fv->expr
             (hash-set fv-set->clos fv-set sc)))

  (set! static-closures
        (cons (Closure name #t 'code-only label self
                       #f fv-ls p* rec-e)
              static-closures)))

;; Analyzes closures to determine if they are well-know
;; and seperates closures into sets of mutually recursive
;; definitions.
#;(: analyze-closures : CoC6-Expr -> CoC6-Expr)
#;
(define (analyze-closures e)
  (define well-known-closures (mset))
  (let rec/cc ([e e]
               [current-closure : (Option Uid) #f]
               [current-closure-self : (Option Uid) #f])
    (let rec ([e e])
      (match e
        [(Let-Closures c* b)
         (define this-binding (mset))
         ;; Initially assume a closure is well-known. If it ever
         ;; reaches the `(Var u)` case remove it from this set, because
         ;; it must be used as an argument somewhere.
         ;; The `(Closure-App (Code-Label _) self e*)` case prevents
         ;; Variables that are used as the self parameter from matching
         ;; this case.
         (for ([c c*])
           (define n (Closure-name c))
           (mset-add! this-binding n)
           (mset-add! well-known-closures n))

         ;; Recursively process the body before deciding well-knowness
         ;; of closures in this binding.
         (define b^ (rec b))

         ;; Recur into each closure but keep all the meta data
         ;; associated with the result.  All closures must be
         ;; recursively processes before we can decide the well-knowness
         ;; of anything in this binding
         (define bxc* : (Listof (Pairof CoC6-Expr (Closure CoC6-Expr)))
           (for/list ([c c*])
             (match-define (Closure name _ _ _ self _ _ _ code) c)
             (cons (rec/cc code name self) c)))

         ;; A graph representation of the closures to their free
         ;; variables of that are bound in this binding.
         ;; This graph is populated in the next step. 
         (define g : (Graph Uid) (make-graph '()))
         
         (define u->c-hash
           (for/hasheq : (HashTable Uid (Closure CoC6-Expr)) ([bxc bxc*])
             (match-define (cons b (Closure name _ rep label self c? f* p* _)) bxc)
             ;; Only after we have done every recursive call can we know
             ;; if the closure is well-kown.
             (define wk? (set-member? well-known-closures name))
             (define c (Closure name wk? rep label self c? f* p* b))
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
           ;; TODO to make sharing closures easier sort unrelated
           ;;      components by well-knowness. We want to allocate
           ;;      not well-known variables first so that any subsequent
           ;;      closures with the same free-variable set can reuse
           ;;      the closure. 
           ;;      To make algorithm more deterministic
           ;;      sort by uid<? on closure name.
           #;
           (let loop ([rev-scc* (scc g)]
                      [var*-unrel-scc* (mset)]
                      [wk-unrel-scc* '()]
                      [nwk-unrel-scc* '()]
                      [scc* '()])
             (cond
               [(null? rev-scc*) (append nwk-unrel-scc* wk-unrel-scc* scc*)]
               [else
                ()])
             (define))
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
         ;; `u`'s type prevents it from being `#f` so this `eq?`
         ;; simultaneously checks to make sure current-closure-self
         ;; isn't false.
         (when (eq? u current-closure-self) 
           (unless current-closure
             (error
              'analyze-closure
              (string-append "current-closure must be a Uid "
                             "when current-closure-self is a Uid")))
           (set-remove! well-known-closures current-closure))
         e]
        [otherwise (form-map e rec)])))) 
