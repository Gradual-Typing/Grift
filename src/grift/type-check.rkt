#lang typed/racket/base/no-check
#|------------------------------------------------------------------------------+
|Pass: compiler/grift/typecheck                                                  |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Description: This is a first attempt at typechecking for the GTLC core         |
|language.  It is currently limited to just the grammar noted below but with the|
|addition of a unification algorithm is could be generalize to mutually         |
|recursive functions. There is a major way in which this algorithm is different |
|from that of the STLC. The difference is that instead of checking for type     |
|equality the algorithm is checking for type consitency.                        |
|                                                                               |
|splits each elimination form of monotonic references to one that is annotated  |
|and the one that is not, refer to the paper for the rules                      |
|                                                                               |
|Essence of the consistency relationship:                                       |
|Two type are consistent any of the following relationships are true.           |
|   1: One of them is dyn.                                                      |
|   2: They are structurally equal.                                             |
|   3: They both share the same super-structure                                 |
|      and their sub-structures are consistent.                                 |
|                                                                               |
+-------------------------------------------------------------------------------+
TODO
Provide a comment for how each pass is layed out in code.
Provide comments about where to find definitions of types and data
+------------------------------------------------------------------------------|#

(require racket/function
         racket/match
         racket/set
         "../configuration.rkt"
         "../errors.rkt"
         "../language/forms.rkt"
         "../language/pprint.rkt"
         "../language/grift0.rkt"
         "../language/grift1.rkt"
         "../language/primitives.rkt"
         "../logging.rkt")

(module+ test
  (require rackunit)
  (define (fresh-uids!)
    (current-unique-counter (make-unique-counter 0)))
  (define s (srcloc #f #f #f #f #f)))


(require/typed racket/base
  [srcloc->string (srcloc -> String)])

(provide
 type-check
 unfold-possible-mu
 (all-from-out
  "../language/grift0.rkt"
  "../language/grift1.rkt"))

(: type-check (Grift0-Lang -> Grift1-Lang))
(define (type-check prgm)
  (define who 'type-check)
  (match-define (Prog (list n c) top-level*) prgm)

  (define uc (make-unique-counter c))
  
  (define g1-top-level*
    (parameterize ([current-unique-counter uc])
      (tc-top* top-level* (hash))))
  (debug off
         (Prog (list n (unique-counter-next! uc) INT-TYPE) g1-top-level*)))

(: tc-top* : G0-Top* Env -> G1-Top*)
(define (tc-top* t* env)
  (match t*
    [(cons (Ann2 (Define #f i t? e) s) t*-rest)
     (define-values (new-e e-type) (tc-expr e env))
     (debug t? e-type)
     (define vt? (and t? (validate-type s t?)))
     (define i-type
       (cond
         [(not vt?) e-type]
         [(consistent? vt? e-type) vt?]
         [else (error 'static-type-error "~a: inconsistent typed ~a and ~a"
                      (srcloc->string s) vt? e-type)]))
     (cons (Define #f i i-type new-e)
           (tc-top* t*-rest (hash-set env i i-type)))]
    [(and t* (cons (Ann2 (Define #t _ _ _) _) _))
     ;; Type check all consecutive recursive defines in a single recursive
     ;; environment.
     ;; This makes a two pass sweep of consequtive recursive bindings
     ;; The first pass "infers" and accumulates the recursive environment
     ;; The second pass does the actual type-checking
     (define-type G0-Rec-Def  (Ann2 (Define #t Uid (Option Grift-Type) G0-Ann-Expr) srcloc))
     (define-type G0-IRec-Def  (Ann2 (Define #t Uid Grift-Type G0-Ann-Expr) srcloc))
     (define-type G0-IRec-Def* (Listof G0-IRec-Def))
     (define-type G1-IRec-Def  (Define #t Uid Grift-Type G1-Ann-Expr))

     (: tc-rec-define : Env -> (G0-IRec-Def -> G1-IRec-Def))
     (define ((tc-rec-define env) d)
       ;; The valid-id-type variable has already been validated in
       ;; tc-rec-define*
       (match-define (Ann2 (Define _ id valid-id-type expr) src) d)
       (define-values (new-expr expr-type) (tc-expr expr env))
       (debug valid-id-type expr-type)
       (unless (consistent? valid-id-type expr-type)
         (error 'static-type-error "~a: inconsistent typed ~a and ~a"
                (srcloc->string src) valid-id-type expr-type))
       (Define #t id valid-id-type new-expr))

     (: tc-rec-define* : G0-Top* Env G0-IRec-Def* -> G1-Top*)
     (define (tc-rec-define* t* env i*)
       (match t*
         ;; Scan through then next consective recusive bindings in t*
         ;; infer their types and accumulate them in reverse.
         [(cons (Ann2 (Define #t id t? e) s) t*-rest)
          (define-values (ty new-e) (synthesize-recursive-binding-type t? e))
          (define valid-ty (validate-type s ty))
          (define i (Ann2 (Define #t id valid-ty new-e) s))
          (tc-rec-define* t*-rest (hash-set env id valid-ty) (cons i i*))]
         ;; Type-check each scanned top level form
         ;; reversing the scanned forms again and typechecking the
         ;; rest with the new environment. 
         [t*-rest 
          (define c* (map (tc-rec-define env) i*)) 
          (define c*-rest (tc-top* t*-rest env))
          ;; reverse c* and append c*-rest
          (foldl (inst cons G1-Top G1-Top*) c*-rest c*)]))
     (tc-rec-define* t* env '())]
    [(cons (Ann2 (Observe expr #f) src) t*-rest)
     (define-values (new-expr type) (tc-expr expr env))
     (cons (Observe new-expr type) (tc-top* t*-rest env))]
    [(list) '()]
    [other (error 'tc-top* "unhandled case: ~a" other)]))

(define-type Env (HashTable Uid Grift-Type))

(: env-extend* : Env Uid* Grift-Type* -> Env)
(define (env-extend* e x* v*) 
  (for/fold ([e e]) ([x (in-list x*)] [v (in-list v*)])
    (hash-set e x v)))

;; Checks to make sure Mu types obey the productivity check
;; Return the a version of type where superflouos mu are droped
;; and unrolled Mus are rerolled.
(: validate-type : srcloc Grift-Type -> Grift-Type)
(define (validate-type src t)
  (define who 'validate-type)
  (debug who src t)
  (: eqs (HashTable Grift-Type Grift-Type))
  (define eqs (make-hash '()))
  (: vt : (Setof Uid) -> (Grift-Type -> Grift-Type))
  (define (vt pending)
    (: rec : Grift-Type -> Grift-Type)
    (define (rec t)
      (cond
        [(Mu? t)
         (match-define (Mu s0) t)
         (define rec-uvar (next-uid! 'rec-fvar))
         (define rec-fvar (FVar rec-uvar))
         (define t0 (grift-type-instantiate s0 rec-fvar))
         (define t1 ((vt (set-add pending rec-uvar)) t0))
         ;; If the inner type is a Mu then we merge the
         ;; Mu we are creating with the inner one.
         ;; This is because both Mu's variables point to
         ;; the same location in the type.
         (define t2
           (cond
             [(Mu? t1)
              (match-define (Mu s1) t1)
              (grift-type-instantiate s1 rec-fvar)]
             [else t1]))
         (define-values (s1 used?)
           (grift-type-abstract/used? rec-uvar t2))
         ;; if rec-uvar isn't abstracted then t1 doesn't contain
         ;; any occurences of it and we don't have to bind it.
         (define ret (if used? (Mu s1) t2))
         (hash-set! eqs (grift-type-instantiate s1 ret) ret)
         ret]
        [(FVar? t)
         (match-define (FVar u) t)
         (when (set-member? pending u)
           (error 'non-productive-variable
                  "recursive variable appears in non-productive position ~a"
                  src))
         t]
        [else
         ;; If the type constructor is for actual data then the mu is productive
         ;; and we can clear the pending recursive variables.
         (define vt/pending
           (if (structural-type? t) (vt (set)) rec))
         (define r (grift-type-map t vt/pending))
         (or (hash-ref eqs r #f) r)]))
    rec)
  (debug who src type ((vt (set)) t)))

(module+ test
  (current-unique-counter (make-unique-counter 0))
  (test-equal?
   "validate type Mu without variable"
   (validate-type s (Mu (Scope (Dyn))))
   (Dyn))

  (test-equal?
   "validate type-Nested Mu without variable"
   (validate-type s (Fn 0 '() (Mu (Scope (Dyn)))))
   (Fn 0 '() (Dyn))))

;; Assumed to not validate types
(: synthesize-recursive-binding-type :
   (Option Grift-Type) G0-Ann-Expr -> (Values Grift-Type G0-Ann-Expr))
(define (synthesize-recursive-binding-type id-type? rhs)
  ;; Normally a binding construct assumes that the lack of type
  ;; annotation means to infer the type of the rhs, but recursive
  ;; binding constructs cannot be trivially infered.
  ;; We have arbitrarily made the following decisions.
  ;; (following inline comments)
  (cond
    ;; If the binding specifies a type always believe it.
    [id-type? (values id-type? rhs)]
    [else
     ;; If the user doesn't specify a binding type
     ;; use the type of the lambda.
     (match rhs
       [(Ann (Lambda (and f* (list (Fml _ _) ...)) (list b return-type?))
             s)
        (define fml-type (inst Fml-type Uid Grift-Type))
        (define t* (map fml-type f*))
        (cond
          [return-type?
           (define id-type (Fn (length t*) t* return-type?))
           (values id-type rhs)]
          [else
           ;; Function without return-type annotatin gets dyn return.
           (define id-type (Fn (length t*) t* DYN-TYPE))
           (define rhs     (Ann (Lambda f* (list b DYN-TYPE)) s))
           (values id-type rhs)])]
       ;; If it isn't a function we use dynamic as the type
       [rhs (values DYN-TYPE rhs)])]))


;;; Procedures that are used to throw errors
;; The error that occurs when a variable is not found. It is an internal
;; error because it is a syntax error to have an unbound variable.
(define-syntax-rule (lookup-failed src id)
  (lambda () (error 'variable-not-found "~a ~a" src id)))

#|
The type rules for core forms that have interesting type rules
|#

;; The type of a lambda that is annotated is the type of the annotation
;; as long as the annotation is consistent with the type of the
;; body
(: lambda-type-rule (-> Src Grift-Type* Grift-Type Grift-Type?
			(Fn Index Grift-Type* Grift-Type)))
(define (lambda-type-rule src ty-param* t-body return-ann)
  (cond
    [(not return-ann) (Fn (length ty-param*) ty-param* t-body)]
    [(consistent? t-body return-ann) (Fn (length ty-param*) ty-param* return-ann)]
    [else
     (error
      '|static type error|
      (string-append
       "Function return annotation inconsistent with the infered "
       "return type of the function.\n"
       "    location: " (or (srcloc->string src) "unkown location") "\n"
       "    annotated type: " (type->string return-ann) "\n"
       "    inferred type: " (type->string t-body) "\n"))]))

(module+ test
  (check-equal? (lambda-type-rule s '() (Dyn) (Int))
                (Fn 0 '() (Int)))
  (check-equal? (lambda-type-rule s '() (Dyn) #f)
                (Fn 0 '() (Dyn)))
  (check-equal? (lambda-type-rule s '() (Int) (Dyn))
                (Fn 0 '() (Dyn)))
  (check-exn
   #rx"static type error:"
   (lambda () (lambda-type-rule s '() (Int) (Bool)))))

(: tuple-type-rule (Grift-Type* -> Grift-Type))
(define (tuple-type-rule t*)
  (STuple (length t*) t*))


;; intersect :: most precise consistent subtype of either
;; (intersect t1 t2) = ⊥ , if t1 <≁ t2 
;; (intersect t1 t2) = t3, if t1 <~ t2, where t3 <~ t2, t3 <~ t2
;; This version of intersect has the property that any mu type
;; that is intersected with a non-mu type will return a non-mu type
;; this is usefull for pushing mu's down while writing type rules.
(define (intersect t g)
  ;; u = previously unfolded mu's
  ;; p = pair representing the current unfold operation
  ;; t = the mu type that is being unfolded
  ;; rec/other = a procedure that take the unfolded type and recurs
  (define (unfold a p t rec/other)
    (define x? (hash-ref a p #f))
    (cond
      [x? x?]
      [else
       (define u (next-uid! 'r))
       (match-define (Mu s) t)
       (define t0 (grift-type-instantiate s t))
       (define t1 (rec/other t0 (hash-set a p (FVar u))))
       (define-values (s0 used?) (grift-type-abstract/used? u t1))
       (if used? (Mu s0) t1)]))
  (with-handlers ([Bottom? values])
    (let rec/a ([t t] [g g] [a (hash)])
      (let rec ([t t] [g g])
        (match* (t g)
          [(t t)     t]
          [((Dyn) g) g]
          [(t (Dyn)) t]
          [((STuple ta t*) (STuple ga g*)) 
           ;; for/list semantics on mismatched lists is important here
           ;; this truncates the result to match the shortest list
           (STuple ta (for/list ([t t*] [g g*]) (rec t g)))]
          [((Fn ta ta* tr) (Fn ta ga* gr))
           (Fn ta (map rec ta* ga*) (rec tr gr))] 
          [((GRef t) (GRef g)) (GRef (rec t g))]
          [((GVect t) (GVect g)) (GVect (rec t g))]
          [((MRef t) (MRef g)) (MRef (rec t g))]
          [((Mu s) g) (unfold a (cons t g) t (λ (t a) (rec/a t g a)))]
          [(t (Mu s)) (unfold a (cons t g) g (λ (g a) (rec/a t g a)))]
          [(t g) (raise (Bottom t g))])))))

(module+ test
  (fresh-uids!)
  (check-equal? (intersect (Mu (Scope (STuple 2 (list (Int) (TVar 0)))))
                           (STuple 2 (list (Int) (Dyn))))
                (STuple 2 (list (Int) (Mu (Scope (STuple 2 (list (Int) (TVar 0)))))))))

(: tuple-proj-type-rule (Grift-Type Integer -> Grift-Type))
(define (tuple-proj-type-rule ty i)
  (define proj-arity (add1 i))
  (define proj-template (STuple proj-arity (make-list proj-arity DYN-TYPE)))
  (match (intersect ty proj-template)
    [(STuple l t*)
     (cond
       [(< -1 i l) (list-ref t* i)]
       [else (error 'grift "type error: tuple index out of bounds")])]
    [(Bottom _ _) (error 'grift "type error: not a tuple")]))

(module+ test
  (check-equal? (tuple-proj-type-rule (STuple 1 (list (Int))) 0)
                (Int))
  (fresh-uids!)
  (check-equal? (tuple-proj-type-rule (Mu (Scope (STuple 2 (list (Int) (TVar 0))))) 0)
                (Int)))



;; The type of a annotated let binding is the type of the annotation
;; as long as it is consistent with the type of the expression.
(: let-binding-type-rule (-> Grift-Type? Grift-Type Uid Src Grift-Type))
(define (let-binding-type-rule t-bnd t-exp id src)
  (cond
    [(not t-bnd) t-exp]
    [(consistent? t-bnd t-exp) t-bnd]
    [else (error 'binding-inconsistent "~a ~a ~a ~a" src id t-bnd t-exp)]))

;; The type of a cast is the cast-type if the expression type and
;; the cast type are consistent.
(: ascription-type-rule (-> Grift-Type Grift-Type Src (Option String)
			    Grift-Type))
(define (ascription-type-rule ty-exp ty-cast src label)
  (if (not (consistent? ty-exp ty-cast))
      (if label
	  (raise (exn:grift:type:static label (current-continuation-marks)))
	  (error 'ascription-inconsistent "~a ~a ~a" src ty-exp ty-cast))
      ty-cast))


;; |- c1 : t1, |- c2 : t2, 
;; |- (if e0 e1 e2) : t1 ∪_? t2,
;; where ∪_? is either the most precise union or least precise union

;; The type of an if is the join of the consequence and alternative
;; types if the type of the branches are consistent and the test is
;; consistent with Bool.
(: if-type-rule (-> Grift-Type Grift-Type Grift-Type Src
		    Grift-Type))
(define (if-type-rule t-tst t-csq t-alt src)
  (define if-t (join t-csq t-alt))
  (cond
    [(not (consistent? t-tst BOOL-TYPE))
     (error 'if-inconsistent-test "~a ~a" src t-tst)]
    [(Bottom? if-t)
     (error 'if-inconsistent-branches "~a ~a ~a" src t-csq t-alt)]
    [else if-t]))

(: switch-type-rule : Grift-Type Grift-Type* Grift-Type -> Grift-Type)
(define (switch-type-rule exp clause* default)
  (define switch-t (join+ (cons default clause*)))
  (cond
    [(not (consistent? exp INT-TYPE))
     (error 'consistency
            "static type error: switch value inconsitent with Int")]
    [(Bottom? switch-t)
     (error 'consistency
            "static type error: inconsitent return types in switch")]
    [else switch-t]))

;; The type of literal constants are staticly known
(: const-type-rule (Grift-Literal -> Base-Type))
(define (const-type-rule c)
  (define who 'type-check/const-type-rule)
  (grift-literal->base-type c))

;; The type of an application is the return type of the applied
;; procedure given that the arguments are consistent with the
;; arguments types of the proceedure.
(: application-type-rule (-> Grift-Type Grift-Type* Src Grift-Type))
(define (application-type-rule t-rator t-rand* src)
  (match (unfold-possible-mu t-rator)
    [(Dyn) (Dyn)]
    [(Fn arity t-fml* t-ret)
     (unless (= arity (length t-rand*))
       (error
         '|static type error|
        (string-append
         "Trying to apply function type of arity " (number->string arity)
         " to " (number->string (length t-rand*)) " arguments.\n"
         "   location: " (or (srcloc->string src) "unkown location") "\n")))
     (for ([t-fml t-fml*] [t-rand t-rand*] [n (in-naturals 1)])
       (unless (consistent? t-fml t-rand)
         (error
          '|static type error|
          (string-append
           "Function argument type inconsistent with applied argument type.\n"
           "   location: " (or (srcloc->string src) "unkown location") "\n"
           "   argument number: " (number->string n) "\n"
           "   specifically: " (type->string t-fml)
           " is inconsistent with " (type->string t-rand) "\n"))
         (error 'app-inconsistent "~a ~a ~a" src t-rator t-rand*)))
     t-ret]
    [other
     (error
      '|static type error|
      (string-append
      "Trying to apply non-function type: " (type->string other) "\n"
       "   location: " (or (srcloc->string src) "unkown location") "\n"))]))

(module+ test

  (check-equal?
   (application-type-rule (Dyn) (list (Int)) s)
   (Dyn))
  (check-equal?
   (application-type-rule
    (Mu (Scope (Fn 1 (list (Int)) (TVar 0))))
    (list (Int))
    s)
   (Mu (Scope (Fn 1 (list (Int)) (TVar 0)))))
  (check-equal?
   (application-type-rule (Fn 0 '() (Bool)) '() s)
   (Bool))
  (check-exn
   #rx"static type error:"
   (lambda ()
     (application-type-rule (Fn 1 (list (Int)) (Int)) (list (Bool)) s)))
  (check-exn
   #rx"static type error:"
   (lambda ()
     (application-type-rule (Fn 0 '() (Unit)) (list (Unit))  s))) 
  (check-exn
   #rx"static type error:"
   (lambda ()
     (application-type-rule
      (Mu (Scope (STuple 1 (list (TVar 0)))))
      (list (Int))
      s))))


;; I am really just defining this in order to maintain
;; the abstraction but the type of a begin is the type
;; of it's final argument
(: begin-type-rule (-> Grift-Type* Grift-Type Grift-Type))
(define (begin-type-rule t* ty) ty)

(: repeat-type-rule (Grift-Type Grift-Type Grift-Type Grift-Type -> Grift-Type))
(define (repeat-type-rule tstart tstop tacc tbody)
  (cond
    [(not (consistent? tstart INT-TYPE))
     (error 'type-check/repeat-type-rule
            "Index initialization must be consistent with Int, actual type: ~a"
            tstart)]
    [(not (consistent? tstop INT-TYPE))
     (error 'type-check/repeat-type-rule
            "Index limit must be consistent with Int, actual type: ~a"
            tstop)]
    [(not (consistent? tacc tbody))
     (error 'type-check/repeat-type-rule
            "Accumulator and body inconsistent with types:\n accumulator: ~a\n body: ~a"
            tacc tbody)]
    [else tacc]))

;; The type of wrapping a value in a gaurded box is a
;; Gref of the value's type
(: gbox-type-rule (-> Grift-Type Grift-Type))
(define (gbox-type-rule ty) (GRef ty))

;; The type of unwrapping a Dyn value is also Dyn
;; The type of unwrapping a Ref is the type that the reference is
;; parameterized by.
(: gunbox-type-rule (-> Grift-Type Grift-Type))
(define (gunbox-type-rule ty)
  (match ty
    [(Dyn) DYN-TYPE]
    [(GRef g) g]
    [otherwise (error 'type-check/todo)]))

;; The type of setting a reference is always unit
(: gbox-set!-type-rule (-> Grift-Type Grift-Type Grift-Type))
(define (gbox-set!-type-rule box-ty val-ty)
  (match box-ty
    [(Dyn) UNIT-TYPE]
    [(GRef g)
     (if (consistent? g val-ty)
         UNIT-TYPE
         (error 'type-error
                "incompatible types in guarded assignment: ~a ~a"
                g val-ty))]
    [otherwise
     (error 'type-error
            "attempt to make guarded box assignment to non guarded box: ~a"
            box-ty)]))

;; The type of wrapping a value in a monotonic box is a
;; MRef of the value's type
(: mbox-type-rule (-> Grift-Type Grift-Type))
(define (mbox-type-rule ty) (MRef ty))

;; The type of unboxing a Dyn value is also Dyn
;; The type of unboxing a MRef is the type that the reference is
;; parameterized by.
(: munbox-type-rule (-> Grift-Type Grift-Type))
(define (munbox-type-rule ty)
  (match ty
    [(Dyn) DYN-TYPE]
    [(MRef m) m]
    [otherwise (error 'type-error "attempt to unbox non-monotonic box: ~a" ty)]))

;; The type of setting a dyn value is dyn
;; The type of setting a MRef value is the type of the argument
(: mbox-set!-type-rule (-> Grift-Type Grift-Type Grift-Type))
(define (mbox-set!-type-rule box-ty val-ty)
  (match box-ty
    [(Dyn) UNIT-TYPE]
    [(MRef m) (if (consistent? m val-ty)
                  UNIT-TYPE
                  (error 'type-check/todo))]
    [otherwise (error 'type-check/todo)]))

(: mbox-val-type (-> Grift-Type Grift-Type))
(define (mbox-val-type box-ty)
  (match box-ty
    [(Dyn) DYN-TYPE]
    [(MRef t) t]
    [otherwise (error 'typecheck/mbox-val-type)]))

;; The type of creating an array is Vect of the type of the initializing argument
;; The size argument must be consistent with Int
(: gvector-type-rule (-> Grift-Type Grift-Type Grift-Type))
(define (gvector-type-rule size-ty init-ty)
  (if (consistent? size-ty INT-TYPE)
      (GVect init-ty)
      (error 'type-check/todo)))

;; The type of reffing into an Dyn is Dyn
;; The type of reffing into a Vect T is T
;; The type of the index must be consistent with Int
(: gvector-ref-type-rule (-> Grift-Type Grift-Type Grift-Type))
(define (gvector-ref-type-rule vect-ty index-ty)
  (if (consistent? index-ty INT-TYPE)
      (match vect-ty
        [(Dyn) DYN-TYPE]
        [(GVect g) g]
        [otherwise (error 'type-check/todo)])
      (error 'type-check/todo)))

;; The type of setting a guarded vector of type T is the type of
;; The new value as long as the new value is consistent with the old value
;; The type of setting a Dyn is Dyn
;; The indice must be consistent with int

;; FIXME: return unit after setting a vector cell
(: gvector-set!-type-rule (-> Grift-Type Grift-Type Grift-Type Grift-Type))
(define (gvector-set!-type-rule vect-ty index-ty val-ty)
  (if (consistent? index-ty INT-TYPE)
      (match vect-ty
        [(Dyn) UNIT-TYPE]
        [(GVect g) (if (consistent? g val-ty)
                       UNIT-TYPE
                       (error 'type-check/todo))]
        [otherwise (error 'type-check/todo)])
      (error 'type-check/todo)))


;; The type of creating an array is Vect of the type of the initializing argument
;; The size argument must be consistent with Int
(: mvector-type-rule (-> Grift-Type Grift-Type Grift-Type))
(define (mvector-type-rule size-ty init-ty)
  (if (consistent? size-ty INT-TYPE)
      (MVect init-ty)
      (error 'type-check/todo)))

(: mvector-length-type-rule (-> Grift-Type Grift-Type))
(define (mvector-length-type-rule vect-ty)
  (if (or (MVect? vect-ty) (Dyn? vect-ty))
      INT-TYPE
      (error 'type-check/todo)))

(: gvector-length-type-rule (-> Grift-Type Grift-Type))
(define (gvector-length-type-rule vect-ty)
  (if (or (GVect? vect-ty) (Dyn? vect-ty))
      INT-TYPE
      (error 'type-check/todo)))

;; The type of reffing into an Dyn is Dyn
;; The type of reffing into a Vect T is T
;; The type of the index must be consistent with Int
(: mvector-ref-type-rule (-> Grift-Type Grift-Type Grift-Type))
(define (mvector-ref-type-rule vect-ty index-ty)
  (if (consistent? index-ty INT-TYPE)
      (match vect-ty
        [(Dyn) DYN-TYPE]
        [(MVect g) g]
        [otherwise (error 'type-check/todo)])
      (error 'type-check/todo)))

;; The type of setting a monotonic vector of type T is the type of
;; The new value as long as the new value is consistent with the old value
;; The type of setting a Dyn is Dyn
;; The indice must be consistent with int
(: mvector-set!-type-rule (-> Grift-Type Grift-Type Grift-Type Grift-Type))
(define (mvector-set!-type-rule vect-ty index-ty val-ty)
  (if (consistent? index-ty INT-TYPE)
      (match vect-ty
        [(Dyn) UNIT-TYPE]
        [(MVect g) (if (consistent? g val-ty)
                       UNIT-TYPE
                       (error 'type-check/todo))]
        [otherwise (error 'type-check/todo)])
      (error 'type-check/todo)))

(: mvector-val-type (-> Grift-Type Grift-Type))
(define (mvector-val-type vect-ty)
  (match vect-ty
    [(Dyn) DYN-TYPE]
    [(MVect t) t]
    [otherwise (error 'type-check/mvector-val-type)]))
                      
;;; Procedures that destructure and restructure the ast
(: tc-expr (-> G0-Ann-Expr Env (values G1-Ann-Expr Grift-Type)))
(define (tc-expr exp env)
  (debug 'tc-expr exp env)
  (: env-extend/bnd ((Bnd Uid Grift-Type Any) Env -> Env))
  (define (env-extend/bnd b env)
    (match-let ([(Bnd i t _) b])
      (hash-set env i t)))
  (: letrec-synth-bnd-type :
     (Bnd Uid (Option Grift-Type) G0-Ann-Expr) -> (Bnd Uid Grift-Type G0-Ann-Expr))
  (define (letrec-synth-bnd-type bnd)
    (match-define (Bnd id id-type? rhs) bnd)
    (define-values (id-type new-rhs)
      (synthesize-recursive-binding-type id-type? rhs))
    (Bnd id id-type new-rhs))
  (: map-letrec-synth-bnd-type :
     (Listof (Bnd Uid (Option Grift-Type) G0-Ann-Expr)) ->
     (Listof (Bnd Uid Grift-Type G0-Ann-Expr)))
  (define (map-letrec-synth-bnd-type bnd*)
    (map letrec-synth-bnd-type bnd*))
  (: map-recur (G0-Ann-Expr* -> (Values G1-Ann-Expr* Grift-Type*)))
  (define (map-recur e*)
    (for/lists ([e* : G1-Ann-Expr*] [t* : Grift-Type*]) ([e e*])
      (tc-expr e env)))
  (: recur-switch-case* :
     (Switch-Case* G0-Ann-Expr) ->
     (Values (Switch-Case* G1-Ann-Expr) Grift-Type*))
  (define (recur-switch-case* c*)
    (map-switch-case2* recur c*))
  (: recur (-> G0-Ann-Expr (Values G1-Ann-Expr Grift-Type)))
  (define (recur e)
    (: src srcloc)
    (define src (Ann-data e))
    (: validate : Grift-Type -> Grift-Type)
    (define (validate t) (validate-type src t))
    (: new-exp G1-Expr)
    (: type Grift-Type)
    (define (trace-type-error [e : exn])
      (printf "error while type-checking: ~a\n" (srcloc->string src))
      (raise e))
    (define-values (new-exp type)
      (with-handlers ([exn? trace-type-error])
        (match (Ann-value e)
          [(Lambda f* (list b t?))
           (define i* (map (inst Fml-identifier Uid Grift-Type) f*))
           (define t* (map (inst Fml-type Uid Grift-Type) f*))
           (define vt* (map validate t*))
           (define vt? (and t? (validate t?)))
           (define-values (body type-of-body) (tc-expr b (env-extend* env i* vt*)))
           (values (Lambda f* body)
                   (lambda-type-rule src vt* type-of-body vt?))]
          [(Letrec (app map-letrec-synth-bnd-type bnd*) body)
           ;; letrce-infer-bnd-type maps validate type
           (define recursive-env (foldr env-extend/bnd env bnd*))
           (: rec/env : G0-Ann-Expr -> (Values G1-Ann-Expr Grift-Type))
           (define (rec/env e) (tc-expr e recursive-env))
           (define new-bnd (map (tc-binding src rec/env) bnd*))
           (define-values (new-body type) (rec/env body))
           (values (Letrec new-bnd new-body) type)]
          [(Let (app (map-tc-binding src recur) bnd*) body)
           (define env0 (foldl env-extend/bnd env bnd*))
           (define-values (new-body type) (tc-expr body env0))
           (values (Let bnd* new-body) type)]
          [(App (app recur rator ty-rator) (app map-recur rand* ty-rand*))
           (values (App rator rand*)
                   (application-type-rule ty-rator ty-rand* src))]
          [(Op (and p (app grift-primitive->type ty-p))
               (app map-recur rand* ty-rand*))
           (unless (Fn? ty-p)
             (error 'type-check/Op "assuming operators have function type"))
           (values (Op (list p (Fn-fmls ty-p)) rand*)
                   (application-type-rule ty-p ty-rand* src))]
          [(Ascribe (app recur exp ty-exp) ty-ann label)
           (values (Ascribe exp ty-exp label)
                   (ascription-type-rule ty-exp (validate ty-ann) src label))]
          [(If (app recur tst ty-tst)
               (app recur csq ty-csq)
               (app recur alt ty-alt)) 
           (values (If tst csq alt)
                   (if-type-rule ty-tst ty-csq ty-alt src))]
          [(Switch (app recur e et)
             (app recur-switch-case* c* t*)
             (app recur d dt))
           (values (Switch e c* d) (switch-type-rule et t* dt))]
          [(and e (Var id))
           (debug e env id src (hash-ref env id #f))
           (values e (hash-ref env id (lookup-failed src id)))]
          [(and e (Quote lit)) (values e (const-type-rule lit))]
          [(Repeat index
               (app recur start ty-start)
               (app recur stop ty-stop)
               (list acc ty-acc?)
               (app recur acc-init ty-acc-init)
             exp) 
           (define ty-acc (let-binding-type-rule (and ty-acc? (validate ty-acc?))
                                                 ty-acc-init acc src))
           (define repeat-env (hash-set (hash-set env index INT-TYPE) acc ty-acc))
           (define-values (new-exp ty-exp) (tc-expr exp repeat-env))
           (values (Repeat index start stop acc acc-init new-exp)
                   (repeat-type-rule ty-start ty-stop ty-acc ty-exp))]
          [(Begin (app map-recur e* t*) (app recur e t))
           (values (Begin e* e) (begin-type-rule t* t))]
          [(Gbox (app recur e t))
           (values (Gbox e) (gbox-type-rule t))]
          [(Gunbox (app recur e t))
           (values (Gunbox e) (gunbox-type-rule t))]
          [(Gbox-set! (app recur e1 t1) (app recur e2 t2))
           (values (Gbox-set! e1 e2) (gbox-set!-type-rule t1 t2))]
          [(MboxS (app recur e t))
           (values (Mbox e t) (mbox-type-rule t))]
          [(Munbox (app recur e t))
           (let ([ty (munbox-type-rule t)])
             (values (Munbox e) ty))]
          ;; move it to insert casts
          [(Mbox-set! (app recur e1 t1) (app recur e2 t2))
           (let ([ty (mbox-set!-type-rule t1 t2)])
             (values (Mbox-set! e1 e2) ty))]
          [(MvectorS (app recur e1 t1) (app recur e2 t2))
           (let ([ty (mvector-type-rule t1 t2)])
             (values (Mvector e1 e2 t2) ty))]
          [(Mvector-ref (app recur e1 t1) (app recur e2 t2))
           (let ([ty (mvector-ref-type-rule t1 t2)])
             (values (Mvector-ref e1 e2) ty))]
          [(Mvector-set! (app recur e1 t1) (app recur e2 t2) (app recur e3 t3))
           (let ([ty (mvector-set!-type-rule t1 t2 t3)])
             (values (Mvector-set! e1 e2 e3) ty))]
          [(Mvector-length (app recur e t))
           (values (Mvector-length e) (mvector-length-type-rule t))]
          [(Gvector (app recur e1 t1) (app recur e2 t2))
           (values (Gvector e1 e2) (gvector-type-rule t1 t2))]
          [(Gvector-ref (app recur e1 t1) (app recur e2 t2))
           (values (Gvector-ref e1 e2) (gvector-ref-type-rule t1 t2))]
          [(Gvector-set! (app recur e1 t1) (app recur e2 t2) (app recur e3 t3))
           (values (Gvector-set! e1 e2 e3) (gvector-set!-type-rule t1 t2 t3))]
          [(Gvector-length (app recur e t))
           (values (Gvector-length e)
                   (gvector-length-type-rule t))]
          [(Create-tuple (app map-recur e* t*))
           (values (Create-tuple e*) (tuple-type-rule t*))]
          [(Tuple-proj (app recur e t) i)
           (values (Tuple-proj e i) (tuple-proj-type-rule t i))]
          [other (error 'type-check "unmatched ~a" other)]))
      )
    (values (Ann new-exp (cons src type)) type))
  (recur exp))


;; Type checks the rhs to be consistent with type annotation if
;; provided the resulting type is the type of the annotation.
(: tc-binding :
   Src (-> G0-Ann-Expr (values G1-Ann-Expr Grift-Type)) -> (-> G0-Bnd G1-Bnd))
(define ((tc-binding src tc-expr) bnd)
  (match-define (Bnd id t0 rhs) bnd)
  (define-values (new-rhs rhs-type) (tc-expr rhs))
  (define t1 (let-binding-type-rule (and t0 (validate-type src t0)) rhs-type id src))
  (Bnd id t1 new-rhs))

(: map-tc-binding :
   Src (-> G0-Ann-Expr (values G1-Ann-Expr Grift-Type))
   -> (-> G0-Bnd* G1-Bnd*))
(define ((map-tc-binding src tc-expr) bnd*)
  (map (tc-binding src tc-expr) bnd*))


(: map-switch-case2 :
   (All (A B C)
        (A -> (Values B C))
        (Switch-Case A) ->
        (Values (Switch-Case B) C)))
(define (map-switch-case2 f c)
  (define-values (r0 r1) (f (cdr c)))
  (values (cons (car c) r0) r1))
(: map-switch-case2* :
   (All (A B C)
        (A -> (Values B C))
        (Switch-Case* A)
        ->
        (Values (Switch-Case* B) (Listof C))))
(define (map-switch-case2* f a*)
  (for/lists ([b* : (Switch-Case* B)]
              [c* : (Listof C)])
             ([a a*])
    (map-switch-case2 f a)))


;; unfold-possible-mu :: takes a type and converts it to a equivalent type
;; where the outermost type is a concrete type constructor (like ->, Tuple, etc.) as
;; opposed to logical (like Rec).
;; NOTE: this should only be called on types that have been converted to normal form. 
(define (unfold-possible-mu t)
  (let rec ([t t])
    (match t
      [(Mu s) (unfold-possible-mu (grift-type-instantiate s t))]
      [t t])))
