#lang racket/base
#|------------------------------------------------------------------------------+
Pass: src/schml/syntax->schml0.rkt
+-------------------------------------------------------------------------------+
Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)
+-------------------------------------------------------------------------------+
Discription: The parse pass takes racket's native syntax objects to a set of 
the only reason that syntax objects are used in to correlate source location
information. Namely the syntax->schml0 tranformation does not handle expansion
whatsoever identifiers maintain lexical scope according to symbolic equality.
+------------------------------------------------------------------------------|#

;; TODO Make sure that we are correlating errors with source code.

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/list
         syntax/parse 
         syntax/location
         "../unique-counter.rkt"
         "../logging.rkt"
         ;;"../helpers-untyped.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/forms.rkt"
         "../language/schml0.rkt"
         "../language/make-begin.rkt")

(require syntax/stx)

(provide syntax->schml0
         parse-type)

(define/match (syntax->schml0 prgm)
  [((Prog name s*))
   (define who 'syntax->schml0)
   (define uc (make-unique-counter 0))
   (define tl*
     (parameterize ([current-unique-counter uc])
       ((parse-top-level name top-level-env)  s*)))
   (debug who (Prog (list name (unique-counter-next! uc)) tl*))]
  [(other) (error 'syntax->schml0 "unmatched: ~a" other)])

(define (syntax->srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))

;; String (Hashtable Symbol Uid) -> (Listof Syntax) -> S0-Top*
(define ((parse-top-level name env) stx*)
  (debug 'parse-top-level name env stx*)
  (match stx*
    [(list s* ...)
     (define/match (observe/ann e)
       [((Ann (Define _ _ _ _) s)) e]
       [((Ann _ s)) (Ann (Observe e #f) s)]
       [(other) (error 'parse-top-level/insert-observe
                       "unmatched: ~a" other)])
     (define rec-env (scan/check-define-scope name s*))
     (debug rec-env (map observe/ann (map (parse-form (env-union env rec-env)) s*)))]
    [other (error 'parse-top-level "unmatched: ~a" other)]))

;; String (U Syntax (Listof Syntax)) -> (Hashtable Symbol Uid)
(define (scan/check-define-scope name stx)
  (define who 'scan/check-define-scope)
  (debug who name stx)
  ;; syntax -> (Option (Pair id uid))
  (define (match-ids stx)
    (syntax-parse stx
      #:datum-literals (define)
      [(~or (define v:id . _) (define (v:id _ ...) . _))
       (list (syntax-e #'v) (syntax->srcloc stx) (id->uid #'v))]
      [_ #f]))
  ;; Checks that x isn't already in xs signals an error if so
  (define (check-unique x xs)
    (match-define (list v s u) x)
    ;; x? : (Option (List Symbol srcloc Uid))
    (define x? (hash-ref xs v #f))
    (when x?
      (error 'schml "duplicate definitions: ~a\n  at:~a  and ~a"
             v s (cadr x?)))
    (hash-set xs v x))
  (define (env-cons x env)
    (match-define (list v _ u) x)
    (hash-set env v (lexical-var u)))
  ;; Check 
  (define stx* (if (list? stx) stx (syntax->list stx)))
  (define rec-vars (filter values (map match-ids stx*)))
  ;; for the effects of the check
  (debug who (foldl check-unique (hash) rec-vars))
  ;; for the final value of the environment
  (debug who (foldl env-cons (hash) rec-vars)))




(define ((parse-form env) stx)
  (debug 'parse-form stx env)
  (define src (syntax->srcloc stx))
  (define cf
    (syntax-parse stx
      [var:id (Var ((env-get-lexical 'parse-form) env #'var))]
      [(var:id rest ...)
       (match (env-lookup env #'var)
         [(lexical-var var)
          (define var-src (quote-srcloc #'var))
          (define as (map (parse-form env) (syntax->list #'(rest ...))))
          (App (Ann (Var var) var-src) as)]
         [(core parse) (parse stx env)]
         [other (error 'parse-form "unmatched: ~a" other)])]
      [(fst rest ...)
       (define p ((parse-form env) #'fst))
       (define as (map (parse-form env) (syntax->list #'(rest ...))))
       (App p as)]
      [other
       (define dat (syntax->datum #'other))
       (unless (schml-literal? dat)
         (syntax-error 'parse-form "expected datum" #'other))
       (if (and (not (exact-integer? dat)) (real? dat))
           (Quote (exact->inexact dat))
           (Quote dat))]))
  (Ann cf src))

#;(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (null? x)
      (real? x)))

(define-syntax syntax-error
  (syntax-rules ()
    [(_ name message) (error name message)]
    [(_ name message expr)
     (let ([msg (format "~a\n\tin:~a" message expr)])
       (error name msg))]
    [(_ name message expr sub)
     (let ([msg (format "~a\n\tin:~a\n\tin particular: ~a" message expr sub)])
       (error name msg))])) 

#;(define (schml-core-form? x)
  (or (Lambda? x) (Letrec? x) (Let? x) (Var? x) (App? x)
      (Op? x) (Quote? x) (Ascribe? x)
      (Begin? x) (If? x) (Repeat? x)
      ;; Constructor, Destructor, Mutator should be factored out
      ;; (Ctr 'Gbox (list arg)) (Ctr 'Gvector (list arg1 arg2))
      (Gbox? x) (Gunbox? x) (Gbox-set!? x)
      (Gvector? x) (Gvector-ref? x) (Gvector-set!? x)
      (MboxS? x) (Munbox? x) (Mbox-set!? x)
      (MvectorS? x) (Mvector-ref? x) (Mvector-set!? x)))

(struct lexical-var  (symbol))
(struct core (parser))
(struct core-atomic (atom))

(define (env-lookup env x)
  (debug off 'env-lookup env x)
  (define (err)
    (syntax-error 'environment "unbound variable" x))
  (hash-ref env (syntax->datum x) err))

(define ((env-get-lexical src) env x)
  (match (env-lookup env x)
    [(lexical-var var) var]
    [other (error src "expected lexical variable for ~a: found ~a"
                  x other)]))


(define (env-extend env x u)
  (hash-set env (syntax->datum x) (lexical-var u)))

(define (env-cons k v e) (hash-set e k v))

(define (env-union e1 e2)
  (define k.v* (hash->list e2))
  (foldl env-cons e1 (map car k.v*) (map cdr k.v*)))

(define-syntax-class binding
  [pattern (var:id (~datum :) ty rhs)]
  [pattern (var:id rhs) #:with ty #f])

(define (id->uid x)
  (next-uid! (symbol->string (syntax->datum x))))

(define ((parse-optional-type env) x)
  (and (syntax? x) (syntax-e x) (parse-type x env)))

(define (destruct-and-parse-bnds-lhs ids env)
  (define uids (map id->uid ids))
  (define new-env (foldl (lambda (i u e) (env-extend e i u)) env ids uids))
  (values new-env uids))

(define (parse-let-expression stx env)
  (syntax-parse stx
    [(_ (bnd:binding ...) body)
     (define-values (new-env uids)
       (destruct-and-parse-bnds-lhs (syntax->list #'(bnd.var ...)) env))
     (define tys  (map (parse-optional-type env) (syntax->list #'(bnd.ty  ...))))
     (define rhss (map (parse-form env) (syntax->list #'(bnd.rhs ...))))
     (Let (map Bnd uids tys rhss)
      ((parse-form new-env) #'body))]))

(define (parse-letrec-expression stx env)
  (syntax-parse stx
    [(_ (bnd:binding ...) body)
     (define-values (new-env uids)
       (destruct-and-parse-bnds-lhs (syntax->list #'(bnd.var ...)) env))
     (define tys  (map (parse-optional-type env) (syntax->list #'(bnd.ty  ...))))
     (define rhss (map (parse-form new-env) (syntax->list #'(bnd.rhs ...))))
     (Letrec (map Bnd uids tys rhss)
       ((parse-form new-env) #'body))]))


(module+ test
  (require "../language/forms-equal.rkt")
  (current-unique-counter (make-unique-counter 0))
  (check-forms=?
   ((parse-form top-level-env) #'(let ([x 1]) x))
   (Ann (Let (list (Bnd 'x #f (Ann (Quote 1) (quote-srcloc))))
          (Ann (Var 'x) (quote-srcloc)))
        (quote-srcloc)))
  (check-forms=?
   ((parse-form top-level-env) #'(letrec ([x x]) x))
   (Ann (Letrec (list (Bnd 'x #f (Ann (Var 'x) (quote-srcloc))))
          (Ann (Var 'x) (quote-srcloc)))
        (quote-srcloc))))

(define-splicing-syntax-class optional-colon-type
  [pattern (~optional (~seq (~datum :) ty) #:defaults ([ty #'#f]))])

(define-splicing-syntax-class optionally-annotated-id
  [pattern (~seq id:id oa:optional-colon-type) #:with ty #'oa.ty])

(define-syntax-class formal-parameter
  [pattern [var:id (~datum :) ty]]
  [pattern var:id #:with ty #'Dyn])

(define (parse-define-form stx env)
  (define (help rec? id ty? body)
    (Define
      rec?
      ((env-get-lexical 'parse-define-form) env id)
      ((parse-optional-type env) ty?)
      ((parse-form env) body)))
  (debug stx env)
  (syntax-parse stx
    [(_ v:optionally-annotated-id e:expr)
     (help #f #'v.id #'v.ty #'e)]
    [(_ (f:id f*:formal-parameter ...) r:optional-colon-type e:expr)
     (if (syntax->datum #'r.ty)
         (help #t #'f #f (syntax/loc stx (lambda (f* ...) : r.ty e)))
         (help #t #'f #f (syntax/loc stx (lambda (f* ...) e))))]))

(module+ test
  (parse-define-form '(define x : Float #i9.8) (hash-set top-level-env 'x (lexical-var (Uid "x" 0)))))

(define (parse-lambda-expression stx env)
  (syntax-parse stx
    [(_ (fml:formal-parameter ...) r:optional-colon-type body:expr)
     (define-values (new-env uids)
       (destruct-and-parse-bnds-lhs (syntax->list #'(fml.var ...)) env))
     (define (pt s) (parse-type s env))
     (define tys  (map pt (syntax->list #'(fml.ty  ...))))
     (Lambda (map Fml uids tys)
       (Ann ((parse-form new-env) #'body)
            ((parse-optional-type new-env) #'r.ty)))]
    [other (error 'parse-lambda "unmatched ~a" (syntax->datum stx))]))

(module+ test
  (check-forms=?
   ((parse-form top-level-env) #'(lambda (x y) x))
   (Ann (Lambda (list (Fml 'x (Dyn)) (Fml 'y (Dyn)))
          (Ann (Ann (Var 'x) (quote-srcloc)) #f))
        (quote-srcloc)))
  (check-forms=?
   ((parse-form top-level-env) #'(lambda ([x : Int]) x))
   (Ann (Lambda (list (Fml 'x (Int)))
          (Ann (Ann (Var 'x) (quote-srcloc)) #f))
        (quote-srcloc)))
  (check-forms=?
   ((parse-form top-level-env) #'(lambda ([x : Int]) : Int x))
   (Ann (Lambda (list (Fml 'x (Int)))
          (Ann (Ann (Var 'x) (quote-srcloc)) (Int)))
        (quote-srcloc))))

(define (parse-op stx env)
  (syntax-parse stx
    [(op rands ...)
     (define o (syntax->datum #'op))
     (define r* (syntax->list #'(rands ...)))
     (Op o (map (parse-form env) r*))]))

(define (parse-ascription stx env)
  (syntax-parse stx
    [(_ expr ty (~optional (~seq l:str) #:defaults ([l #'#f])))
     (define lbl (syntax->datum (attribute l)))
     (unless (or (not lbl) (string? lbl))
       (error 'parse-ascription "this doesn't work like I thought it would: ~a ~a" lbl #'l))
     (Ascribe ((parse-form env) #'expr) (parse-type #'ty env) lbl)]))

(module+ test
  (check-forms=? (parse-ascription #'(ann 3 Int) top-level-env)
                 (Ascribe (Ann (Quote 3) (quote-srcloc)) (Int) #f))
  
  (check-forms=? (parse-ascription #'(ann #f Int "Bad") top-level-env)
                 (Ascribe (Ann (Quote #f) (quote-srcloc)) (Int)  "Bad")))

(define (parse-begin stx env)
  (syntax-parse stx
    [(_ expr* ... expr)
     (Begin (map (parse-form env) (syntax->list #'(expr* ...)))
            ((parse-form env) #'expr))]))

(define (env-extend* env . bnds)
  (let loop ([env env] [b* bnds])
    (match b*
      [(list) env]
      [(cons id (cons uid others))
       (loop (env-extend env id uid) others)]
      [other (error 'env-extend* "unmatched ~a" other)])))

(define (parse-repeat stx env)
  (debug off 'parse-repeat env)
  (define-syntax-class index-binding
    [pattern ((~var id id) (~var start) (~var stop))])
  (define-syntax-class accumulator-binding
    #:datum-literals (:)
    [pattern ((~var id id)
              (~optional (~seq : (~var type))
                         #:defaults ([type #'#f]))
              (~var init))])
  (syntax-parse stx
    [(_ i:index-binding a:accumulator-binding M)
     (define i (id->uid #'i.id))
     (define a (id->uid #'a.id))
     (unless (and (Uid? i) (Uid? a))
       (error 'crap))
     (define new-env (env-extend* env #'i.id i #'a.id a))
     (define recur (parse-form env))
     (Repeat i (recur #'i.start) (recur #'i.stop)
       (Ann a ((parse-optional-type env) #'a.type))
       ((parse-form env) #'a.init)
       ((parse-form new-env) #'M))]))

(module+ test
  (parse-repeat #'(repeat (foo 0 10) (bar : Int 0) foo) top-level-env))

(define (parse-switch stx env)
  (define-syntax-class switch-clause
    [pattern [(case*:integer ...+) rhs]])
  (define recur (parse-form env))
  (syntax-parse stx
    #:datum-literals (else)
    [(_ e c*:switch-clause ... [else default])
     (Switch (recur #'e)
             (map cons
                  (syntax->datum #'((c*.case* ...) ...))
                  (map recur (syntax->list #'(c*.rhs ...))))
             (recur #'default))]))

(define ((parse-simple-form sym ctr arg-count) stx env)
  (syntax-parse stx
    #:context sym
    [(_ params ...)
     #:do [(define p* (syntax->list #'(params ...)))]
     #:when (= (length p*) arg-count)
     (apply ctr (map (parse-form env) p*))]))

(define ((parse-simple*-form sym ctr) stx env)
  (syntax-parse stx
    #:context sym
    [(_ params ...)
     #:do [(define p* (syntax->list #'(params ...)))]
     (ctr (map (parse-form env) p*))]))

(define (parse-tuple-proj stx env)
  (syntax-parse stx
    [(_ t i:integer)
     (Tuple-proj ((parse-form env) #'t) (syntax->datum #'i))]))


(module+ test
  (define parse-tuple (parse-simple*-form 'tuple Create-tuple))
  (check-forms=? (parse-tuple #'(tuple 0 1 2) top-level-env)
                 (Create-tuple
                  (list (Ann (Quote 0) (quote-srcloc))
                        (Ann (Quote 1) (quote-srcloc))
                        (Ann (Quote 2) (quote-srcloc))))))

(define (parse-time stx env)
  ;; this should be implemented as a macro once we have an expander
  (syntax-parse stx
    [(_ exp)
     (define src (syntax->srcloc stx))
     (define (/src e) (Ann e src))
     (define/match (begin . e*)
       [((list e* ... e)) (Begin (map /src e*) (/src e))])
     (define (op s) (Op s '()))
     (define tmp (next-uid! "tmp_s2s0"))
     (begin (op 'timer-start)
            (Let (list (Bnd tmp #f ((parse-form env) #'exp)))
              (/src (begin (op 'timer-stop)
                           (op 'timer-report)
                           (Var tmp)))))]))


#|
Parse Type converts syntax objects to the core forms that
represents types in the schml abstract syntax tree.
|#
;|#

(define (parse-type stx [env top-level-env])
  (define (recur s) (parse-type s env))
  (syntax-parse stx
    #:datum-literals (-> Ref Vect GRef GVect MRef MVect Tuple)
    [atm:id
     (match (env-lookup env #'atm)
       ;;[(lexical-var id) id]
       [(core-atomic cf) cf]
       [other
        (error 'parse-type
               "unmatched atom ~a resolved to ~a"
               (syntax->datum #'atm) other)])]
    [() UNIT-TYPE]
    ;; Nice syntax for types currently implemented with support from parser
    ;; Todo: should we provide an infix binding construct in general?
    [(t* ... -> t)
     (define a* (syntax->list #'(t* ...)))
     (Fn (length a*) (map recur a*) (recur #'t))]
    [(atm:id stx* ...)
     (match (env-lookup env #'atm)
       [(core t) (t stx env)]
       [other
        (error 'parse-type
               "unmatched type operator ~a resolved to ~a"
               (syntax->datum #'atm) other)])]))


(define ((make-parse-type-w/ctr sym ctr) stx env)
  (syntax-parse stx
    [(_ stx) (ctr (parse-type #'stx env))]
    [other (syntax-error sym "invalid syntax" stx)]))


(define (parse-tuple-type stx env)
  (define (map-type s) (parse-type s env))
  (syntax-parse stx
    [(Tuple) UNIT-TYPE]
    [(Tuple type ...)
     (define a* (syntax->list #'(type ...)))
     (STuple (length a*) (map map-type a*))]))

(module+ test
  (require rackunit)
  (define-syntax-rule (tpt f s)
    (check-equal? (parse-type f) s))
  (tpt #'Int INT-TYPE)
  (check-equal? (parse-type #'(Int -> Int))
                (Fn 1 `(,INT-TYPE) INT-TYPE))
  (check-equal? (parse-type #'(-> Int))
                (Fn 0 `() INT-TYPE))
  (check-equal? (parse-type #'(Bool -> Bool))
                (Fn 1 `(,BOOL-TYPE) BOOL-TYPE))
  (check-equal? (parse-type #'(Int Bool -> Int))
                (Fn 2 `(,INT-TYPE ,BOOL-TYPE) INT-TYPE))
  (check-equal? (parse-type #'((-> Bool) -> (-> Int)))
                (Fn 1 `(,(Fn 0 '() BOOL-TYPE)) (Fn 0 '() INT-TYPE)))
  (check-exn exn? (lambda () (parse-type #'(-> Int Int))))
  (check-exn exn? (lambda () (parse-type #'(-> (-> Bool Int)))))
   (check-equal? (parse-type #'Unit)
                UNIT-TYPE)
  (check-equal? (parse-type #'(Tuple))
                UNIT-TYPE)
  (check-equal? (parse-type #'(Tuple Int Bool))
                (STuple 2 `(,INT-TYPE ,BOOL-TYPE))))

(define core-parse-prim (core parse-op))

(define top-level-env
  (make-immutable-hash
   `((define     . ,(core parse-define-form))
     (let        . ,(core parse-let-expression))
     (letrec     . ,(core parse-letrec-expression))
     (lambda     . ,(core parse-lambda-expression))
     (if         . ,(core (parse-simple-form 'if If 3)))
     (begin      . ,(core parse-begin))     
     (tuple      . ,(core (parse-simple*-form 'tuple Create-tuple)))
     (tuple-proj . ,(core parse-tuple-proj))
     (repeat     . ,(core parse-repeat))
     (switch     . ,(core parse-switch))
     (Int        . ,(core-atomic INT-TYPE))
     (Bool       . ,(core-atomic BOOL-TYPE))
     (Unit       . ,(core-atomic UNIT-TYPE))
     (Dyn        . ,(core-atomic DYN-TYPE))
     (Float      . ,(core-atomic FLOAT-TYPE))
     (Tuple      . ,(core parse-tuple-type))
     (GRef       . ,(core (make-parse-type-w/ctr 'GRef  GRef)))
     (GVect      . ,(core (make-parse-type-w/ctr 'GVect GVect)))
     (MVect      . ,(core (make-parse-type-w/ctr 'MVect MVect)))
     (MRef       . ,(core (make-parse-type-w/ctr 'MRef  MRef)))
     ,@(let ([parse-gbox
              (core (parse-simple-form 'gbox Gbox 1))]
             [parse-gbox-set!
              (core (parse-simple-form 'gbox-set! Gbox-set! 2))]
             [parse-gunbox
              (core (parse-simple-form 'gunbox Gunbox 1))]
             [parse-gvector
              (core (parse-simple-form 'gvector Gvector 2))]
             [parse-gvector-ref
              (core (parse-simple-form 'gvector-ref Gvector-ref 2))]
             [parse-gvector-set!
              (core (parse-simple-form 'gvector-set! Gvector-set! 3))]
             [parse-mbox
              (core (parse-simple-form 'mbox MboxS 1))]
             [parse-munbox
              (core (parse-simple-form 'munbox Munbox 1))]
             [parse-mbox-set!
              (core (parse-simple-form 'mbox-set! Mbox-set! 2))]
             [parse-mvector
              (core (parse-simple-form 'mvector MvectorS 2))]
             [parse-mvector-ref
              (core (parse-simple-form 'mvector-ref Mvector-ref 2))]
             [parse-mvector-set!
              (core (parse-simple-form 'mvector-set! Mvector-set! 3))])
         `(,@(match (reference-semantics)
               ['Guarded
                `((Ref        . ,(core (make-parse-type-w/ctr 'GRef  GRef)))
                  (Vector     . ,(core (make-parse-type-w/ctr 'GVect GVect)))
                  (vector       . ,parse-gvector)
                  (vector-ref   . ,parse-gvector-ref)
                  (vector-set!  . ,parse-gvector-set!)
                  (box          . ,parse-gbox)
                  (box-set!     . ,parse-gbox-set!)
                  (unbox        . ,parse-gunbox))]
               ['Monotonic
                `((Ref        . ,(core (make-parse-type-w/ctr 'MRef  MRef)))
                  (Vector     . ,(core (make-parse-type-w/ctr 'MVect MVect)))
                  (vector       . ,parse-mvector)
                  (vector-ref   . ,parse-mvector-ref)
                  (vector-set!  . ,parse-mvector-set!)
                  (box          . ,parse-mbox)
                  (box-set!     . ,parse-mbox-set!)
                  (unbox        . ,parse-munbox))]
               [other (error 'syntax-to-schml0/top-level-env "unmatched: ~a" other)])
           (gvector      . ,parse-gvector)
           (gvector-ref  . ,parse-gvector-ref)
           (gvector-set! . ,parse-gvector-set!)
           (gbox         . ,parse-gbox)
           (gbox-set!    . ,parse-gbox-set!)
           (gunbox       . ,parse-gunbox)
           (mvector      . ,parse-mvector)
           (mvector-ref  . ,parse-mvector-ref)
           (mvector-set! . ,parse-mvector-set!)
           (mbox         . ,parse-mbox)
           (mbox-set!    . ,parse-mbox-set!)
           (munbox       . ,parse-munbox)))
     (ann . ,(core parse-ascription))
     (: . ,(core parse-ascription))
     ;; Fixnum operations
     (* . ,core-parse-prim)
     (+ . ,core-parse-prim)
     (- . ,core-parse-prim)
     (%/ . ,core-parse-prim)
     (%% . ,core-parse-prim)
     (%>> . ,core-parse-prim)
     (%<< . ,core-parse-prim)
     (binary-and . ,core-parse-prim)
     (binary-or  . ,core-parse-prim)
     (binary-xor . ,core-parse-prim)
     (read-int . ,core-parse-prim)
     (<  . ,core-parse-prim)
     (<= . ,core-parse-prim)
     (=  . ,core-parse-prim)
     (>  . ,core-parse-prim)
     (>= . ,core-parse-prim)
     ;; Float operations
     (fl+   . ,core-parse-prim)
     (fl-   . ,core-parse-prim)
     (fl*   . ,core-parse-prim)
     (fl/   . ,core-parse-prim)
     (flmodulo . ,core-parse-prim)
     (flabs . ,core-parse-prim)
     (fl<   . ,core-parse-prim)
     (fl<=  . ,core-parse-prim)
     (fl=   . ,core-parse-prim)
     (fl>=  . ,core-parse-prim)
     (fl>   . ,core-parse-prim)
     (flmin . ,core-parse-prim)
     (flmax . ,core-parse-prim)
     (flround . ,core-parse-prim)
     (flfloor . ,core-parse-prim)
     (flceiling . ,core-parse-prim)
     (fltruncate . ,core-parse-prim)
     ;; Float operations (trig)
     (flsin . ,core-parse-prim)
     (flcos . ,core-parse-prim)
     (fltan . ,core-parse-prim)
     (flasin . ,core-parse-prim)
     (flacos . ,core-parse-prim)
     (flatan . ,core-parse-prim)
     ;; Float operations (math)
     (fllog . ,core-parse-prim)
     (flexp . ,core-parse-prim)
     (flsqrt . ,core-parse-prim)
     (flexpt . ,core-parse-prim)
     (float->int . ,core-parse-prim)
     (int->float . ,core-parse-prim)
     (read-float . ,core-parse-prim)
     (time . ,(core parse-time))
     (timer-start . ,core-parse-prim)
     (timer-stop . ,core-parse-prim)
     (timer-report . ,core-parse-prim)
     )))


(module+ test
  (check-forms=? ((parse-form top-level-env) #'(vector 0 2))
                 (Ann (Gvector (Ann (Quote 0) (quote-srcloc))
                               (Ann (Quote 2) (quote-srcloc)))
                      (quote-srcloc))))
