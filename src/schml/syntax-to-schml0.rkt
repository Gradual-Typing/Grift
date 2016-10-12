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
   (define e
     (parameterize ([current-unique-counter uc])
       ((parse-top-level name top-level-env)  s*)))
   (debug who (Prog (list name (unique-counter-next! uc)) e))]
  [(other) (error 'syntax->schml0 "unmatched: ~a" other)])

(define (syntax->srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))


;; String (Hashtable Symbol Uid) -> (Listof Syntax) -> Expr
(define ((parse-top-level name env) stx*)
  (match stx*
    [(list) NO-OP]
    [(list s-final) ((parse-expr env) s-final)]
    [(list s* ... s-final)
     (define/match (expr/ann e)
       [((Ann _ s)) (Ann (Expression e) s)]
       [(other) (error 'expression "unmatched: ~a" other)])
     (define e* (map (parse-expr env) s*))
     (define ef ((parse-expr env) s-final))
     (make-begin (map expr/ann e*) ef)]
    [other (error 'parse-top-level "unmatched: ~a" other)]))

(define ((parse-expr env) stx)
  (debug off 'parse-expr stx env)
  (define src (syntax->srcloc stx))
  (define cf
    (syntax-parse stx
      [var:id
       (match (env-lookup env #'var)
         [(lexical-var var) var]
         [other (syntax-error 'parse-expr "needed variable" stx)])]
      [(var:id rest ...)
       (match (env-lookup env #'var)
         [(lexical-var var)
          (define var-src (quote-srcloc #'var))
          (define as (map (parse-expr env) (syntax->list #'(rest ...))))
          (App (Ann var var-src) as)]
         [(core parse) (parse stx env)]
         [other (error 'parse-expr "unmatched: ~a" other)])]
      [(fst rest ...)
       (define p ((parse-expr env) #'fst))
       (define as (map (parse-expr env) (syntax->list #'(rest ...))))
       (App p as)]
      [other
       (define dat (syntax->datum #'other))
       (unless (datum? dat)
         (syntax-error 'parse-expr "expected datum" #'other))
       (Quote dat)]))
  (Ann cf src))

(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (null? x)))

(define-syntax syntax-error
  (syntax-rules ()
    [(_ name message) (error name message)]
    [(_ name message expr)
     (let ([msg (format "~a\n\tin:~a" message expr)])
       (error name msg))]
    [(_ name message expr sub)
     (let ([msg (format "~a\n\tin:~a\n\tin particular: ~a" message expr sub)])
       (error name msg))])) 

(define (schml-core-form? x)
  (or (Lambda? x) (Letrec? x) (Let? x) (Var? x) (App? x)
      (Op? x) (Quote? x) (Ascribe? x)
      (Begin? x) (If? x) (Repeat? x)
      ;; Constructor, Destructor, Mutator should be factored out
      ;; (Ctr 'Gbox (list arg)) (Ctr 'Gvector (list arg1 arg2))
      (Gbox? x) (Gunbox? x) (Gbox-set!? x)
      (Gvector? x) (Gvector-ref? x) (Gvector-set!? x)
      (Mbox? x) (Munbox? x) (Mbox-set!? x)
      (Mvector? x) (Mvector-ref? x) (Mvector-set!? x)))

(struct lexical-var  (symbol))
(struct core (parser))
(struct core-atomic (atom))

(define (env-lookup env x)
  (debug off 'env-lookup env x)
  (define (err)
    (syntax-error 'environment "unbound variable" x))
  (hash-ref env (syntax->datum x) err))

(define (env-extend env x u)
  (hash-set env (syntax->datum x) (lexical-var (Var u))))


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
     (define rhss (map (parse-expr env) (syntax->list #'(bnd.rhs ...))))
     (Let (map Bnd uids tys rhss)
       ((parse-expr new-env) #'body))]))

(define (parse-letrec-expression stx env)
  (syntax-parse stx
    [(_ (bnd:binding ...) body)
     (define-values (new-env uids)
       (destruct-and-parse-bnds-lhs (syntax->list #'(bnd.var ...)) env))
     (define tys  (map (parse-optional-type env) (syntax->list #'(bnd.ty  ...))))
     (define rhss (map (parse-expr new-env) (syntax->list #'(bnd.rhs ...))))
     (Letrec (map Bnd uids tys rhss)
       ((parse-expr new-env) #'body))]))

(module+ test
  (require "../language/forms-equal.rkt")
  (current-unique-counter (make-unique-counter 0))
  (check-forms=?
   ((parse-expr top-level-env) #'(let ([x 1]) x))
   (Ann (Let (list (Bnd 'x #f (Ann (Quote 1) (quote-srcloc))))
          (Ann (Var 'x) (quote-srcloc)))
        (quote-srcloc)))
  (check-forms=?
   ((parse-expr top-level-env) #'(letrec ([x x]) x))
   (Ann (Letrec (list (Bnd 'x #f (Ann (Var 'x) (quote-srcloc))))
          (Ann (Var 'x) (quote-srcloc)))
        (quote-srcloc))))

(define-syntax-class formal-parameter
  [pattern [var:id (~datum :) ty]]
  [pattern var:id #:with ty #'Dyn])

(define (parse-lambda-expression stx env)
  (syntax-parse stx
    [(_ (fml:formal-parameter ...)
        (~optional (~seq (~datum :) ty) #:defaults ([ty #'#f]))
        body)
     (define-values (new-env uids)
       (destruct-and-parse-bnds-lhs (syntax->list #'(fml.var ...)) env))
     (define (pt s) (parse-type s env))
     (define tys  (map pt (syntax->list #'(fml.ty  ...))))
     (Lambda (map Fml uids tys)
       (Ann ((parse-expr new-env) #'body)
            ((parse-optional-type new-env) #'ty)))]))

(module+ test
  (check-forms=?
   ((parse-expr top-level-env) #'(lambda (x y) x))
   (Ann (Lambda (list (Fml 'x (Dyn)) (Fml 'y (Dyn)))
          (Ann (Ann (Var 'x) (quote-srcloc)) #f))
        (quote-srcloc)))
  (check-forms=?
   ((parse-expr top-level-env) #'(lambda ([x : Int]) x))
   (Ann (Lambda (list (Fml 'x (Int)))
          (Ann (Ann (Var 'x) (quote-srcloc)) #f))
        (quote-srcloc)))
  (check-forms=?
   ((parse-expr top-level-env) #'(lambda ([x : Int]) : Int x))
   (Ann (Lambda (list (Fml 'x (Int)))
          (Ann (Ann (Var 'x) (quote-srcloc)) (Int)))
        (quote-srcloc))))

(define (parse-op stx env)
  (syntax-parse stx
    [(op rands ...)
     (define o (syntax->datum #'op))
     (define r* (syntax->list #'(rands ...)))
     (Op o (map (parse-expr env) r*))]))

(define (parse-ascription stx env)
  (syntax-parse stx
    [(_ expr ty (~optional (~seq l:str) #:defaults ([l #'#f])))
     (define lbl (syntax->datum (attribute l)))
     (unless (or (not lbl) (string? lbl))
       (error 'parse-ascription "this doesn't work like I thought it would: ~a ~a" lbl #'l))
     (Ascribe ((parse-expr env) #'expr) (parse-type #'ty env) lbl)]))

(module+ test
  (check-forms=? (parse-ascription #'(ann 3 Int) top-level-env)
                 (Ascribe (Ann (Quote 3) (quote-srcloc)) (Int) #f))
  
  (check-forms=? (parse-ascription #'(ann #f Int "Bad") top-level-env)
                 (Ascribe (Ann (Quote #f) (quote-srcloc)) (Int)  "Bad")))

(define (parse-begin stx env)
  (syntax-parse stx
    [(_ expr* ... expr)
     (Begin (map (parse-expr env) (syntax->list #'(expr* ...)))
            ((parse-expr env) #'expr))]))

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
     (define recur (parse-expr env))
     (Repeat i (recur #'i.start) (recur #'i.stop)
       (Ann a ((parse-optional-type env) #'a.type))
       ((parse-expr env) #'a.init)
       ((parse-expr new-env) #'M))]))

(module+ test
  (parse-repeat #'(repeat (foo 0 10) (bar : Int 0) foo) top-level-env))

(define (parse-switch stx env)
  (define-syntax-class switch-clause
    [pattern [(case*:integer ...+) rhs]])
  (define recur (parse-expr env))
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
     (apply ctr (map (parse-expr env) p*))]))

(define ((parse-simple*-form sym ctr) stx env)
  (syntax-parse stx
    #:context sym
    [(_ params ...)
     #:do [(define p* (syntax->list #'(params ...)))]
     (ctr (map (parse-expr env) p*))]))

(define (parse-tuple-proj stx env)
  (syntax-parse stx
    [(_ t i:integer)
     (Tuple-proj ((parse-expr env) #'t) (syntax->datum #'i))]))


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
            (Let (list (Bnd tmp #f ((parse-expr env) #'exp)))
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
       [(lexical-var id) id]
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
   `((let    . ,(core parse-let-expression))
     (letrec . ,(core parse-letrec-expression))
     (lambda . ,(core parse-lambda-expression))
     (if     . ,(core (parse-simple-form 'if If 3)))
     (begin  . ,(core parse-begin))
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
     (tuple     . ,(core (parse-simple*-form 'tuple Create-tuple)))
     (tuple-proj . ,(core parse-tuple-proj))
     (repeat . ,(core parse-repeat))
     (switch . ,(core parse-switch))
     (Int    . ,(core-atomic INT-TYPE))
     (Bool   . ,(core-atomic BOOL-TYPE))
     (Unit   . ,(core-atomic UNIT-TYPE))
     (Dyn    . ,(core-atomic DYN-TYPE))
     (Tuple . ,(core parse-tuple-type))
     (Ref   . ,(core (make-parse-type-w/ctr 'GRef  GRef)))
     (Vect  . ,(core (make-parse-type-w/ctr 'GVect GVect)))
     (GRef   . ,(core (make-parse-type-w/ctr 'GRef  GRef)))
     (GVect  . ,(core (make-parse-type-w/ctr 'GVect GVect)))
     (MVect  . ,(core (make-parse-type-w/ctr 'MVect MVect)))
     (MRef   . ,(core (make-parse-type-w/ctr 'MRef  MRef)))
     ,@(let ([parse-gbox      (parse-simple-form 'gbox Gbox 1)]
             [parse-gbox-set! (parse-simple-form 'gbox-set! Gbox-set! 2)]
             [parse-gunbox    (parse-simple-form 'gunbox Gunbox 1)]
             [parse-gvector
              (parse-simple-form 'gvector Gvector 2)]
             [parse-gvector-ref
              (parse-simple-form 'gvector-ref Gvector-ref 2)]
             [parse-gvector-set!
              (parse-simple-form 'gvector-set! Gvector-set! 3)])
         `((gvector   . ,(core parse-gvector))
           (gvector-ref . ,(core parse-gvector-ref))
           (gvector-set! . ,(core parse-gvector-set!))
           (vector   . ,(core parse-gvector))
           (vector-ref . ,(core parse-gvector-ref))
           (vector-set! . ,(core parse-gvector-set!))
           (gbox      . ,(core parse-gbox))
           (gbox-set! . ,(core parse-gbox-set!))
           (gunbox    . ,(core parse-gunbox))
           (box       . ,(core parse-gbox))
           (box-set!  . ,(core parse-gbox-set!))
           (unbox    . ,(core parse-gunbox))))
     (mbox . , (core (parse-simple-form 'mbox MboxS 1)))
     (munbox . ,(core (parse-simple-form 'munbox Munbox 1)))
     (mbox-set! . ,(core (parse-simple-form 'mbox-set! Mbox-set! 2)))
     (mvector   . ,(core (parse-simple-form 'mvector Mvector 2)))
     (mvector-ref . ,(core (parse-simple-form 'mvector-ref Mvector-ref 2)))
     (mvector-set! . ,(parse-simple-form 'mvector-set! Mvector-set! 3))
     (ann . ,(core parse-ascription))
     (: . ,(core parse-ascription))
     (time . ,(core parse-time))
     (timer-start . ,core-parse-prim)
     (timer-stop . ,core-parse-prim)
     (timer-report . ,core-parse-prim)
     )))


(module+ test
  (check-forms=? ((parse-expr top-level-env) #'(vector 0 2))
                 (Ann (Gvector (Ann (Quote 0) (quote-srcloc))
                               (Ann (Quote 2) (quote-srcloc)))
                      (quote-srcloc))))
