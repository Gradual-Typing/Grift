#lang typed/racket

;; While many of these syntaxes seem absurd they
;; provide a way to incrementally change the syntax
;; used in the compiler and to write object code,
;; when it isn't particularly pleasant to manually
;; construct the AST.


(require (for-syntax racket/syntax)
         "forms.rkt"
         "../configuration.rkt"
         syntax/location)
(require/typed racket/base
  [srcloc->string (srcloc -> (Option String))])
(provide (all-defined-out))

(define-syntax (track-next-uid!$ stx)
  (syntax-case stx ()
    [(_ name)
     (let-values ([(_1 path _2) (split-path (syntax-source #'name))])
       (with-syntax ([fmt-name (format "~a_~a_~a_~a"
                                       (syntax->datum #'name)
                                       path
                                       (syntax-line #'name)
                                       (syntax-column #'name))])
         #'(if (emit-vars-with-original-source-location?)
               (next-uid! fmt-name)
               (next-uid! 'name))))]))

(define-syntax (define-track-next-uid!$ stx)
  (syntax-case stx ()
    [(_ name) #'(define name (track-next-uid!$ name))]))

(define-syntax (begin$ stx)
  (syntax-case stx (assign$)
    [(_ b) #'b]
    [(_ (assign$ u e) b* ... b)
     #'(let ([t (track-next-uid!$ u)])
         (Begin
           (list (Assign t e))
           (let ([u (Var t)])
             (begin$ b* ... b))))]
    [(_ b* ... b) #'(Begin `(,b* ...) b)]))

(define-syntax (let$ stx)
  (syntax-case stx ()
    [(_ ([i* e*] ...) b* ... b)
     (with-syntax ([(u* ...) (generate-temporaries #'(i* ...))])
       #'(let ([u* (track-next-uid!$ i*)] ...)
           (Let `([,u* . ,e*] ...)
             (let ([i* (Var u*)] ...)
               (begin$ b* ... b)))))]))

(define-syntax let*$
  (syntax-rules ()
    [(_ () b* ... b) (begin$ b* ... b)]
    [(_ ([i0 e0] [i* e*] ...) b* ... b)
     (let$ ([i0 e0]) (let*$ ([i* e*] ...) b* ... b))]))

(define-syntax (bind-value$ stx)
  (syntax-case stx ()
    [(_ ([i* e*] ...) b* ... b)
     (with-syntax ([(t* ...) (generate-temporaries #'(i* ...))]
                   [(bnd?* ...) (generate-temporaries #'(i* ...))]
                   [(val?* ...) (generate-temporaries #'(i* ...))])
       #'(let ([t* e*] ...)
           (define-values (bnd* val*)
             (let-values ([(bnd?* val?*)
                           (match t*
                             [(or (Var _) (Type _) (Quote _) (Quote-Coercion _))
                              (values '() `(,t*))]
                             [_
                              (define u (track-next-uid!$ i*))
                              (values `([,u . ,t*]) `(,(Var u)))])]
                          ...)
               (values (append bnd?* ...) (append val?* ...))))
           (let*-values ([(i* val*) (values (car val*) (cdr val*))] ...)
             (define f (lambda () (begin$ b* ... b)))
             (cond
               [(null? bnd*) (f)]
               [else (Let bnd* (f))]))))]))


;; The above macro could be really bad for type checking times in some
;; cases this version might aliviate the pain in those cases.
(define-syntax (define-local-syntax-bind-expr$ stx)
  (syntax-case stx ()
    [(_ type)
     (with-syntax ([bind-expr$ (datum->syntax stx 'bind-expr$)])
       #'(... ;; All elipsis inside are ellipsis literals
          (
           (: bind-expr* : (Listof Symbol) (Listof type) ->
              (Values (Listof (cons Uid type)) (Listof (U (Var Uid) type))))
           (define (bind-expr* s* e*)
             (match* (s* e*)
               [('() '()) (values '() '())]
               [((cons s s*) (cons e e*))
                (define-values (b* v*) (bind-expr* s* e*))
                (match e
                  [(or (Var _) (Quote _) (Type _) (Quote-Coercion _))
                   (values b* (cons e v*))]
                  [_
                   (define u (track-next-uid!$ s))
                   (values `([,u . ,e] . ,b*) (cons (Var u) v*))])]))
           (define-syntax (bind-expr$ stx)
             (syntax-case stx ()
               [(_ ([i* e*] ...) b* ... b)
                #'(let-values ([(bnd* val*) (bind-expr* '(i* ...) `(,e* ...))])
                    (let*-values ([(i* val*) (values (car val*) (cdr val*))] ...)
                      (let ([f (lambda () (begin$ b* ... b))])
                        (cond
                          [(null? bnd*) (f)]
                          [else (Let bnd* (f))]))))])))))]))


(define-syntax (code$ stx)
  (syntax-case stx ()
    [(_ (p* ...) b* ... b)
     (with-syntax ([(u* ...) (generate-temporaries #'(p* ...))])
       #'(let ([u* (track-next-uid!$ p*)] ...)
           (Code `(,u* ...)
             (let ([p* (Var u*)] ...)
               (begin$ b* ... b)))))]))

(define-syntax (repeat$ stx)
  (syntax-case stx (:)
    [(_ (i e1 e2) () b* ... b) #'(repeat$ (i e1 e2) (_ (Quote '())) b* ... b)]
    [(_ (i e1 e2) (a e3) b* ... b)
     #'(let ([iu (track-next-uid!$ i)] [au (track-next-uid!$ a)])
         (Repeat iu e1 e2 au e3
           (let ([i (Var iu)] [a (Var au)])
             (begin$ b* ... b))))]))

(define-syntax cond$
  (syntax-rules (else)
    [(_ [else ee* ... ee]) (begin$ ee* ... ee)]
    [(_ [c e* ... e] [c* e** ...] ... [else ee* ... ee])
     (If c
         (begin$ e* ... e)
         (cond$
          [c* e** ...]
          ...
          [else ee* ... ee]))]))

(define-syntax-rule (case$ v [(p** ...) e* ... e] ... [else d* ... d])
  (Switch v `([(,p** ...) . ,(begin$ e* ... e)] ...) (begin$ d* ... d)))

;; 
#;
(define-syntax and$
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...) (Op 'and (list e1 (and$ e* ...)))]))

#;
(define-syntax or$
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...) (Op 'or (list e1 (or$ e* ...)))]))

(define-syntax and$
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...) (If e1 (and$ e* ...) (Quote #f))]))

(define-syntax or$
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...) (If e1 (Quote #t) (or$ e* ...))]))


(define-syntax-rule (op$ o a ...) (Op 'o (list a ...)))
(define-syntax-rule (app-code$ cl a ...) (App-Code cl (list a ...)))
(define-syntax-rule (not$ x) (op$ not x))
(define-syntax-rule (op=? e1 e2) (op$ = e1 e2))
(define-syntax-rule (op<=? e1 e2) (op$ <= e1 e2))
(define-syntax-rule (op>=? e1 e2) (op$ >= e1 e2))

(define-syntax (precondition$ stx)
  (syntax-case stx ()
    [(_ c b* ... b)
     #'(let ([e (begin$ b* ... b)])
         (if (check-asserts?)
             (Begin
               (list
                (If c
                    (Quote 0)
                    (Blame (Quote (format "runtime precondition failed: ~a" (srcloc->string (quote-srcloc #,stx)))))))
               e)
             e))]))


;; Type-Repr syntax
;; Change these to dyn type
;; and pbox type of
;; so that they read better once I am done
(define-syntax-rule (type-dyn?$ t) (Type-Dyn-Huh t))
(define-syntax-rule (type-tup?$ t) (Type-Tuple-Huh t))
(define-syntax-rule (type-tup-arity$ t) (Type-Tuple-num t))
(define-syntax-rule (type-fn?$ t) (Type-Fn-Huh t))
(define-syntax-rule (type-fn-arity$ t) (Type-Fn-arity t))
(define-syntax-rule (type-pbox?$ t) (Type-GRef-Huh t))
(define-syntax-rule (type-pbox-of$ t) (Type-GRef-Of t))
(define-syntax-rule (type-pvec?$ t) (Type-GVect-Huh t))
(define-syntax-rule (type-pvec-of$ t) (Type-GVect-Of t))
(define-syntax-rule (type-mbox?$ t) (Type-MRef-Huh t))
(define-syntax-rule (type-mbox-of$ t) (Type-MRef-Of t))
(define-syntax-rule (type-mvec?$ t) (Type-MVect-Huh t))
(define-syntax-rule (type-mvec-of$ t) (Type-MVect-Of t))


(define-syntax-rule (id-coercion?$ c) (Id-Coercion-Huh c))
(define-syntax-rule (seq-coercion$ c1 c2) (Sequence-Coercion c1 c2))
(define-syntax-rule (seq-coercion?$ c) (Sequence-Coercion-Huh c))
(define-syntax-rule (seq-coercion-fst$ c) (Sequence-Coercion-Fst c))
(define-syntax-rule (seq-coercion-snd$ c) (Sequence-Coercion-Snd c))
(define-syntax-rule (prj-coercion$ t l) (Project-Coercion t l))
(define-syntax-rule (prj-coercion?$ c) (Project-Coercion-Huh c))
(define-syntax-rule (prj-coercion-type$ c) (Project-Coercion-Type c))
(define-syntax-rule (prj-coercion-label$ c) (Project-Coercion-Label c))
(define-syntax-rule (failed-coercion?$ c) (Failed-Coercion-Huh c))
(define-syntax-rule (failed-coercion-label$ c) (Failed-Coercion-Label c))
(define-syntax-rule (inj-coercion$ t) (Inject-Coercion t))
(define-syntax-rule (inj-coercion?$ c) (Inject-Coercion-Huh c))
(define-syntax-rule (inj-coercion-type$ c) (Inject-Coercion-Type c))
(define-syntax-rule (med-coercion?$ c) (Mediating-Coercion-Huh c))
(define-syntax-rule (tup-coercion?$ c) (Tuple-Coercion-Huh c))
(define-syntax-rule (tup-coercion-arity$ c) (Tuple-Coercion-Num c))
(define-syntax-rule (ref-coercion?$ c) (Ref-Coercion-Huh c))
(define-syntax-rule (ref-coercion$ c1 c2) (Ref-Coercion c1 c2))
(define-syntax-rule (ref-coercion-read$ c) (Ref-Coercion-Read c))
(define-syntax-rule (ref-coercion-write$ c) (Ref-Coercion-Write c))
(define-syntax-rule (fn-coercion?$ c) (Fn-Coercion-Huh c))
(define-syntax-rule (fn-coercion-arity$ c) (Fn-Coercion-Arity c))
(define-syntax-rule (mbox-coercion$ t) (MRef-Coercion t))
(define-syntax-rule (mbox-coercion?$ c) (MRef-Coercion c))
(define-syntax-rule (mbox-coercion-type$ c) (MRef-Coercion-Type c))
(define-syntax-rule (mvec-coercion$ t) (MVect-Coercion t))
(define-syntax-rule (mvec-coercion?$ c) (MVect-Coercion c))
(define-syntax-rule (mvec-coercion-type$ c) (MVect-Coercion-Type c))
