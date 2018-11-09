#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/label-lambdas                                             |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: Moves every lambda into a letrec thus giving it a
| binding. With the new closure conversion steps this pass is now uneeded
| and thus needs to be removed unfortunately it is not entirely evident how to
| do so and I need to make some progress thus letrec-refinement and lambda labeling
| is occuring here. This should be changed. One thought is to perform letrec
| purification earlier. In general I don't like this because letrec is not
| fundemental to the system but is being forced into the center stage. Perhaps
| enforcing the use of letrec only on functions is enough to eliminate this pass
|
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
(require
 typed/racket/unsafe
 "../configuration.rkt"
 "../errors.rkt"
 "../helpers.rkt"
 "../language/forms.rkt"
 "../language/cast-or-coerce3.2.rkt"
 "../language/cast-or-coerce4.rkt"
 "../unique-counter.rkt"
 "../unique-identifiers.rkt"
 "../type-equality.rkt")

(unsafe-require/typed
 "../language/form-map.rkt"
 [form-map ((Label-Lambdas= CoC3.2-Expr)
            (CoC3.2-Expr -> CoC4-Expr)
            -> (Label-Lambdas= CoC4-Expr))])

(provide
 label-lambdas
 (all-from-out
  "../language/cast-or-coerce3.2.rkt"
  "../language/cast-or-coerce4.rkt"))

;; This pass removes Lambdas from expression contexts
(define-type (Label-Lambdas- E)
  (Castable-Lambda E))

;; And leaves the rest expression forms alone.
(define-type (Label-Lambdas= E)
  (U (Named-Castable-Lambda-Forms E)
     (App-Fn E (Listof E))
     (Fn-Proxy-Forms E)
     (Let (Bnd* E) E)
     (Var Uid)
     (Global String)
     (Assign Id E)
     (Gen-Data-Forms E)
     (Code-Forms E)
     (Quote-Coercion Immediate-Coercion)
     (Coercion-Operation-Forms E)
     (Hyper-Coercion-Operation-Forms E)
     (Type Immediate-Type)
     (Type-Operation-Forms E)
     (Control-Flow-Forms E)
     (Op Grift-Primitive (Listof E))
     (Quote Cast-Literal)
     No-Op
     (Blame E)
     (Observe E Grift-Type)
     (Unguarded-Forms E)
     (Guarded-Proxy-Forms E)
     (Monotonic-Forms E Immediate-Type)
     (Error E)
     (Tuple-Operation-Forms E)))

;; Show that the invariant language forms equivalent to the target
;; language.
(assert-subtype? (Label-Lambdas= CoC4-Expr) CoC4-Expr)
(assert-subtype? CoC4-Expr (Label-Lambdas= CoC4-Expr))

;; Show that src language is equivalent to language forms removed
;; plus the invariant language forms.
(assert-subtype? (U (Label-Lambdas- CoC3.2-Expr)
                    (Label-Lambdas= CoC3.2-Expr))
                 CoC3.2-Expr)
(assert-subtype? CoC3.2-Expr
                 (U (Label-Lambdas- CoC3.2-Expr)
                    (Label-Lambdas= CoC3.2-Expr)))

(: label-lambdas (Cast-or-Coerce3.2-Lang  -> Cast-or-Coerce4-Lang))
(define (label-lambdas prgm)
  (match-define (Prog (list n c t)
                  (Static*
                   (list mtb* tb* mcb* cb* constb*)
                   e))
    prgm)
  (define uc (make-unique-counter c))
  (: new-exp CoC4-Expr)
  (: new-constb* CoC4-Bnd-Data*)
  (define-values (new-exp new-constb*)
    (parameterize ([current-unique-counter uc])
      (values
       (ll-expr e)
       (for/list ([b constb*])
         (match-define (cons id expr) b)
         (cons id (ll-expr expr))))))
  (Prog (list n (unique-counter-next! uc) t)
    (Static*
     (list mtb* tb* mcb* cb* new-constb*)
     new-exp)))

(: ll-expr (CoC3.2-Expr -> CoC4-Expr))
(define (ll-expr expr)
  (cond
    [(Lambda? expr)
     (match-define (Lambda f* (Castable ctr e)) expr)
     (let ([id (next-uid! "annon")])
       (Letrec `((,id . ,(Lambda f* (Castable ctr (ll-expr e)))))
         (Var id)))]
    ;; While seemingly innocent this letrec case is important
    ;; to avoid naming lambadas that are already the right hand
    ;; side of a letrec binding. 
    [(Letrec? expr)
     (match-define (Letrec b*0 e) expr)
     (define b*1 : (Bnd* (Castable-Lambda CoC4-Expr))
       (for/list ([bnd b*0])
         (match-define (cons i (Lambda p* (Castable c b))) bnd)
         (cons i (Lambda p* (Castable c (ll-expr b))))))
     (Letrec b*1 (ll-expr e))]
    [(Let? expr)
     (define-values (bp* bd*)
       ;; split binding into procedures (bd*) and data (bd*)
       ;; and recur on substructure of rhs
       (for/fold ([bp* : CoC4-Bnd-Lambda* '()]
                  [bd* : CoC4-Bnd-Data*   '()])
                 ([b   : CoC3.2-Bnd (Let-bindings expr)])
         (match b
           [(cons i (Lambda f* (Castable b e)))
            (define bp (cons i (Lambda f* (Castable b (ll-expr e)))))
            (values (cons bp bp*) bd*)]
           [(cons i e)
            (define bd (cons i (ll-expr e)))
            (values bp* (cons bd bd*))])))
     (define e (ll-expr (Let-body expr)))
     (cond
       [(and (null? bp*) (null? bd*)) e]
       [(null? bp*) (Let bd* e)]
       [(null? bd*) (Letrec bp* e)]
       [else (Let bd* (Letrec bp* e))])]
    [else (form-map expr ll-expr)]))

(module+ test
  (require typed/rackunit)
  (define e1 : CoC3.2-Expr
    (Lambda '() (Castable #f (Quote 1))))
  (define r1 : CoC4-Expr
    (Letrec (list (cons (Uid "annon" 0) (Lambda '() (Castable #f (Quote 1)))))
      (Var (Uid "annon" 0))))
  (parameterize ([current-unique-counter (make-unique-counter 0)])
    (check-equal? (ll-expr e1) r1))
  ;; Note because form-map is actually an type-agnostic map of forms
  ;; we need to be careful to make that letrec doesn't get mapped
  ;; this test should fail if the letrec case is removed.
  (define e2 : CoC3.2-Expr
    (Letrec (list (cons (Uid "id" 0)
                        (Lambda (list (Uid "x" 1))
                          (Castable (Uid "fn-caster-1" 3)
                                    (Var (Uid "x" 1))))))
      (Var (Uid "id" 0))))
  (parameterize ([current-unique-counter (make-unique-counter 0)])
    (check-equal? (ll-expr e2) e2)))
