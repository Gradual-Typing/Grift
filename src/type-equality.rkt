#lang typed/racket/base

(require (for-syntax racket/base))

(provide assert-subtype?
         assert-types=?
         assert-pass-types-whole)

(define-syntax (assert-subtype? stx)
  (syntax-case stx ()
    [(_ T1 T2)
     #`(void
        #,(quasisyntax/loc stx
            (lambda ([#,(syntax/loc stx x) : T1])
             : T2
             #,(syntax/loc stx x))))]))

(define-syntax (assert-types=? stx)
  (syntax-case stx ()
    [(_ T1 T2)
     #`(void
        #,(quasisyntax/loc stx
            (lambda ([#,(syntax/loc stx f) : (T1 -> T1)])
              : (T2 -> T2)
              #,(syntax/loc stx f))))]))

(define-syntax (assert-pass-types-whole stx)
  (syntax-case stx ()
    [(_ L1 Sub ∩ Add L2) 
     #`(begin
         #,(syntax/loc stx (assert-types=? L1 (Rec L (U (∩ L) (Sub L)))))
         #,(syntax/loc stx (assert-types=? L2 (Rec L (U (∩ L) (Add L))))))]))

(module+ test
  (require racket/match)
  ;; Types similar to our forms
  (struct (Fmls Body) Lambda ([fmls : Fmls] [body : Body]) #:transparent)
  (struct (Rator Rands) App ([rator : Rator] [rands : Rands]) #:transparent)
  (struct (Bnds Body) Let ([bnds : Bnds] [body : Body]) #:transparent)
  ;; Types similar to our Language Grammars
  (define-type L1
    (U (Lambda (Listof Symbol) L1)
       (App L1 (Listof L1))
       Symbol))
  (define-type L2 
    (U (Lambda (Listof Symbol) L2)
       (Let (Listof (Pair Symbol L2)) L2)
       (App L2 (Listof Symbol))
       Symbol))
  ;; These types document the changes that the pass makes.
  ;; P1 adds these forms
  (define-type (P1+ L)
    (U (Let (Listof (Pair Symbol L)) L)
       (App L (Listof Symbol))))
  ;; P1 removes these forms
  (define-type (P1- E)
    (App L1 (Listof E)))
  ;; This assertion helps show that at least all types are
  ;; accounted for in the tranformation.
  ;; This can be read as, "L1 minus P1- is L1∩L2 and
  ;; L1∩L2 plus P1+ is L2.
  (assert-pass-types-whole L1 P1- L1∩L2 P1+ L2)

  (: p1 : L1 -> L2)
  (define (p1 e)
    (match e
      [(Lambda fml b) (Lambda fml (p1 b))]
      [(App r a*)
       (define-values (bnd* s*)
         (for/lists ([bnd* : (Listof (Pair Symbol L2))]
                     [s*   : (Listof Symbol)])
                    ([a a*])
           (define s (gensym))
           (values (cons s (p1 a)) s)))
       (Let bnd* (App (p1 r) s*))]
      [(? symbol? s) s]))

  ;; This intersection is necesarry because typed-rackets
  ;; type difference operations is woefully incomplete.
  (define-type (L1∩L2 L1)
    (U  (Lambda (Listof Symbol) L1)
        Symbol)))
