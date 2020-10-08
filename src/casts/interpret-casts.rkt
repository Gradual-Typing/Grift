#lang typed/racket/base/no-check

(require
 racket/match
 "../configuration.rkt"
 "../logging.rkt"
 "../language/forms.rkt"
 "../unique-identifiers.rkt"
 "./interpret-casts-common.rkt"
 "./interpret-casts-with-type-based-casts.rkt"
 "./interpret-casts-with-coercions.rkt"
 "./interpret-casts-with-hyper-coercions.rkt"
 "./interpret-casts-with-error.rkt"
 "./coercion-passing.rkt")

(provide
 interpret-casts)

(: interpret-casts : Cast0.5-Lang -> Cast-or-Coerce3-Lang)
(define (interpret-casts prgm #:debug? [debug? #f])
  ;; The debug flag suppresses the addition of runtime code
  ;; for the purposes of testing specialization.
  (match-define (Prog (list name next type) e) prgm)
  ;; All the implicit state of this program
  (define next-unique (make-unique-counter next))
  
  (parameterize ([cast-runtime-code-bindings '()]
                 [cast-runtime-constant-bindings '()]
                 [current-unique-counter next-unique]
                 [types-greatest-lower-bound-code-label? #f]
                 [mref-state-reduction-uid? #f]
                 [mvect-state-reduction-uid? #f])

    (define ic-expr! : (C0-Expr -> CoC3-Expr)
      (case (cast-representation)
        [(|Type-Based Casts|) (interpret-casts/type-based-casts)]
        [(Coercions)
         (cond
           [(eq? (enable-tail-coercion-composition?) 'tsuda)
            (define-values (icc apply-coercion-uid compose-coercions-uid)
              (interpret-casts/coercions))
            (define cps
              (coercion-passing-trans
               #:apply-coercion-uid apply-coercion-uid
               #:compose-coercions-uid compose-coercions-uid))
            (lambda (e)
              (parameterize ([specialize-cast-code-generation? #f])
                (cps (icc e))))]
          [else
           (define-values (icc apply-coercion-uid compose-coercions-uid)
             (interpret-casts/coercions))
           icc])]
        [(Hyper-Coercions) (interpret-casts/hyper-coercions)]
        [(Static) (interpret-casts/error)]
        [else (error 'grift/interpret-casts
                     "unexpected cast representation: ~a"
                     (cast-representation))]))

    (define new-e
      (cond [(enable-tail-coercion-composition?)
             ;; avoid removing id coercions around injection/projection:
             (parameterize ([optimize-first-order-coercions? #f])
               (ic-expr! e))]
            [else (ic-expr! e)]))
    (debug e new-e)
    
    (define rt-bindings  (cast-runtime-code-bindings))
    (define rt-constants : (Option CoC3-Bnd*)
      (cast-runtime-constant-bindings))
    (for ([bnd rt-bindings])
      (debug bnd))
    
    ;; This is needed to convince the typechecker that parameter
    ;; returned non-false
    (unless (and rt-bindings rt-constants)
      ;; This should never happen because in the dynamic extent of the
      ;; above parameterize the cast-runtime-code-bindings parameter
      ;; should be of type CoC3-Code-Bnd*
      (error 'interpret-cast "very unexpected error"))
    
    ;; Reconstruct the entire program
    (Prog (list name (unique-counter-next! next-unique) type)
      (Static*
       (if debug? '() (list rt-constants))
       (Labels
        (if debug? '() rt-bindings)
        new-e)))))

(module+ test
  (require
   rackunit
   "../language/forms-equal.rkt")
 
  (define (ic e)
    (Labels-body
     (Static*-body
      (Prog-expression
       (interpret-casts
        (Prog (list "" 0 (Dyn)) e)
        #:debug? #t)))))

  (define (ic-old e)
    (parameterize
        ([specialize-cast-code-generation? #t]
         [optimize-first-order-coercions? #t]
         [constant-fold-coercions? #f]
         [apply-coercions-at-first-tail-cast? #f]
         [enable-tail-coercion-composition? #f])
      (ic e)))
  
  (define (ic-tsuda e)
    (parameterize
        ([specialize-cast-code-generation? #f]
         [optimize-first-order-coercions? #f]
         [enable-tail-coercion-composition? 'tsuda])
      (ic e)))

  (define (ic-andre-emulate-old e)
    (parameterize
        ([specialize-cast-code-generation? #t]
         [optimize-first-order-coercions? #t]
         [constant-fold-coercions? #f]
         [apply-coercions-at-first-tail-cast? #f]
         [enable-tail-coercion-composition? 'false])
      (ic e)))

  (define-simple-check (check-andre-emulates-old e)
    (forms=?
     (ic-andre-emulate-old e)
     (ic-old e)))
  
  (define (ic-andre-emulate-tsuda e)
    (parameterize
        ([specialize-cast-code-generation? #f]
         [optimize-first-order-coercions? #f]
         [constant-fold-coercions? #f]
         [apply-coercions-at-first-tail-cast? #f]
         [enable-tail-coercion-composition? 'andre])
      (ic e)))

  (define (ic-andre-cf e)
    (parameterize
        ([specialize-cast-code-generation? #t]
         [optimize-first-order-coercions? #f]
         [constant-fold-coercions? #t]
         [apply-coercions-at-first-tail-cast? #f]
         [enable-tail-coercion-composition? 'andre])
      (ic e)))

  (define (ic-andre e)
    (parameterize
        ([specialize-cast-code-generation? #t]
         ;; optimize-first-order is effectively done
         ;; with the combination of specialize-cast and entail-tail-coercion... = andre
         [optimize-first-order-coercions? #f]
         [constant-fold-coercions? #t]
         [apply-coercions-at-first-tail-cast? #t]
         [enable-tail-coercion-composition? 'andre])
      (ic e)))

  ;; These need to be manually confirmed to discrepancies in let
  ;; binding handling
  (check-andre-emulates-old
   (Cast (Quote 1) (Twosome (Int) (Dyn) "1")))
  (check-forms=?
   (ic-andre-emulate-old
    (Cast (Quote 1) (Twosome (Int) (Dyn) "1")))
   (Dyn-Object (Quote 1) (Type (Int))))
  (check-forms=?
   (ic-tsuda (Cast (Quote 1) (Twosome (Int) (Dyn) "1")))
   (App-Code
    (Code-Label (Uid "apply-coercion" 159))
    (list
     (Quote 1)
     (Quote-Coercion (Sequence (Identity) (Inject (Int))))
     (Quote #t))))
  ;; effectively the same code code from above
  (check-forms=?
   (ic-andre-emulate-tsuda
    (Cast (Quote 1) (Twosome (Int) (Dyn) "1")))
   (Let
    `((c . ,(Quote-Coercion (Sequence (Identity) (Inject (Int))))))
    (App-Code
     (Code-Label (Uid "apply-coercion" 159))
     (list (Quote 1) (Var 'c) (Quote #t)))))
  (check-forms=?
   (ic-andre-cf
    (Cast (Quote 1) (Twosome (Int) (Dyn) "1")))
   (Dyn-Object (Quote 1) (Type (Int))))
  (check-forms=?
   (ic-andre
    (Cast (Quote 1) (Twosome (Int) (Dyn) "1")))
   (Dyn-Object (Quote 1) (Type (Int))))
  
  (check-andre-emulates-old
   (Cast
    (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))
    (Twosome (Dyn) (Int) "2")))
  (check-forms=?
   (ic-andre-emulate-old
    (Cast
     (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))
     (Twosome (Dyn) (Int) "2")))
   (Let
    (list (cons (Uid "v" 437) (Dyn-Object (Quote 1) (Type (Int)))))
    ;; Try to quick project the value then fall back to runtime project procedure
    (If (Dyn-Immediate-Tag=Huh (Var (Uid "v" 437)) (Type (Int)))
        (Dyn-Immediate-Value (Var (Uid "v" 437)))
        (App-Code (Code-Label (Uid "project" 241))
                  (list (Var (Uid "v" 437)) (Type (Int)) (Quote "2") (Quote #t))))))
  (check-forms=?
   (ic-tsuda
    (Cast (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))
          (Twosome (Dyn) (Int) "2")))
   (Let
    (list
     (cons
      (Uid "kappa" 435)
      (App-Code
       (Code-Label (Uid "compose-coercions" 86))
       (list
        (Quote-Coercion (Sequence (Identity) (Inject (Int))))
        (Quote-Coercion (Sequence (Project (Int) "2") (Identity)))
        (Quote 0)
        (Quote 0)))))
    (App-Code
     (Code-Label (Uid "apply-coercion" 159))
     (list (Quote 1) (Var (Uid "kappa" 435)) (Quote #t)))))
  ;; effectively the same code code from above
  ;; TODO check if tsuda code duplicates coercions composition expressions
  (check-forms=?
   (ic-andre-emulate-tsuda
    (Cast (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))
          (Twosome (Dyn) (Int) "2")))
   (Let
    (list
     (cons
      (Uid "cont-crcn" 435)
      (Quote-Coercion (Sequence (Project (Int) "2") (Identity)))))
    (Let
     (list
      (cons
       (Uid "cont-crcn" 436)
       (App-Code
        (Code-Label (Uid "compose-coercions" 86))
        (list
         (Quote-Coercion (Sequence (Identity) (Inject (Int))))
         (Var (Uid "cont-crcn" 435))
         (Quote 0)
         (Quote 0)))))
     (App-Code
      (Code-Label (Uid "apply-coercion" 157))
      (list (Quote 1) (Var (Uid "cont-crcn" 436)) (Quote #t))))))
  ;; What the code should produce
  (check-forms=?
   (ic-andre-cf
    (Cast
     (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))
     (Twosome (Dyn) (Int) "2")))
   (Quote 1))
  ;; sanity check
  (check-forms=?
   (ic-andre
    (Cast
     (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))
     (Twosome (Dyn) (Int) "2")))
   (Quote 1))
  
  (check-andre-emulates-old
   (Lambda '() (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))))
  (check-forms=?
   (ic-andre-emulate-old
    (Lambda '() (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))))
   (Lambda
    '()
    (Castable
     (Uid "fn-cast-0" 437)
     (Dyn-Object (Quote 1) (Type (Int))))))
  (check-forms=?
   (ic-tsuda
    (Lambda '() (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "kappa" 447))
    (Castable
     (Uid "fn-cast-0" 435)
     (Let
      (list
       (cons
        (Uid "kappa" 448)
        (App-Code
         (Code-Label (Uid "compose-coercions" 86))
         (list
          (Quote-Coercion (Sequence (Identity) (Inject (Int))))
          (Var (Uid "kappa" 447))
          (Quote 0)
          (Quote 0)))))
      (App-Code
       (Code-Label (Uid "apply-coercion" 159))
       (list (Quote 1) (Var (Uid "kappa" 448)) (Quote #t)))))))

  (check-forms=?
   (ic-andre-emulate-tsuda
    (Lambda '() (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "cont-crcn" 435))
    (Castable
     (Uid "fn-cast-0" 437)
     (Let
      (list
       (cons
        (Uid "cont-crcn" 436)
        (App-Code
         (Code-Label (Uid "compose-coercions" 86))
         (list
          (Quote-Coercion (Sequence (Identity) (Inject (Int))))
          (Var (Uid "cont-crcn" 435))
          (Quote 0)
          (Quote 0)))))
      (App-Code
       (Code-Label (Uid "apply-coercion" 159))
       (list (Quote 1) (Var (Uid "cont-crcn" 436)) (Quote #t)))))))
  
  (check-forms=?
   (ic-andre-cf
    (Lambda '() (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "cont-crcn" 437))
    (Castable
     (Uid "fn-cast-0" 438)
     (App-Code
      (Code-Label (Uid "apply-coercion" 159))
      (list
       (Quote 1)
       (App-Code
        (Code-Label (Uid "compose-coercions" 86))
        (list
         (Quote-Coercion (Sequence (Identity) (Inject (Int))))
         (Var (Uid "cont-crcn" 437))
         (Quote 0)
         (Quote 0)))
       (Quote #t))))))
  (check-forms=?
   (ic-andre
    (Lambda '() (Cast (Quote 1) (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "cont-crcn" 437))
    (Castable (Uid "fn-cast-0" 438) (Dyn-Object (Quote 1) (Type (Int))))))

  (check-andre-emulates-old
   (Lambda
    (list (Uid "f" 0) (Uid "x" 1))
    (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))))

  (check-forms=?
   (ic-andre-emulate-old
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))))
   (Lambda
    (list (Uid "f" 0) (Uid "x" 1))
    (Castable
     (Uid "fn-cast-2" 3)
     (App-Fn-or-Proxy
      (Uid "apply-coercion" 3) (Uid "compose-coercions" 0)
      (Var (Uid "f" 0)) (list (Var (Uid "x" 1)))))))

  (check-forms=?
   (ic-tsuda
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))))
   (Lambda
    (list (Uid "kappa" 449) (Uid "f" 0) (Uid "x" 1))
    (Castable
     (Uid "fn-cast-2" 435)
     (App-Fn-or-Proxy
      (Uid "apply-coercion" 159)
      (Uid "compose-coercions" 86)
      (Var (Uid "f" 0))
      (list (Var (Uid "kappa" 449)) (Var (Uid "x" 1)))))))

  (check-forms=?
   (ic-andre-cf
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))))
   (Lambda
    (list (Uid "f" 0) (Uid "x" 1) (Uid "cont-crcn" 2))
    (Castable
     (Uid "fn-cast-2" 435)
     (App-Fn-or-Proxy
      (Uid "apply-coercion" 159)
      (Uid "compose-coercions" 86)
      (Var (Uid "f" 0))
      (list (Var (Uid "x" 1)) (Var (Uid "cont-crcn" 2)))))))
  (check-forms=?
   (ic-andre
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))))
      (Lambda
    (list (Uid "f" 0) (Uid "x" 1) (Uid "cont-crcn" 437))
    (Castable
     (Uid "fn-cast-2" 438)
     (App-Fn-or-Proxy
      (Uid "apply-coercion" 159)
      (Uid "compose-coercions" 86)
      (Var (Uid "f" 0))
      (list (Var (Uid "x" 1)) (Var (Uid "cont-crcn" 437)))))))

  (check-andre-emulates-old
   (Lambda
    (list (Uid "f" 0) (Uid "x" 1))
    (Cast (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))
          (Twosome (Int) (Dyn) "1"))))
  (check-forms=?
   (ic-andre-emulate-old
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (Cast (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))
           (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "f" 0) (Uid "x" 1))
    (Castable
     (Uid "fn-cast-2" 437)
     (Dyn-Object
      (App-Fn-or-Proxy
       (Uid "apply-coercion" 159)
       (Uid "compose-coercions" 86)
       (Var (Uid "f" 0))
       (list (Var (Uid "x" 1))))
      (Type (Int))))))
  (check-forms=?
   (ic-tsuda
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (Cast (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))
           (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "kappa" 449) (Uid "f" 0) (Uid "x" 1))
    (Castable
     (Uid "fn-cast-2" 435)
     (Let
      (list
       (cons
        (Uid "kappa" 450)
        (App-Code
         (Code-Label (Uid "compose-coercions" 86))
         (list
          (Quote-Coercion (Sequence (Identity) (Inject (Int))))
          (Var (Uid "kappa" 449))
          (Quote 0)
          (Quote 0)))))
      (App-Fn-or-Proxy
       (Uid "apply-coercion" 159)
       (Uid "compose-coercions" 86)
       (Var (Uid "f" 0))
       (list (Var (Uid "kappa" 450)) (Var (Uid "x" 1))))))))
  (check-forms=?
   (ic-andre-emulate-tsuda
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (Cast (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))
           (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "f" 0) (Uid "x" 1) (Uid "cont-crcn" 435))
    (Castable
     (Uid "fn-cast-2" 437)
     (Let
      (list
       (cons
        (Uid "cont-crcn" 436)
        (App-Code
         (Code-Label (Uid "compose-coercions" 86))
         (list
          (Quote-Coercion (Sequence (Identity) (Inject (Int))))
          (Var (Uid "cont-crcn" 435))
          (Quote 0)
          (Quote 0)))))
      (App-Fn-or-Proxy
       (Uid "apply-coercion" 159)
       (Uid "compose-coercions" 86)
       (Var (Uid "f" 0))
       (list (Var (Uid "x" 1)) (Var (Uid "cont-crcn" 436))))))))

  (check-forms=?
   (ic-andre-cf
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (Cast (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))
           (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "f" 0) (Uid "x" 1) (Uid "cont-crcn" 437))
    (Castable
     (Uid "fn-cast-2" 438)
     (App-Fn-or-Proxy
      (Uid "apply-coercion" 159)
      (Uid "compose-coercions" 86)
      (Var (Uid "f" 0))
      (list
       (Var (Uid "x" 1))
       (App-Code
        (Code-Label (Uid "compose-coercions" 86))
        (list
         (Quote-Coercion (Sequence (Identity) (Inject (Int))))
         (Var (Uid "cont-crcn" 437))
         (Quote 0)
         (Quote 0))))))))

  (check-forms=?
   (ic-andre
    (Lambda
     (list (Uid "f" 0) (Uid "x" 1))
     (Cast (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1))))
           (Twosome (Int) (Dyn) "1"))))
   (Lambda
    (list (Uid "f" 0) (Uid "x" 1) (Uid "cont-crcn" 437))
    (Castable
     (Uid "fn-cast-2" 440)
     (If
      (Op '= (list (Var (Uid "cont-crcn" 437)) (Quote 0)))
      (Let
       (list
        (cons
         (Uid "tail-crcn-ref" 438)
         (Unguarded-Box (Quote-Coercion (Sequence (Identity) (Inject (Int)))))))
       (Let
        (list
         (cons
          (Uid "ret-val" 439)
          (App-Fn-or-Proxy
           (Uid "apply-coercion" 159)
           (Uid "compose-coercions" 86)
           (Var (Uid "f" 0))
           (list (Var (Uid "x" 1)) (Var (Uid "tail-crcn-ref" 438))))))
        (App-Code
         (Code-Label (Uid "apply-coercion" 159))
         (list
          (Var (Uid "ret-val" 439))
          (Unguarded-Box-Ref (Var (Uid "tail-crcn-ref" 438)))
          (Quote #t)))))
      (Begin
       (list
        (Unguarded-Box-Set!
         (Var (Uid "cont-crcn" 437))
         (App-Code
          (Code-Label (Uid "compose-coercions" 86))
          (list
           (Quote-Coercion (Sequence (Identity) (Inject (Int))))
           (Unguarded-Box-Ref (Var (Uid "cont-crcn" 437)))
           (Quote 0)
           (Quote 0)))))
       (App-Fn-or-Proxy
        (Uid "apply-coercion" 159)
        (Uid "compose-coercions" 86)
        (Var (Uid "f" 0))
        (list (Var (Uid "x" 1)) (Var (Uid "cont-crcn" 437)))))))))
  
  
  (check-andre-emulates-old
   (Lambda
    (list  (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1))
    (Cast
     (If (Cast (App (Var "c") '()) (Twosome (Dyn) (Bool) "2"))
         (Cast (Quote 1) (Twosome (Int) (Dyn) "3"))
         (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1)))))
     (Twosome (Dyn) (Int) "1"))))

  (check-forms=?
   (ic-andre-emulate-old
    (Lambda
     (list  (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1))
     (Cast
      (If (Cast (App (Var "c") '()) (Twosome (Dyn) (Bool) "2"))
          (Cast (Quote 1) (Twosome (Int) (Dyn) "3"))
          (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1)))))
      (Twosome (Dyn) (Int) "1"))))
   (Lambda
    (list (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1))
    (Castable
     (Uid "fn-cast-4" 439)
     (Let
      (list
       (cons
        (Uid "v" 438)
        (If
         (Let
          (list
           (cons
            (Uid "v" 437)
            (App-Fn-or-Proxy
             (Uid "apply-coercion" 159)
             (Uid "compose-coercions" 86)
             (Var "c")
             '())))
          (If
           (Dyn-Immediate-Tag=Huh (Var (Uid "v" 437)) (Type (Bool)))
           (Dyn-Immediate-Value (Var (Uid "v" 437)))
           (App-Code
            (Code-Label (Uid "project" 241))
            (list (Var (Uid "v" 437)) (Type (Bool)) (Quote "2") (Quote #t)))))
         (Dyn-Object (Quote 1) (Type (Int)))
         (App-Fn-or-Proxy
          (Uid "apply-coercion" 159)
          (Uid "compose-coercions" 86)
          (Var (Uid "f" 0))
          (list (Var (Uid "x" 1)))))))
      (If
       (Dyn-Immediate-Tag=Huh (Var (Uid "v" 438)) (Type (Int)))
       (Dyn-Immediate-Value (Var (Uid "v" 438)))
       (App-Code
        (Code-Label (Uid "project" 241))
        (list (Var (Uid "v" 438)) (Type (Int)) (Quote "1") (Quote #t))))))))

  (check-forms=?
   (ic-tsuda
    (Lambda
     (list  (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1))
     (Cast
      (If (Cast (App (Var "c") '()) (Twosome (Dyn) (Bool) "2"))
          (Cast (Quote 1) (Twosome (Int) (Dyn) "3"))
          (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1)))))
      (Twosome (Dyn) (Int) "1"))))
   (Lambda
    (list (Uid "kappa" 451) (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1))
    (Castable
     (Uid "fn-cast-4" 435)
     (Let
      (list
       (cons
        (Uid "kappa" 452)
        (App-Code
         (Code-Label (Uid "compose-coercions" 86))
         (list
          (Quote-Coercion (Sequence (Project (Int) "1") (Identity)))
          (Var (Uid "kappa" 451))
          (Quote 0)
          (Quote 0)))))
      (If
       (App-Fn-or-Proxy
        (Uid "apply-coercion" 159)
        (Uid "compose-coercions" 86)
        (Var "c")
        (list (Quote-Coercion (Sequence (Project (Bool) "2") (Identity)))))
       (Let
        (list
         (cons
          (Uid "kappa" 453)
          (App-Code
           (Code-Label (Uid "compose-coercions" 86))
           (list
            (Quote-Coercion (Sequence (Identity) (Inject (Int))))
            (Var (Uid "kappa" 452))
            (Quote 0)
            (Quote 0)))))
        (App-Code
         (Code-Label (Uid "apply-coercion" 159))
         (list (Quote 1) (Var (Uid "kappa" 453)) (Quote #t))))
       (App-Fn-or-Proxy
        (Uid "apply-coercion" 159)
        (Uid "compose-coercions" 86)
        (Var (Uid "f" 0))
        (list (Var (Uid "kappa" 452)) (Var (Uid "x" 1)))))))))

  (check-forms=?
   (ic-andre-emulate-tsuda
    (Lambda
     (list  (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1))
     (Cast
      (If (Cast (App (Var "c") '()) (Twosome (Dyn) (Bool) "2"))
          (Cast (Quote 1) (Twosome (Int) (Dyn) "3"))
          (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1)))))
      (Twosome (Dyn) (Int) "1"))))
   (Lambda
    (list (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1) (Uid "cont-crcn" 435))
    (Castable
     (Uid "fn-cast-4" 439)
     (Let
      (list
       (cons
        (Uid "cont-crcn" 436)
        (App-Code
         (Code-Label (Uid "compose-coercions" 86))
         (list
          (Quote-Coercion (Sequence (Project (Int) "1") (Identity)))
          (Var (Uid "cont-crcn" 435))
          (Quote 0)
          (Quote 0)))))
      (If
       (Let
        (list
         (cons
          (Uid "cont-crcn" 437)
          (Quote-Coercion (Sequence (Project (Bool) "2") (Identity)))))
        (App-Fn-or-Proxy
         (Uid "apply-coercion" 159)
         (Uid "compose-coercions" 86)
         (Var "c")
         (list (Var (Uid "cont-crcn" 437)))))
       (Let
        (list
         (cons
          (Uid "cont-crcn" 438)
          (App-Code
           (Code-Label (Uid "compose-coercions" 86))
           (list
            (Quote-Coercion (Sequence (Identity) (Inject (Int))))
            (Var (Uid "cont-crcn" 436))
            (Quote 0)
            (Quote 0)))))
        (App-Code
         (Code-Label (Uid "apply-coercion" 159))
         (list (Quote 1) (Var (Uid "cont-crcn" 438)) (Quote #t))))
       (App-Fn-or-Proxy
        (Uid "apply-coercion" 159)
        (Uid "compose-coercions" 86)
        (Var (Uid "f" 0))
        (list (Var (Uid "x" 1)) (Var (Uid "cont-crcn" 436)))))))))

  (check-forms=?
   (ic-andre-cf
    (Lambda
     (list  (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1))
     (Cast
      (If (Cast (App (Var "c") '()) (Twosome (Dyn) (Bool) "2"))
          (Cast (Quote 1) (Twosome (Int) (Dyn) "3"))
          (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1)))))
      (Twosome (Dyn) (Int) "1"))))
   (Lambda
    (list (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1) (Uid "cont-crcn" 437))
    (Castable
     (Uid "fn-cast-4" 438)
     (If
      (App-Fn-or-Proxy
       (Uid "apply-coercion" 159)
       (Uid "compose-coercions" 86)
       (Var "c")
       (list (Quote-Coercion (Sequence (Project (Bool) "2") (Identity)))))
      (App-Code
       (Code-Label (Uid "apply-coercion" 159))
       (list (Quote 1) (Var (Uid "cont-crcn" 437)) (Quote #t)))
      (App-Fn-or-Proxy
       (Uid "apply-coercion" 159)
       (Uid "compose-coercions" 86)
       (Var (Uid "f" 0))
       (list
        (Var (Uid "x" 1))
        (App-Code
         (Code-Label (Uid "compose-coercions" 86))
         (list
          (Quote-Coercion (Sequence (Project (Int) "1") (Identity)))
          (Var (Uid "cont-crcn" 437))
          (Quote 0)
          (Quote 0)))))))))

  (check-forms=?
   (ic-andre
    (Lambda
     (list  (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1))
     (Cast
      (If (Cast (App (Var "c") '()) (Twosome (Dyn) (Bool) "2"))
          (Cast (Quote 1) (Twosome (Int) (Dyn) "3"))
          (App (Var (Uid "f" 0)) (list (Var (Uid "x" 1)))))
      (Twosome (Dyn) (Int) "1"))))
   (Lambda
    (list (Uid "c" 2) (Uid "f" 0) (Uid "g" 3) (Uid "x" 1) (Uid "cont-crcn" 437))
    (Castable
     (Uid "fn-cast-4" 442)
     (If
      (Let
       (list
        (cons
         (Uid "tail-crcn-ref" 438)
         (Unguarded-Box
          (Quote-Coercion (Sequence (Project (Bool) "2") (Identity))))))
       (Let
        (list
         (cons
          (Uid "ret-val" 439)
          (App-Fn-or-Proxy
           (Uid "apply-coercion" 159)
           (Uid "compose-coercions" 86)
           (Var "c")
           (list (Var (Uid "tail-crcn-ref" 438))))))
        (App-Code
         (Code-Label (Uid "apply-coercion" 159))
         (list
          (Var (Uid "ret-val" 439))
          (Unguarded-Box-Ref (Var (Uid "tail-crcn-ref" 438)))
          (Quote #t)))))
      (Quote 1)
      (If
       (Op '= (list (Var (Uid "cont-crcn" 437)) (Quote 0)))
       (Let
        (list
         (cons
          (Uid "tail-crcn-ref" 440)
          (Unguarded-Box
           (Quote-Coercion (Sequence (Project (Int) "1") (Identity))))))
        (Let
         (list
          (cons
           (Uid "ret-val" 441)
           (App-Fn-or-Proxy
            (Uid "apply-coercion" 159)
            (Uid "compose-coercions" 86)
            (Var (Uid "f" 0))
            (list (Var (Uid "x" 1)) (Var (Uid "tail-crcn-ref" 440))))))
         (App-Code
          (Code-Label (Uid "apply-coercion" 159))
          (list
           (Var (Uid "ret-val" 441))
           (Unguarded-Box-Ref (Var (Uid "tail-crcn-ref" 440)))
           (Quote #t)))))
       (Begin
        (list
         (Unguarded-Box-Set!
          (Var (Uid "cont-crcn" 437))
          (App-Code
           (Code-Label (Uid "compose-coercions" 86))
           (list
            (Quote-Coercion (Sequence (Project (Int) "1") (Identity)))
            (Unguarded-Box-Ref (Var (Uid "cont-crcn" 437)))
            (Quote 0)
            (Quote 0)))))
        (App-Fn-or-Proxy
         (Uid "apply-coercion" 159)
         (Uid "compose-coercions" 86)
         (Var (Uid "f" 0))
         (list (Var (Uid "x" 1)) (Var (Uid "cont-crcn" 437))))))))))
  

  )
