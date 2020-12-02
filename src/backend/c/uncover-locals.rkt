#lang typed/racket/base/no-check
#|------------------------------------------------------------------------------+
|Pass: src/data/remove-let                                          |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass records local variables and should probably be placed
| after the flattening passes.
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require racket/set
         racket/match
         typed/racket/unsafe
         "../../helpers.rkt"
         "../../errors.rkt"
         "../../configuration.rkt"
         "../../lib/mutable-set.rkt"
         "../../language/form-map.rkt"
         "../../language/forms.rkt")

;; Only the pass is provided by this module
(provide
 uncover-locals)

#;
(TODO this pass has a very wierd interface for effects.
      I think that at the time I was trying to be clever.
      This should be fixed to be more naive.
      A perhaps everything in the body should be in the state
      monad.)

(: uncover-locals (-> Data1-Lang Data2-Lang))
(define (uncover-locals prgm)
  (match-let ([(Prog (list name count type)
                 (GlobDecs d* (Labels bnd-code* tail))) prgm])
    (define uc (make-unique-counter count))
    (parameterize ([current-unique-counter uc])
      (let* ([gd-set (list->mset d*)]
             [bnd-code* : D2-Bnd-Code* (ul-bnd-code* gd-set bnd-code*)]
             [body : D2-Body (ul-body gd-set tail)])
        (Prog (list name (unique-counter-next! uc) type)
          (GlobDecs d* (Labels bnd-code* body)))))))

(: ul-bnd-code* (-> (MSet Uid) D1-Bnd-Code* D2-Bnd-Code*))
(define (ul-bnd-code* dec* bnd*) (map (ul-bnd-code dec*) bnd*))

(: ul-bnd-code ((MSet Uid) -> (D1-Bnd-Code -> D2-Bnd-Code)))
(define ((ul-bnd-code dec*) bnd)
  (match-let ([(cons uid (Code uid* exp)) bnd])
    (cons uid (Code uid* (ul-body dec* exp)))))

(: ul-body ((MSet Uid) D1-Tail -> D2-Body))
(define (ul-body gdecs tail)
  (define vars : (MSet Uid) (mset))
  (define space : (Boxof (Bnd* Nat)) (box '()))

  (: rec (case->
          [D1-Effect -> D2-Effect]
          [D1-Pred -> D2-Pred]
          [D1-Value -> D2-Value]
          [D1-Tail -> D2-Tail]
          ))
  (define (rec x)
    (match x
      [(Assign (? Uid? uid) e)
       (mset-add! vars uid)
       (Assign uid (rec e))]
      [(Repeat i e1 e2 #f #f e3) 
       (mset-add! vars i)
       (Repeat i (rec e1) (rec e2) #f #f (rec e3))]
      [(Stack-Alloc d)
       (define u (next-uid! "stack_alloc"))
       (set-box! space (cons (cons u d) (unbox space)))
       (Var u)]
      [other (form-map other rec)]))

  (define tail^ (rec tail))
  (mset-subtract! vars gdecs)
  (Locals (mset->list vars) (unbox space) tail^))


