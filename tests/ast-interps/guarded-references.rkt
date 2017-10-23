#lang racket

(require "../../src/helpers-untyped.rkt"
         "../../src/language/forms.rkt")

(provide (all-defined-out))

(define make-gbox box)
(define make-gvect make-vector)

(struct Interp-GProxy (type value proxy)
  #:transparent)

;; (: write-gref (Cast-Procedure GReP Value Value Index -> Value))
(define (write-grep cast ref val [index #f])
  (match ref
    [(box _)        (set-box! ref val) '()]
    [(vector _ ...) (vector-set! ref index val) '()]
    [(Interp-GProxy _ ref^ (Twosome t1 t2 lbl))
     (write-grep cast ref^ (cast val t2 t1 lbl) index)]
    [(Interp-GProxy _ ref^ (Ref _ write))
     (write-grep cast ref^ (cast write val) index)]
    [otherwise (raise-not-grep write-grep ref)]))

;; (: write-gvect (Cast-Procedure GVect Integer Value -> Value))
#;
(define (write-gvect cast vect pos val)
  (cond
    [(vector? vect) (begin (vector-set! vect pos val) '())]
    [(CL-GVectProxy? vect)
     (define val^
       (cast val (CL-GVectProxy-t2 vect)
             (CL-GVectProxy-t1 vect) (CL-GVectProxy-label vect)))
     (define underlying-vector (CL-GVectProxy-value vect))
     (write-gvect cast underlying-vector pos val^)]
    [else (raise-not-grep write-gvect vect)]))

;; (: cast-gvect (GVect Schml-Type Schml-Type Blame-Label -> GVect))
;; Obviously not space efficient at the start
(define (cast-grep/twosome ref t1 t2 lbl)
  (define type
    (match ref
      [(or (box _) (Interp-GProxy 'Box _ _)) 'Box]
      [(or (vector _ ...) (Interp-GProxy 'Vector _ _)) 'Vector]
      [other (error 'cast-grep/twosome
                    "~a not cononical form of gref or gvect"
                    ref)]))
  (Interp-GProxy type ref (Twosome t1 t2 lbl)))

(define (read-grep cast ref [index #f])
  (match ref
    [(box val) val]
    [(vector _ ...) (vector-ref ref index)]
    [(Interp-GProxy _ ref^ (Twosome t1 t2 lbl))
     (cast (read-grep cast ref^ index) t1 t2 lbl)]
    [(Interp-GProxy _ ref^ (Ref read _))
     (cast read (read-grep cast ref^ index))]
    [otherwise (raise-not-grep read-gref ref)]))

;;(: read-gvect (Cast-Procedure Integer GVect -> Value))
#;
  (define (read-gvect cst i r)
    (cond
      [(vector? r) (vector-ref r i)]
      [(CL-GVectProxy? r)
       (cst (read-gvect cst i (CL-GVectProxy-value r))
            (CL-GVectProxy-t1 r)
            (CL-GVectProxy-t2 r)
            (CL-GVectProxy-label r))]
      [else (raise-not-grep read-gvect r)]))

;; Helpers Follow

(define-syntax-rule (raise-not-grep who x)
  (error 'grift-interp-internal
         "~a applied to non guarded representation value: ~a"
         'who x))

