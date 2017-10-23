#lang racket/base

(require racket/match
         "../../src/language/forms.rkt"
         "../../src/helpers-untyped.rkt")

(provide (all-defined-out) mk-coercion)

(module+ test (require rackunit))

(define proxy-function-identity-contraction?
  (make-parameter #t))

(define proxy-guarded-identity-contraction?
  (make-parameter #t))

;; D flavor Coercions

;; i = Int | Bool | ()
;; (base-type? x) :: Any -> Boolean : (U Int Bool Unit)

;; A, B, C ::= i | Dyn | A ... -> B | (GRef A)
;; (grift-type? x) :: Any -> Boolean : Grift-Type

;; Coercions
;; c, d ::= id_A | G! | G?ᵖ | c -> d | c ; d | ⊥ᴳᵖᴴ

(define (coercion? x)
  (or (Identity? x)
      ((inj-coercion? grift-type?) x)
      ((proj-coercion? grift-type? blame-label?) x)
      ((fn-coercion? coercion?) x)
      ((gref-coercion? coercion?) x)
      ((seq-coercion? coercion? coercion?) x)
      ((failed-coercion? blame-label?) x)))

(define ((inj-coercion? type?) x)
  (and (Inject? x)
       (type? (Inject-type x))))

(define ((proj-coercion? type? label?) x)
  (and (Project? x)
       (type? (Project-type x))
       (label? (Project-label x))))

(define ((fn-coercion? c?) x)
  (and (Fn? x)
       (let ([l (Fn-arity x)]
             [a (Fn-fmls  x)])
         (and (exact-nonnegative-integer? l)
              (list? a)
              (= (length a) l)
              (andmap c? a)))
       (c? (Fn-ret x))))

(define ((gref-coercion? c?) x)
  (and (Ref? x)
       (c? (Ref-read x))
       (c? (Ref-write x))))

(define ((seq-coercion? fst? snd?) x)
  (and (Sequence? x)
       (fst? (Sequence-fst x))
       (snd? (Sequence-snd x))))

(define ((failed-coercion? label?) x)
  (and (Failed? x) (label? (Failed-label x))))

(define (space-efficient-lud? x)
  (define (recur? x) (space-efficient-lud? x))
  (define (g? x)
    (or (Identity? x)
        (and (Fn? x)
             (andmap recur? (Fn-fmls x)))
        (and (Ref? x)
             (recur? (Ref-read x))
             (recur? (Ref-write x)))))
  (define (i? x)
    (or (g? x)
        (and (Sequence? x)
             (g? (Sequence-fst x))
             (Inject? (Sequence-snd x)))
        (Failed? x)))
  (or (Identity? x)
      (and (Sequence? x)
           (Project? (Sequence-fst x))
           (i? (Sequence-snd x)))
      (i? x)))



(require "../../src/casts/casts-to-coercions.rkt")

;; create a coercion that is equivalent to performing c1
;; and then performing c2
(trace-define (compose-from-coercions&blame c1 c2)
  (match* (c1 c2)
       ;; Cases that seem to be needed for Lazy-d
       [((Inject t1) (Project t2 p)) ((mk-coercion p) t1 t2)]
       ;; T? & ⊥ = (T? ; ⊥)
       [(c1 (Identity)) c1]
       [((Identity) c2) c2]
       ;; Not sure about this one
       ;; You could create a special ID-Dyn-checked node to optimize this case
       ;; In the case that the label types are the same perform the extraction
       ;; for the effect of raising blame when it is wrong and just return the dyn
       ;; Covered by the last case
       ;; [((Project t1 lbl) (Inject t2)) (Sequence c1 c2)]
       
       ;; Compose as presented by 
       ;; Idι & Idι = Idι
       [((Identity) (Identity)) #;#:when #;(equal? i1 i2) c2]
       ;; Id* & t = t
       [((Identity) _) c2]
       ;; (s -> t) & (s' -> t') = (s' & s) -> (t & t')
       ;; (Id_s -> Id_t) = Id_(s -> t)  
       [((Fn n1 s1* t1) (Fn n2 s2* t2))
        (cond
          [(= n1 n2)
           (define-values (s* t)
             (values (map compose s2* s1*) (compose t1 t2)))
           (match* (s* t)
             [((list (Identity) ...) (Identity))
              #:when (proxy-function-identity-contraction?)
              (Identity)]
             [(s* t*) (Fn n1 s* t)])]
          [else (error 'TODO "This should fail but I am not sure how")])]
       [((Fn n1 s1* t1) (Inject t)) (Sequence c1 c2)]
       ;; (Ref s t) & (Ref s' t') = (Ref (s' & s) (t & t'))
       ;; (Ref Id_t Id_t) = (Id (Ref t))
       [((Ref s1 t1) (Ref s2 t2))
        (define-values (s t)
          (values (compose s1 s2) (compose t2 t1)))
        (match* (s t)
          [((Identity) (Identity)) (Identity)]
          [(s t) (Ref s t)])]
       ;; (g ; G!) & Id* = (g ; G!)
       [((Sequence g (Inject G)) (Identity)) c1]
       ;; (G?ᵖ; i) & t = (G?ᵖ ; (i & t))
       [((Sequence (and fst (Project G p)) i) t)
        ;; no need to check if i is an intermediate
        ;; because the normal form says that is all
        ;; can be.
        (Sequence fst (compose i t))]
       ;; g & (h ; H!) = (g & h) ; H!
       [(g (Sequence h (and snd (Inject H))))
        #:when (not (or (Sequence? g) (Failed? g)))
        (Sequence (compose g h) snd)]
       ;; (g ; G!) & (G?ᵖ ; i) = g & i
       ;; (g; G!) & (H?ᵖ ; i) = ⊥ᴳᵖᴴ
       [((Sequence g (Inject G1)) (Sequence (Project G2 p) i))
        #;(if (equal? G1 G2)
            (compose g i)
        (Failed p))
        (compose g (compose ((mk-coercion p) G1 G2) i))]
       [((Sequence g (Inject G1)) (Project G2 p))
        (compose g ((mk-coercion p) G1 G2))]
       ;; Failures are idempotent
       ;; ⊥ᴳᵖᴴ & s = ⊥ᴳᵖᴴ
       [((and fail (Failed _)) _) fail]
       ;; g ; ⊥ᴳᵖᴴ = ⊥ᴳᵖᴴ
       [(_ (and fail (Failed _))) fail]))

(trace-define (compose-from-interpretations c1 c2)
 (match* (c1 c2)
   [((Identity) c2) c2]
   [(c1 (Identity)) c1]
   [((Inject t1) (Project t2 lbl)) ((mk-coercion lbl) t1 t2)]
   [((Fn n1 s1* t1) (Fn n2 s2* t2))
    (define-values (s* t)
      (values (map compose s2* s1*) (compose t1 t2)))
    (unless (= n1 n2)
      (error 'compose "function types statically enforced at correct arity"))
    (match* (s* t)
      [((list (Identity) ...) (Identity)) (Identity)]
      [(s* t*) (Fn n1 s* t)])]
   [((Ref s1 t1) (Ref s2 t2))
    (define-values (s t)
      (values (compose s1 s2) (compose t2 t1)))
    (match* (s t)
      [((Identity) (Identity)) (Identity)]
      [(s t) (Ref s t)])]
    [((and fail (Failed _)) _) fail]
    [((Inject _) (and fail (Failed _))) fail]
    [((Sequence c11 c12) c2)
     (compose c11 (compose c12 c2))]
    [((and proj (Project _ _))
      (and seq (Sequence (Fn _ _ _) _)))
     (Sequence proj seq)]
    [((and proj (Project _ _))
      (and seq (Sequence (Ref _ _) _)))
     (Sequence proj seq)]
    [(c1 (Sequence c21 c22)) (compose (compose c1 c21) c22)]
    [(c1 c2) (Sequence c1 c2)]))


 (define (compose-se-lud c1 c2)
   (define (g? x)
     (or (Identity? x) (Fn? x) (Ref? x)))
   (match* (c1 c2)
     [(_ (Identity)) c1]
     [((Identity) _) c2]
     [((Fn n1 s1* t1) (Fn n2 s2* t2))
      (define-values (s* t)
        (values (map compose-se-lud s2* s1*) (compose-se-lud t1 t2)))
      (match* (s* t)
        [((list (Identity) ...) (Identity)) (Identity)]
        [(s* t) (Fn n1 s* t)])]
     [((Ref s1 t1) (Ref s2 t2))
      (define-values (s t)
        (values (compose-se-lud s1 s2) (compose-se-lud t2 t1)))
      (match* (s t)
        [((Identity) (Identity)) (Identity)]
        [(_ _) (Ref s t)])]
     ;;[((Identity (Dyn)) s) s]
     ;;[((Sequence g (Inject _)) (Identity (Dyn))) c1]
     [((Sequence (and I? (Project _ _)) i) s) (Sequence I? (compose-se-lud i s))]
     [((Sequence g (Inject t1)) (Sequence (Project t2 l) i)) (compose-se-lud g (compose-se-lud ((mk-coercion l) t1 t2) i))]
     [((? g? g1) (Sequence g2 (and I! (Inject _)))) (Sequence (compose-se-lud g1 g2) I!)]
     ;; Identity is idempotent
     [((Failed l) _) c1]
     [(i (Failed l)) c2]))


(define (seq-inj? x)
  (and (Sequence? x)
       (Inject? (Sequence-snd x))))

(define (seq-inj-g x)
  (Sequence-fst x))

(define (seq-inj-t x)
  (Inject-type (Sequence-snd x)))

(define (seq-prj? x)
  (and (Sequence? x)
       (Project? (Sequence-fst x))))

(define (seq-prj-t x)
  (Project-type (Sequence-fst x)))

(define (seq-prj-l x)
  (Project-label (Sequence-fst x)))

(define (seq-prj-i x)
  (Sequence-snd x))

(define (seq-prj t l i)
  (Sequence (Project t l) i))

(define (seq-inj g t)
  (Sequence g (Inject t)))


(module+ test
  (check eq%id
         (compose-se-ld-efficient (Sequence (Identity)
                                             (Inject   (Fn 0 '() (Int))))
                                   (Sequence (Project  (Fn 0 '() (Dyn)) "1")
                                             (Identity)))
         (Fn 0 '() (Sequence (Identity) (Inject (Int)))))

  (check eq%id
         (compose-se-ld-efficient
          (Fn 1 (list (Sequence (Project (Bool) "1") (Identity)))
              (Sequence (Identity) (Inject (Bool))))
          (Fn 1 (list (Sequence (Identity) (Inject (Bool))))
              (Sequence (Project (Bool) "2") (Identity))))
         (Identity))
)  

(trace-define (compose-se-ld-efficient c1 c2)
  ;; Preconditions c1 = [[ t1 =>^l t2 ]] and c2 = [[ t2 =>^l t3 ]]
   (unless (and (space-efficient-lud? c1)
                (space-efficient-lud? c2))
     (error 'compose-se-lud-efficient "precodition violated ~v ~v" c1 c2))
   (define res
     (cond
       [(Identity? c1) c2]
       [(Identity? c2) c1]
       [(seq-prj? c1)
        (seq-prj (seq-prj-t c1) (seq-prj-l c1) (compose (seq-prj-i c1) c2))]
       ;; Checking Both fails here eliminates several branches
       [(Failed? c1) c1]
       [(Failed? c2) c2]
       [(seq-inj? c1) ;; c2 must be (seq-prj I3 l i) explained on the next line
        ;; c1 = [[I1 => Dyn]] = (seq-inj g I1)
        ;; c2 = [[Dyn =>^l I3]] = (Id Dyn) or (seq-prj I3 l i)
        ;; but we checked above that for the Id case ;) ... magic
        ;; furthermore I1 and I3 injectable types
        ;; therefore we can infer that g? = [[I1 =>^l I3]] = (Failed l) or some g 
        ;; Failure should always be the slowest path so we can act
        (let ([g? (make-coercion-efficient (seq-inj-t c1) (seq-prj-t c2) (seq-prj-l c2))])
          ;; composing the smaller two g and g? avoids one recursive call if i = (seq-inj i I4))
          (let ([t (compose (seq-inj-g c1) g?)])
            ;; composing a g and an i will result in another i
            (compose t (seq-prj-i c2))))]
       [(seq-inj? c2) (seq-inj (compose c1 (seq-inj-g c2)) (seq-inj-t c2))]
       [(Fn? c1)
        (let* ([s? (box #t)]
               [s* (map (lambda (c1 c2)
                          (let ([c (compose c1 c2)])
                            (unless (Identity? c)
                              (set-box! s? #f))
                            c))
                        (Fn-fmls c2)
                        (Fn-fmls c1))]
               [t (compose (Fn-ret c1) (Fn-ret c2))])
          (if (and (Identity? t) (unbox s?))
              (Identity)
              (Fn (Fn-arity c1) s* t)))]
       [(Ref? c1)
        (let ([s (compose (Ref-read c1) (Ref-read c2))]
              [t (compose (Ref-write c2) (Ref-write c1))])
          (if (and (Identity? s) (Identity? t))
              (Identity)
              (Ref s t)))]
       [else (error 'compose-se-lud-eff "there shouldn't be anything else ~a ~a" c1 c2)]))
   (define alt-res (compose-se-lud c1 c2))
   (unless (eq%id res alt-res)
     (error 'compose-se-lud-eff "found a difference ~a ~a\n\t~a\n\t~a" c1 c2 res alt-res))
   res)

(define (eq%id x y)
  (or (and (Identity? x)
           (Identity? y))
      (and (Fn? x)
           (Fn? y)
           (equal? (Fn-arity x) (Fn-arity y))
           (andmap eq%id (Fn-fmls x) (Fn-fmls y))
           (eq%id (Fn-ret x) (Fn-ret y)))
      (and (Ref? x)
           (Ref? y)
           (eq%id (Ref-read x) (Ref-read y))
           (eq%id (Ref-write x) (Ref-write y)))
      (and (Project? x)
           (Project? y)
           (equal? (Project-type x) (Project-type y))
           (equal? (Project-label x) (Project-label y)))
      (and (Inject? x)
           (Inject? y)
           (equal? (Inject-type x) (Inject-type y)))
      (and (Sequence? x)
           (Sequence? y)
           (eq%id (Sequence-fst x)(Sequence-fst y))
           (eq%id (Sequence-snd x)(Sequence-snd y)))
      (and (Failed? x)
           (Failed? y)
           (equal? (Failed-label x) (Failed-label y)))))

(define (make-coercion-efficient t1 t2 lbl)
  (cond
    ;; This line only exists to test the theory becuase types are pointer equal in the dev branch
    [(equal? t1 t2) (Identity)] ;; Use eq once types are 
    [(and (Fn? t1) (Fn? t1) (= (Fn-arity t1) (Fn-arity t2)))
     (Fn (Fn-arity t1)
         (map (lambda (t2 t1) (make-coercion-efficient t2 t1 lbl))
              (Fn-fmls t2)
              (Fn-fmls t1))
         (make-coercion-efficient (Fn-ret t1) (Fn-ret t2) lbl))]
    [(and (GRef? t1)  (GRef? t2)) 
     (let ([t1 (GRef-arg t1)]
           [t2 (GRef-arg t2)])
       (Ref (make-coercion-efficient t1 t2 lbl)
            (make-coercion-efficient t2 t1 lbl)))]
    [(and (GVect? t1) (GVect? t2))
     (let ([t1 (GVect-arg t1)]
           [t2 (GVect-arg t2)])
       (Ref (make-coercion-efficient t1 t2 lbl)
            (make-coercion-efficient t2 t1 lbl)))]
    [(Dyn? t1) (Sequence (Project t2 lbl) (Identity))]
    [(Dyn? t2) (Sequence (Identity) (Inject t1))]
    [else (Failed lbl)]))


(define compose compose-se-ld-efficient)
