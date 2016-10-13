
#lang racket/base

(require "stlc.rkt")
(provide (all-defined-out))

(require racket/pretty)
(define-syntax-rule (test-full-coverage c)
  (test-true (or (andmap (λ (c) (< 0 (cdr c))) (covered-cases c))
                 (and (pretty-print (covered-cases c)) #f))))

(define-extended-language λhc STLC
  (e  ::= .... (cast c e))
  (ℓ  ::= string)
  (T! ::=  B! (T! -> T!))
  (B! ::=  B Dyn)
  (C  ::= (T! => T!))
  (p  ::= ε ℓ) 
  (i  ::= ε !) 
  (m  ::= ι (-> (c -> c) (T! -> T!)))
  (c  ::= (hc p T! m i) (hc⊥ p T! ℓ)))

(define-syntax-rule (test-hc-match? pat t)
  (test-true (redex-match? λhc pat (term t))))

(module+ test
  (let ([mkc-cover (make-coverage mkc)])
    (parameterize ([relation-coverage (list mkc-cover)])
      (test-equal (redex-match? λhc T (term Int)) #t)
      (test-equal (redex-match? λhc T! (term Int)) #t)
      (test-equal (term (mkc Int Int  "l")) (term (hc ε Int ι ε)))
      (test-equal (term (mkc Int Bool "l")) (term (hc⊥ ε Int "l")))
      (test-equal (term (mkc Dyn Int  "l")) (term (hc "l" Int ι ε)))
      (test-equal (term (mkc Int Dyn  "l")) (term (hc ε Int ι !)))
      (test-equal (term (mkc Dyn Dyn  "l")) (term (hc ε Dyn ι ε)))
      (test-equal
       (term (mkc Dyn (Int -> Bool) "l"))
       (term (hc "l"
                 (Int -> Bool)
                 (-> ((hc ε Int ι ε) -> (hc ε Bool ι ε)) (Int -> Bool))
                 ε)))
      (test-equal
       (term (mkc (Dyn -> Dyn) (Int -> Bool) "l"))
       (term (hc ε
                 (Dyn -> Dyn)
                 (-> ((hc ε Int ι !) -> (hc "l" Bool ι ε)) (Int -> Bool))
                 ε)))
      (test-equal
       (term (mkc (Int -> Int) Dyn "l"))
       (term (hc ε (Int -> Int) (-> ((hc ε Int ι ε) -> (hc ε Int ι ε)) (Int -> Int)) !)))
      (test-full-coverage mkc-cover))))

(define-metafunction λhc
  mkc : T! T! ℓ -> c
  [(mkc B   B   ℓ) (hc ε B   ι ε)]
  [(mkc Dyn Dyn ℓ) (hc ε Dyn ι ε)]
  [(mkc Dyn B   ℓ) (hc ℓ B   ι ε)]
  [(mkc B   Dyn ℓ) (hc ε B   ι !)]
  [(mkc Dyn (T!_n -> T!_r) ℓ)
   (hc ℓ (T!_n -> T!_r) (-> ((mkc T!_n T!_n ℓ) -> (mkc T!_r T!_r ℓ)) (T!_n -> T!_r)) ε)]
  [(mkc (T!_n -> T!_r)  Dyn ℓ)
   (hc ε (T!_n -> T!_r) (-> ((mkc T!_n T!_n ℓ) -> (mkc T!_r T!_r ℓ)) (T!_n -> T!_r)) !)]
  [(mkc (T!_1 -> T!_2) (T!_3 -> T!_4) ℓ)
   (hc ε (T!_1 -> T!_2) (-> ((mkc T!_3 T!_1 ℓ) -> (mkc T!_2 T!_4 ℓ)) (T!_3 -> T!_4)) ε)]
  [(mkc T!_1 T!_2 ℓ) (hc⊥ ε T!_1 ℓ)])

(define-equality-helpers λhc same? == =/=)

(module+ test
  (test-true  (judgment-holds (⊢hc? ε Dyn Dyn)))
  (test-true  (judgment-holds (⊢hc? ε Int Int)))
  (test-true  (judgment-holds (⊢hc? ε (Int -> Int) (Int -> Int))))
  (test-false (judgment-holds (⊢hc? ε Dyn (Int -> Int))))
  (test-false (judgment-holds (⊢hc? "l" Dyn Dyn)))
  (test-false (judgment-holds (⊢hc? "l" Int Int)))
  (test-false (judgment-holds (⊢hc? "l" (Int -> Int) (Int -> Int))))
  (test-true  (judgment-holds (⊢hc? "l" Dyn (Int -> Int)))))

(define-judgment-form λhc
  #:mode (⊢hc? I O I)
  #:contract (⊢hc? p T! T!)
  [
   -------------------- "wf-proj-empty"
   (⊢hc? ε T! T!)]
  [(=/= Dyn T!)
   -------------------- "wf-proj-check"
   (⊢hc? ℓ Dyn T!)])

(module+ test
  (test-true  (judgment-holds (⊢hc! ε Dyn Dyn)))
  (test-true  (judgment-holds (⊢hc! ε Int Int)))
  (test-true  (judgment-holds (⊢hc! ε (Int -> Int) (Int -> Int))))
  (test-false (judgment-holds (⊢hc! ε (Int -> Int) Dyn)))
  (test-false (judgment-holds (⊢hc! ! Dyn Dyn)))
  (test-true  (judgment-holds (⊢hc! ! Int Dyn)))
  (test-true  (judgment-holds (⊢hc! ! (Int -> Int) Dyn)))
  (test-false (judgment-holds (⊢hc! ! Dyn (Int -> Int)))))

(define-judgment-form λhc
  #:mode (⊢hc! I I O)
  #:contract (⊢hc! i T! T!)
  [
   -------------------- "wf-inj-empty"
   (⊢hc! ε T!_1 T!_1)]
  [(=/= Dyn T!_1)
   -------------------- "wf-inj-box"
   (⊢hc! ! T!_1 Dyn)])


(define-judgment-form λhc
  #:mode (check-wt-hc! I I I)
  #:contract (check-wt-hc! i T! T!)
  [-------------------- "wf-inj-empty"
   (check-wt-hc! ε T!_1 T!_1)]
  [-------------------- "wf-inj-box"
   (check-wt-hc! ! T_1 Dyn)])


(module+ test
  (test-true  (judgment-holds (⊢hcm ι Dyn Dyn)))
  (test-true  (judgment-holds (⊢hcm ι Int Int)))
  (test-false (judgment-holds (⊢hcm ι (Int -> Int) (Int -> Int))))
  (test-false (judgment-holds (⊢hcm ι (Int -> Int) Dyn)))
  (test-true
   (judgment-holds
    (⊢hcm (-> ((hc "" Int ι ε) -> (hc "" Int ι ε)) (Dyn -> Int))
          (Int -> Dyn)
          (Dyn -> Int))))
  (test-true
   (judgment-holds
    (⊢hcm (-> ((hc "" Int ι !) -> (hc "" Int ι !)) (Dyn -> Dyn))
          (Dyn -> Dyn)
          (Dyn -> Dyn))))
  (test-false
   (judgment-holds
    (⊢hcm (-> ((hc "" Dyn ι !) -> (hc "" Dyn ι !)) (Dyn -> Dyn))
          (Dyn -> Dyn)
          (Dyn -> Dyn))))
  (test-false
   (judgment-holds
    (⊢hcm (-> ((hc "" Int ι ε) -> (hc ε Int ι !))
              (Int -> Dyn))
          (Dyn -> Int)
          (Int -> Dyn)))))

(define-judgment-form λhc
  #:mode (⊢hcm I I O)
  #:contract (⊢hcm m T! T!)
  [
   -------------------- "wf-mediate-empty"
   (⊢hcm ι B! B!)]
  [(⊢hc c_1 (T!_3 => T!_1)) (⊢hc c_2 (T!_2 => T!_4))  
   ------------------------------------------------- "wf-mediate-convert"
   (⊢hcm (-> (c_1 -> c_2) (T!_3 -> T!_4))
         (T!_1 -> T!_2)
         (T!_3 -> T!_4))])


(module+ test
  (test-true
   (judgment-holds (⊢hc (hc ε Dyn ι ε)   (Dyn => Dyn))))
  (test-true
   (judgment-holds (⊢hc (hc ε Int ι ε)   (Int => Int))))
  (test-true
   (judgment-holds (⊢hc (hc "l" Int ι !) (Dyn => Dyn))))
  (test-true
   (judgment-holds (⊢hc (hc "l" Int ι ε) (Dyn => Int))))
  (test-false
   (judgment-holds (⊢hc (hc "l" Dyn ι !) (Dyn => Dyn))))
  (test-true
   (judgment-holds
    (⊢hc (hc "" (Int -> Dyn) (-> ((hc "" Int ι ε) -> (hc "" Int ι ε)) (Dyn -> Int)) ε)
         (Dyn => (Dyn -> Int)))))
  (test-true
   (judgment-holds
    (⊢hc (hc ε
             (Int -> Int)
             (-> ((hc "" Int ι ε) -> (hc ε Int ι ε)) (Dyn -> Int))
             ε)
         ((Int -> Int) => (Dyn -> Int)))))
  (test-true
   (judgment-holds
    (⊢hc (hc ε
             (Int -> Dyn)
             (-> ((hc "" Int ι ε) -> (hc "" Int ι !)) (Dyn -> Dyn))
             ε)
         ((Int -> Dyn) => (Dyn -> Dyn)))))
  (test-true
   (judgment-holds
    (⊢hc (hc ε
             (Int -> Int)
             (-> ((hc ε Int ι ε) -> (hc ε Int ι !)) (Int -> Dyn))
             !)
         ((Int -> Int) => Dyn))))
  (test-false
   (judgment-holds
    (⊢hc (hc ε
             (Int -> Int)
             (-> ((hc "" Int ι !) -> (hc "" Dyn ι ε)) (Dyn -> Int))
             ε)
         ((Int -> Int) => (Dyn -> Int)))))

  (define-term i->i=>i->d
    (hc ε
        (Int -> Int)
        (-> ((hc ε Int ι ε) -> (hc ε Int ι !))
            (Int -> Dyn))
        ε))
  (test-true (judgment-holds (⊢hc i->i=>i->d ((Int -> Int) => (Int -> Dyn)))))
  (test-true
   (judgment-holds
    (⊢hc (hc ε
             ((Int -> Dyn) -> Int)
             (-> (i->i=>i->d
                  ->
                  (hc ε Int ι ε))
                 ((Int -> Int) -> Int))
             ε)
         (((Int -> Dyn) -> Int) => ((Int -> Int) -> Int))))))

(define-judgment-form λhc
  #:mode (⊢hc I O)
  #:contract (⊢hc c C)
  [(⊢hc? p T!_1 T!) (⊢hcm m T! T!_m) (⊢hc! i T!_m T!_2)
   ------------------- "wf-hc-cast"
   (⊢hc (hc p T! m i) (T!_1 => T!_2))])

(define-judgment-form λhc
  #:mode (check-wt-hc I I I)
  #:contract (check-wt-hc c C)
  [(check-wt-hc? p T!_1 T!)
   (check-wt-hc-m m T! T!_m)
   (check-wt-hc! i T!_m T!_2) 
   ------------------- "wf-hc-cast"
   (⊢hc (hc p T! m i) (T!_1 => T!_2))]
  [(⊢hc? p T!_1 T!)
   ------------------- "check bottom"
   (check-wt-hc (hc⊥ p T! ℓ) T!_1 T!_2)])

(module+ test
  (test-judgment-holds (check-wt-hc (hc⊥ "1" Int "2") Dyn Int)))

(define-judgment-form λhc
  #:mode (composable-at-type I I I I I)
  #:contract (composable-at-type c c T! T! T!)
  [(check-wt-hc c_1 T!_1 T!_2)
   (check-wt-hc c_2 T!_2 T!_3)
   ------------------- "cct"
   (composable-at-type c_1 c_2 T!_1 T!_2 T!_3)])

(define-judgment-form λhc
  #:mode (wt-composable-at-type I I I)
  #:contract (wt-composable-at-type c c C)
  [(⊢hc c_1 (T!_1 => T!_2))
   (⊢hc c_2 (T!_2 => T!_3))
   ------------------- "cct"
   (wt-composable-at-type c_1 c_2 (T!_1 => T!_3))])

(module+ test
  (let ([cc-coverage (make-coverage cc)])
    (parameterize ([relation-coverage (list cc-coverage)])
      (define-term id-int (hc ε Int ι ε))
      (define-term id-dyn (hc ε Int ι ε))
      (define-term prj-int (hc "1" Int ι ε))
      (define-term inj-int (hc ε Int ι !))
      (define-term chk-int (hc "2" Int ι !))

      (test-equal (term (cc id-int id-int)) (term id-int))
      (test-equal (term (cc id-dyn id-dyn)) (term id-dyn))
      (test-equal (term (cc prj-int inj-int)) (term (hc "1" Int ι !)))
      (test-equal (term (cc inj-int prj-int)) (term id-int))
      (test-equal (term (cc inj-int chk-int)) (term inj-int))
      (test-equal (term (cc (hc ε Dyn ι ε) (hc ε Dyn ι ε))) (term (hc ε Dyn ι ε)))
      (test-equal (term (cc (hc⊥ ε () "c") (hc⊥ ε ((Dyn -> Dyn) -> Dyn) "YL")))
                  (term (hc⊥ ε () "c")))
      (test-equal (term (cc (hc ε Int ι ε) (hc⊥ ε Int "hc")))
                  (term (hc⊥ ε Int "hc")))
      (test-judgment-holds (check-wt-hc (cc (hc ε () ι ε)
                                            (hc⊥ ε () ""))
                                        () (Dyn -> Dyn)))
      (test-equal (term (cc (hc ε () ι ε) (hc⊥ ε () "")))
                  (term (hc⊥ ε () "")))

      (test-hc-match?
       (hc p_1 T!  _   _)
       (hc "l"
           (Int -> Int)
           (-> ((hc ε Int ι ε) -> (hc ε Int ι ε))
               (Int -> Int))
           !))
      (test-hc-match? p "l")
      (test-hc-match? T! (Int -> Int))
      (test-hc-match? c (hc ε Int ι ε))
      (test-hc-match? p ε)
      (test-hc-match? T! Int)
      (test-hc-match? m ι)
      (test-hc-match? i ε)
      
      
      (test-hc-match? (c -> c) ((hc ε Int ι ε) -> (hc ε Int ι ε)))
      (test-hc-match? (T! -> T!) (Int -> Int))
      (test-hc-match? i !)
      (test-hc-match?
       (hc  ℓ  B! _   _  )
       (hc "" Int ι !))
      (test-hc-match?
       c
       (hc "" Int ι !))
      (test-equal
       (term (cc (hc "l1"
                     (Int -> Int)
                     (-> ((hc ε Int ι ε) -> (hc ε Int ι ε))
                         (Int -> Int))
                     !)
                 (hc "l2" Int ι !)))
       (term (hc⊥ "l1" (Int -> Int) "l2")))

      (test-judgment-holds
       (⊢hc (hc "l" (Int -> Int) (-> ((hc ε Int ι ε) -> (hc ε Int ι ε)) (Int -> Int)) !)
            (Dyn => Dyn)))
      (test-judgment-holds
       (⊢hc (hc ε (Int -> Int) (-> ((hc ε Int ι ε) -> (hc ε Int ι ε)) (Int -> Int)) !)
            ((Int -> Int) => Dyn)))
      
      (test-judgment-holds
       (check-wt-hc
        (hc "l"
            (Int -> Int)
            (-> ((hc ε Int ι ε) -> (hc ε Int ι ε)) (Int -> Int))
            !)
        Dyn
        Dyn))

      (test-judgment-holds (check-wt-hc (hc "" Int ι ε) Dyn Int))
      (test-judgment-holds
       (composable-at-type
        (hc ε
            (Int -> Int)
            (-> ((hc ε Int ι ε) -> (hc ε Int ι ε)) (Int -> Int))
            !)
        (hc "" Int ι ε)
        (Int -> Int)
        Dyn
        Int))

      (redex-check
       λhc
       #:satisfying
       (wt-composable-at-type c_1 c_2 (T!_1 => T!_3))
       (judgment-holds (⊢hc (cc c_1 c_2) (T!_1 => T!_3)))
       #:attempts 200)

      (redex-check
       λhc
       #:satisfying
       (composable-at-type c_1 c_2 T!_1 T!_2 T!_3)
       (judgment-holds (check-wt-hc (cc c_1 c_2) T!_1 T!_3))
       #:attempts 200)

      (redex-let* λhc ([T   (term (Int -> Int))]
                       [m   (term (-> ((hc ε Int ι ε) -> (hc ε Int ι ε)) T))]
                       [c_0 (term (hc ε T m !))]
                       [c_1 (term (hc "1" T m !))]
                       [c_2 (term (hc "2" Int ι ε))]
                       [c_3 (term (hc⊥  ε  T "2"))]
                       [c_4 (term (hc⊥ "1" T "2"))])
                  (test-equal (term (cc c_0 c_2)) (term c_3))
                  (test-equal (term (cc c_1 c_2)) (term c_4))
                  (test-judgment-holds (composable-at-type c_0 c_2 T   Dyn Int))
                  (test-judgment-holds (composable-at-type c_1 c_2 Dyn Dyn Int))
                  (test-judgment-holds (check-wt-hc c_3 T   Int))
                  (test-judgment-holds (check-wt-hc c_4 Dyn Int)))
      
      #;
      (redex-check
       λhc
       #:satisfying
       (composable-at-type
        (hc p_1 T! m_1 i_1) (hc "2" B! ι i_2)
        T!_1
        Dyn
        T!_3)
       (judgment-holds
        (check-wt-hc (cc (hc p_1 T! m_1 i_1) (hc "2" B! ι i_2)) T!_1 T!_3)))

      
      (define-judgment-form λhc
        #:mode (infer-fn-tests I I O O O)
        #:contract (infer-fn-tests c c T! T! T!)
        [(⊢hc? p_1 T!_1 (T!_111 -> T!_112))
         (⊢hc  c_11 (T!_121 => T!_111))
         (⊢hc  c_12 (T!_112 => T!_122))
         (⊢hc! p_1 (T!_121 -> T!_122) T!_2)
         (⊢hc? p_2 T!_2 (T!_211 -> T!_212))
         (⊢hc  c_21 (T!_221 => T!_211))
         (⊢hc  c_22 (T!_212 => T!_222))
         (⊢hc! p_1 (T!_221 -> T!_222) T!_3)
         ------------------- "cct"
         (infer-fn-tests (hc p_1
                             (T!_111 -> T!_112)
                             (-> (c_11 -> c_12)
                                 (T!_121 -> T!_122))
                             i_1)
                         (hc p_2
                             (T!_211 -> T!_212)
                             (-> (c_21 -> c_22)
                                 (T!_221 -> T!_222))
                             i_2)
                         T!_1 T!_2 T!_3)])
      
      (redex-check
       λhc
       #:satisfying
       (⊢hc (hc p T! m i) (T!_1 => T!_2))
       (judgment-holds
        (infer-fn-tests
         (hc ε
             (T!_2 -> T!_1)
             (-> ((hc p T! m i) -> (hc p T! m i))
                 (T!_1 -> T!_2))
             !)
         (hc "l"
             (T!_2 -> T!_1)
             (-> ((hc p T! m i) -> (hc p T! m i))
                 (T!_1 -> T!_2))
             ε)
          (T!_2 -> T!_1) Dyn (T!_1 -> T!_2)))
       #:attempts 200)

      (redex-let
       (test-judgment-holds
        (check-wt-hc
         (cc (hc p_1
                 (T!_11 -> T!_12)
                 (-> (c_11 -> c_12) (T!_13 -> T!_14))
                 i_1)
             (hc ℓ
                 (T!_21 -> T!_22)
                 (-> (c_21 -> c_22) (T!_23 -> T!_24))
                 i_2)
             (T!_1 => T!_2)))))
      
      (test-full-coverage cc-coverage))))

;; Assumes (⊢hc c1 (T_1 => T_2)) (⊢hc c1 (T_2 => T_3))
(define-metafunction λhc
  cc : c c -> c
  [(cc (hc p_1 B  m_1 i_1)  (hc p_2 B! m_2 i_2)) (hc p_1 B! m_2 i_2)]
  [(cc (hc p_1 Dyn m_1 i_1) (hc p_2 B! m_2 i_2)) (hc p_2 B! m_2 i_2)]
  [(cc (hc p_1 T!  _   _)   (hc  ℓ  B! _   _  )) (hc⊥ p_1 T! ℓ)]
  [(cc (hc p_1 T!  _   _)   (hc⊥ _  _  ℓ))       (hc⊥ p_1 T! ℓ)]
  [(cc (hc p_1 (T!_11 -> T!_12) (-> (c_11 -> c_12) (T!_13 -> T!_14)) i_1)
       (hc ε   (T!_21 -> T!_22) (-> (c_21 -> c_22) (T!_23 -> T!_24)) i_2))
   (hc p_1
       (T!_11 -> T!_12)
       (-> ((cc c_21 c_11) -> (cc c_12 c_22)) (T!_23 -> T!_24))
       i_2)]
  [(cc (hc p_1
           (T!_11 -> T!_12)
           (-> (c_11 -> c_12) (T!_13 -> T!_14))
           i_1)
       (hc ℓ
           (T!_21 -> T!_22)
           (-> (c_21 -> c_22) (T!_23 -> T!_24))
           i_2))
   (hc p_1
       (T!_11 -> T!_12)
       (-> ((cc c_21 (cc (mkc T!_21 T!_11 ℓ) c_11))
            -> (cc c_12 (cc (mkc T!_14 T!_22 ℓ) c_22)))
           (T!_23 -> T!_24))
       i_2)]
  [(cc (hc p_1 B m_1 i_1)
       (hc ℓ_2 (T_1 -> T_2) (-> (c_1 -> c_2) (T_3 -> T_4)) i_2))
   (hc⊥ p_1 B ℓ_2)]
  [(cc (hc p_1 Dyn m_1 i_1) ;;Don't actually check T!_1
       (hc ℓ_2 (T_1 -> T_2) (-> (c_1 -> c_2) (T_3 -> T_4)) i_2))
   ;; c_2
   (hc ℓ_2 (T_1 -> T_2) (-> (c_1 -> c_2) (T_3 -> T_4)) i_2)]
  [(cc (hc p_1 T! m_1 i_1) (hc ℓ_2 Dyn m_2 i_2))
   (hc p_1 T! m_1 i_1)]
  [(cc (hc⊥ p_1 T!_1 ℓ) _) (hc⊥ p_1 T!_1 ℓ)])





