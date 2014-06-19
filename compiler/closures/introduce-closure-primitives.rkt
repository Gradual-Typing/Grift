#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/introduce-closure-primitives                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: 
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors)

(require Schml/language/shared
         (prefix-in s: Schml/language/closure-il1)
         (prefix-in t: Schml/language/closure-il2))
;; Only the pass is provided by this module
(provide introduce-closure-primitives)

(define-pass (introduce-closure-primitives prgm comp-config)
  ;; A holder for now of a check that we must perform if we
  ;; are going to optimize direct calls
  (define s:Label? (lambda (a) #f))
  (define code-field-name (uvar "code" 0))
  (define (mk-pset-form var)
        (lambda (i var)
          (t:Prim Void-Type (Closure-Set! var (Const Int-Type i) v))))
      (define (calculate-offsets start ls)
        (if (null? ls)
            '()
            (cons start
                  (calculate-offsets
                   (+ (size-of-rt-obj (car ls)) start)
                 (cdr ls)))))
    (define (icp-binding lbl ty fml* free* exp)
      (Bnd:Ty lbl (t:Lambda ty fml*
                    ((icp-expr (mk-env (car free*) (cdr free*))) exp))
              ty))
    (define (icp-closures var lbl free* ty)
      (Bnd:Ty var (Closure:Build lbl free*) ty))
    (define (cset-fold env)
      (lambda (var lbl free* cset*) ;; cset is an accumulator
        (cons (mk-cset var code-field-name lbl env)
              (for/fold ([cset* cset*])
                  ([free (in-list free*)]
                   (let ((fvar (Fml-id free))
                         (fvar-ty (Fml:Ty-type free)))
                     (cons (mk-cset clos fvar fvar fvar-ty env) cset*)))))))
    (define (mk-cset clos field free free-ty env)
      (T:Prim Void-Type (Closure:Set clos field (lookup free free-ty env)))) 
    (define (lookup var var-ty env)
      (env-lookup env var (th (t:Var var-ty var))))
    (define (extend clos)
      (lambda (var var-ty env)
        (env-extend env var (t:Prim var-ty (Closure:Ref clos var)))))
    (define (mk-env clos free*)
      (foldr (extend clos)
             (empty-env) (map Fml-id free*) (map Fml:Ty-type free*)))
    (define (icp-expr env)
      (define (recur exp)
        (match exp
          [(Let-Proc ty
                     (list
                      (Bnd:Ty bnd-lbl*
                              (S:Lambda lam-ty* lam-fml** lam-free** lam-exp*)
                              bnd-ty*) ...)
                     (list
                      (Bnd:Ty clos-var*
                              (S:Close-Over clo-lbl* clos-free**)
                              clos-ty*) ...)
                     (app (icp-expr env) body))
           (t:Let-Proc
              ty 
              (map icp-binding bnd-lbl* lam-ty* lam-fml** lam-free** lam-exp*)
              (t:Let
               ty
               (map (icp-closures env) clos-var* clos-lbl* clos-free** clos-ty)
               (t:Begin
                ty
                (foldr cset-fold clos-var* clos-lbl* clos-free**)
                body)))]
          [(s:Let ty (list (Bnd:Ty i* (app recur e*) t*) ...) exp)
           (t:Let ty (map Bnd:Ty i* e* t*) (recur exp))]
          [(s:Cast ty-cast (app recur exp) ty-exp label)
           (t:Cast ty-cast exp ty-exp label)]
          [(s:If ty tst csq alt)
           (t:If ty (recur tst) (recur csq) (recur alt))]
          [(s:App ty exp (list (app recur exp*) ...))
           (if (s:Label? exp)
               (t:App ty exp exp*)
               (let ((exp (recur exp)))
                 (t:App ty
                        (t:Prim Expr-ty
                                (Closure:Ref exp code-field-name))
                        exp*)))]
          [(s:Prim ty pexp) (icp-prim recur ty pexp)]
          [(s:Var t i) (lookup i t env)]
          [(s:Const t k) (t:Const t k)]
          [e (match-pass-error pass 'icp-expr e)])
        recur))
  (define (icp-prim icp-expr ty pexp)
    (match pexp
      [(Op:IntxInt (app icp-expr fst) (app icp-expr snd))
       (t:Prim ty (mk-op:int-int pexp fst snd))
       (values 
               (set-union fstf* sndf*))]
      [(Rel:IntxInt (app icp-expr fst fstf*) (app icp-expr snd sndf*))
       (values (t:Prim ty (mk-rel:int-int pexp fst snd))
               (set-union fstf* sndf*))] 
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  (define (icp-let-proc letp env)
    
    (match letp
      []))
  (match prgm
    [(s:Prog n u t e)
     (s:Prog n u t ((icp-expr (empty-env)) e))]
    [otherwise (match-pass-error pass 'body prgm)]))





  (define-who (introduce-procedure-primitives ast)
    (define (Expr exp als)
      (match exp
        [,label (guard (label? label)) label]
        [,uvar (guard (uvar? uvar)) (Lookup-Free uvar als)]
        [(quote ,i ) `(quote ,i)]
        [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
        [(begin ,[e*] ... ,[v]) `(begin ,e* ... ,v)]
        [(let ([,new-u* ,[exp*]] ...) ,[exp])`(let ([,new-u* ,exp*] ...) ,exp)]
        [(letrec ([,lbl* (lambda (,fml** ...)
                           (bind-free ,[(Update-als als) -> als*]
                                      ,exp*))] ...)
           ,[(Letrec-Body als) -> body])
         `(letrec ([,lbl* (lambda (,fml** ...) ,(map Expr exp* als*))] ...)
            ,body)]         
        [(,prim ,[exp*] ...) (guard (primitiveq prim)) `(,prim ,exp* ...)]
        [(,exp1 ,exp2 ,[exp*] ...)
         (cond
           [(label? exp1) `(,exp1 ,(Expr exp2 als) ,exp* ...)]
           [else `((procedure-code ,(Expr exp1 als))
                   ,(Expr exp2 als) ,exp* ...)])]
        [,x (errorf who "Unmatched datum in Expr ~a" x)]))

      (define (Letrec-Body als)
        (define (pset-form var free*)
          (map (lambda (i f)
                 `(procedure-set! ,var ,i ,(Lookup-Free f als)))
               (list->i* 0 free*) free*))
        (define (list->i* i ls)
          (if (null? ls) '() (cons `',i (list->i* (add1 i) (cdr ls)))))
        (define (mk-procs-form lbl free*)
          `(make-procedure ,lbl ',(length free*)))
        (lambda (exp)
          (match exp
            [(closures ([,var* ,lbl* ,free** ...] ...) ,exp)
             (let ((exp (Expr exp als))
                   (psets** (map pset-form var* free**))
                   (mk-procs* (map mk-procs-form lbl* free**)))
               `(let ([,var* ,mk-procs*] ...)
                  (begin ,psets** ... ... ,exp)))])))

      (define (Lookup-Free uvar als)
        (let ((tmp (assq uvar als)))
            (if tmp
                `(procedure-ref ,(cadr tmp) ,(cddr tmp))
                uvar)))
        
      
      (define (Update-als als)
        (lambda (var*)
          (let ((cp (car var*)))
            (let loop ((i 0) (v (cdr var*)))
              (if (null? v)
                  als
                  (cons `(,(car v) ,cp . ',i) (loop (add1 i) (cdr v))))))))
                     
        (Expr ast '()))
