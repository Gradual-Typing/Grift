#lang typed/racket/base
(require
 (for-syntax
  racket/base
  syntax/parse)
 racket/pretty)

(provide
 (except-out
  (all-defined-out)
  ;; This is some syntax used to keep repeating myself
  with-reconfigurable-parameters))


;; This syntax-class is used to parse the id and type
;; out of parameter definitions in the following macro.
(begin-for-syntax
  (define-syntax-class optional-param-def
    #:literals (define Parameterof)
    (pattern (define name:id : (Parameterof type) _)
             #:attr names #'(name)
             #:attr types #'(type))
    (pattern _
             #:attr names #'()
             #:attr types #'())))

;; This macro creates a definition for a function (reconfigure :
;; (Listof (Pair Symbol Any)) -> Void) which will effectfully update
;; any parameter whose defintion was found in the body of this macro
;; and which has a association in the alist.

(module+ examples
  (with-reconfigurable-parameters
    (define foo : (Parameterof (Option Integer)) (make-parameter #f))
    (define bar : (Parameterof (Listof Any)) (make-parameter '())))
  (reconfigure '((foo . 5)(baz "This is ignored")))
  (foo) #| => 5   |#
  (bar) #| => '() |#)

(define-syntax (with-reconfigurable-parameters stx)
  (syntax-parse stx
    #:literals (define Parameterof)
    [(_ (~and any* op:optional-param-def) ...)
     (with-syntax ([((n* ...) ...) #'(op.names ...)]
                   [((t* ...) ...) #'(op.types ...)]
                   [reconfigure (datum->syntax stx 'reconfigure)]
                   [displayconf (datum->syntax stx 'display-configuration)])
       #'(begin
           any* ...
           (define (reconfigure [ls : (Listof (Pair Symbol Any))])
             (define-syntax-rule (update p t)
               (let ([r? (assoc 'p ls)])
                 (when (pair? r?)
                   (p (cast (cdr r?) t)))))
             (define (handle-type-error e)
               (error 'grift "invalid configuration: ~a" ls))
             (with-handlers ([exn? handle-type-error])
               (update n* t*)
               ...
               ...))
           (define (displayconf [port : Output-Port (current-output-port)])
             (pretty-display `((n* . ,(n*)) ... ...) port))))]))

(with-reconfigurable-parameters
  ;; How blame is tracked
  (define-type Blame-Semantics (U 'Lazy-D))
  (define blame-semantics : (Parameterof Blame-Semantics)
    (make-parameter 'Lazy-D))

  ;; How casts are represented
  (define-type Cast-Representation
    (U '|Type-Based Casts| 'Coercions 'Hyper-Coercions 'Static))
  (define cast-representation : (Parameterof Cast-Representation)
    (make-parameter 'Coercions))

  (define program-must-be-statically-typed? : (Parameterof Boolean)
    (make-parameter #f))

  ;; Optimizations options
  (: dynamic-operations? (Parameterof (U Boolean 'inline)))
  (define dynamic-operations? (make-parameter #t))
  (: direct-fn-cast-optimization? (Parameterof Boolean))
  (define direct-fn-cast-optimization? (make-parameter #t))
  (: check-asserts? (Parameterof Boolean))
  (define check-asserts? (make-parameter #f))

  ;; Vector Behavior
  (define bounds-checks? : (Parameterof Boolean)
    (make-parameter #t))
  (define emit-vars-with-original-source-location? : (Parameterof Boolean)
    (make-parameter #f))
  (define-type Ref-Semantics (U 'Monotonic 'Proxied))
  (define reference-semantics : (Parameterof (U 'Monotonic 'Proxied))
    (make-parameter 'Proxied))
  (: inline-guarded-branch? (Parameterof Boolean))
  (define inline-guarded-branch? (make-parameter #t))

  ;; Cast behavior
  ;; TODO this is largely setup based on how the main
  ;; function is designed we should edit this and the
  ;; main function to give the best performance by default
  ;; and options to enable non-optimized configurations
  (: specialize-cast-code-generation? (Parameterof Boolean))
  ;; TODO if this is faster we should default to this
  (define specialize-cast-code-generation?
    (make-parameter #f))
  (: optimize-first-order-coercions? (Parameterof Boolean))
  ;; TODO if this doesn't matter anymore we should nix it
  (define optimize-first-order-coercions? (make-parameter #t))
  (: coercions-are-space-efficient? (Parameterof Boolean))
  (define coercions-are-space-efficient? (make-parameter #t))
  ;; TODO if this is faster we should default to this
  (define hybrid-cast/coercion-runtime? : (Parameterof Boolean)
    (make-parameter #f))



  ;; Default places for everything, but there is no default source
  (define c-path : (Parameterof (Option Path))
    (make-parameter #f))
  (define s-path : (Parameterof (Option Path))
    (make-parameter #f))
  (define output-path : (Parameterof (Option Path))
    (make-parameter #f))
  (define init-heap-kilobytes : (Parameterof Natural)
    (make-parameter 1024))
  (define init-types-hash-table-slots : (Parameterof Natural)
    (make-parameter 50))
  (define types-hash-table-load-factor : (Parameterof Flonum)
    (make-parameter 0.75))
  (define-type GC (U 'Boehm 'None))
  (define garbage-collector : (Parameterof GC)
    (make-parameter 'Boehm))



  ;; Interaction with the c compiler
  (define c-flags : (Parameterof (Listof String))
    (make-parameter '("-Wno-int-conversion" "-Wno-format" "-Wno-unused-value")))
  ;; where is the runtime to be used located
  (define runtime-path : (Parameterof (Option Path)) (make-parameter #f))
  (define hashcons-path : (Parameterof (Option Path)) (make-parameter #f)))
