#lang racket/base

(require
 racket/match
 (for-syntax racket/base racket/syntax))

(define-syntax (define-grift-parameter stx)
  (syntax-case stx ()
    [(_ name default #:expect '(datum* ...))
     #'(define name
         (make-parameter
          default
          (λ (x)
            (case x
              [(datum* ...) x]
              [else (error 'name "expected one of: ~a \r\trecieved: ~a" '(datum* ...) x)]))))]
    [(_ name default #:pred pred?)
     #'(define name
         (make-parameter
          default
          (λ (x)
            (cond
              [(pred? x) x]
              [else (error 'name "expected: ~a\trecieved: ~a" 'pred? x)]))))]))

(define-syntax (define-grift-parameter-map stx)
  (syntax-case stx ()
    [(_ def* ...)
     (let ()
       (define def*-ls (syntax->list #'(def* ...)))
       (define (pdef? x)
         (syntax-case x (define-grift-parameter)
           [(define-grift-parameter . _) #t]
           [_ #f]))
       (define pdef*-ls (filter pdef? def*-ls))
       (define/with-syntax (p* ...)
         (for/list ([d def*-ls])
           (with-syntax ([(_ p . _) d])
             #'p)))
       (define/with-syntax gpm (datum->syntax stx 'grift-parameter-map))
       #'(begin
           def* ...
           (provide p* ... gpm)
           (define gpm
             (make-hash `((p* . ,p*) ...)))))]))

(provide list-of-strings?)
(define (list-of-strings? x)
  (and (list? x) (andmap string? x)))

(define ((optional pred?) x)
  (or (not x) (pred? x)))

(define-grift-parameter-map
  ;; How blame is tracked
  (define-grift-parameter blame-semantics 'Lazy-D #:expect '(Lazy-D))
  ;; How casts are represented
  (define-grift-parameter cast-representation 'Coercions
    #:expect '(|Type-Based Casts| Coercions Hyper-Coercions Static ))

  (define-grift-parameter program-must-be-statically-typed? #f #:pred boolean?)

  ;; Optimizations options
  (define-grift-parameter dynamic-operations? #t #:expect '(#t #f inline))
  (define-grift-parameter direct-fn-cast-optimization? #t #:pred boolean?)

  (define-grift-parameter check-asserts? #f #:pred boolean?)

  ;; Vector Behavior
  (define-grift-parameter bounds-checks? #t #:pred boolean?)
  (define-grift-parameter emit-vars-with-original-source-location? #f #:pred boolean?)
  (define-grift-parameter reference-semantics 'Proxied #:expect '(Monotonic Proxied))
  (define-grift-parameter fn-proxy-representation 'Hybrid #:expect '(Hybrid Data))
  (define-grift-parameter inline-proxied-branch? #t #:pred boolean?)
  (define-grift-parameter inline-monotonic-branch? #t #:pred boolean?)

  ;; Cast behavior
  ;; TODO this is largely setup based on how the main
  ;; function is designed we should edit this and the
  ;; main function to give the best performance by default
  ;; and options to enable non-optimized configurations
  (define-grift-parameter specialize-cast-code-generation? #t #:pred boolean?)
  (define-grift-parameter optimize-first-order-coercions? #t #:pred boolean?)
  (define-grift-parameter coercions-are-space-efficient? #t #:pred boolean?)
  (define-grift-parameter hybrid-cast/coercion-runtime? #f #:pred boolean?)
  (define-grift-parameter enable-tail-coercion-composition? #t #:pred boolean?)
  (define-grift-parameter cast-profiler? #f #:pred boolean?)

  
  ;; Default places for everything, but there is no default source
  (define-grift-parameter ir-code-path #f #:pred (optional path?))
  (define-grift-parameter s-path #f #:pred (optional path?))
  (define-grift-parameter output-path #f #:pred (optional path?))
  ;; Runtime Parameters
  (define-grift-parameter init-heap-kilobytes 1024 #:pred exact-nonnegative-integer?)
  (define-grift-parameter init-types-hash-table-slots 50 #:pred exact-nonnegative-integer?)
  (define-grift-parameter types-hash-table-load-factor 0.75 #:pred flonum?)
  (define-grift-parameter garbage-collector 'Boehm #:expect '(None Boehm))


  ;; Select Backend Code Generator
  (define-grift-parameter backend 'C #:expect '(LLVM C))

  ;; Configuring the C Backend
  (define-grift-parameter c-flags
    '("-Wno-int-conversion" "-Wno-format" "-Wno-unused-value")
    #:pred list-of-strings?)

  ;; Configuring the LLVM Backend
  (define-grift-parameter optimize-tail-calls? #t #:pred boolean?)
  (define-grift-parameter calling-convention 'Fast #:expect '(C Fast))


  ;; where is the runtime to be used located
  (define-grift-parameter runtime-path #f #:pred (optional path?))
  (define-grift-parameter hashcons-path #f #:pred (optional path?))

  ;; Check Invarients While Compiling
  (define-grift-parameter with-contracts #f #:pred boolean?)
  (define-grift-parameter with-debug-symbols #f #:pred boolean?)

  ;; Filter applied to logging macros : Setting it to a file name white
  ;; lists debugging macros from that file and blacklist everything else.
  ;; You can use glob notation and a relative path to white list multiple
  ;; files at the same time.
  ;; Not you still have to direct grift to save or print the log.
  ;; For example: "src/grift/*" allows only output from debugging macros
  ;; for parsing and typechecking.
  (define-grift-parameter grift-logger-filter #f #:pred (optional string?))
  ;;(define-type Log-Level (U 'none 'fatal 'error 'warning 'info 'debug))
  (define-grift-parameter grift-log-level 'none #:expect '(none fatal error warning info debug))
  (define-grift-parameter grift-log-port #f #:pred (optional output-port?)))

(provide symbol->grift-parameter)
(define (symbol->grift-parameter x) 
  (hash-ref grift-parameter-map x))

(provide call-with-grift-parameterization)
(define (call-with-grift-parameterization params th)
  (let loop ([p* params])
    (match p*
      ['() (th)]
      [(cons (cons k v) rest)
       (define p
         (cond
           [(symbol? k) (symbol->grift-parameter k)]
           [(string? k) (symbol->grift-parameter (string->symbol k))]
           [(parameter? k) k]
           [else (raise-argument-error
                  'call-with-grift-parameterization
                  "symbol?, string?, or parameter?" k)]))
       (parameterize ([p v])
         (loop rest))]
      [_ (raise-argument-error
          'call-with-grift-parameterization
          "list of grift-parameter and value pairs"
          params)])))

