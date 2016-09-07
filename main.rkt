#lang racket/base

(require "src/compile.rkt" racket/cmdline)

(provide (all-from-out "src/compile.rkt"))

(module+ main
  (define recursive-parameter (make-parameter #f))
  (command-line
   #:program "schml"
   #:once-any
   ["--coercions"
    "Select the coercions representation of casts"
    (cast-representation 'Coercions)]
   ["--type-based-casts"
    "Select the type-based cast representation of casts"
    (cast-representation 'Type-Based)]
   [("-R" "--cast-representation")
    cast-rep
    ("select cast runtime representation"
     "default: Coercions")
    (case cast-rep
      [("Type-Based") (cast-representation 'Type-Based)]
      [("Coercions")  (cast-representation 'Coercions)]
      [("Hyper-Coercions") (error 'schml "Hyper-Coercions not yet supported")]
      [else (error 'schml "unrecognized cast representation: ~a" cast-rep)])]
   #:once-each
   [("--keep-c") name
    "keep the c intermediate representation"
    (c-path (build-path name))]
   [("-O") level
    "set the optimization level"
    (define l (string->number level))
    (when (and (exact-nonnegative-integer? l) (<= 0 l 3))
      (error 'schml "flag given incorrect input ~v" level))
    (c-flags (cons (format "-O~a" l) (c-flags)))]
   [("--open-coded-casts")
    "turn on cast specialization"
    (specialize-cast-code-generation? #t)]
   ["--no-dyn-operations"
    "disable optimization of dynamic function, reference, and tuples usage"
    (dynamic-operations? #f)]
   [("-m" "--start-memory")
    kilobytes
    "select the runtime's starting heap size"
    (cond
      [(string->number kilobytes) => 
       (lambda (k)
         (if (exact-nonnegative-integer? k)
             (init-heap-kilobytes k)
             (error 'schml "invalid initial heap size: ~a" k)))]
      [else
       (error 'schml "invalid argument given for memory size: ~v" kilobytes)])]
   [("-r" "--recursive")
    "recursively compile directory"
    (recursive-parameter #t)]
   [("-g" "--with-debug-symbols")
    "Invoke c compiler so that debugging symbols are retained."
    (c-flags (cons "-g" (c-flags)))]
   #:args (target)
   (cond
     [(string->path target) =>
      (λ (path)
        (cond
          [(recursive-parameter) (compile-directory path)]
          [else (compile path)]))]
     [else (error 'schml "invalid target path: ~v" target)])))
