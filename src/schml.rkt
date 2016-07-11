#lang typed/racket/base

(require "./compile.rkt")

(module+ main
  (define recursive-parameter (make-parameter #f))
  (command-line
   #:program "schml"
   #:once-any
   ["--coercions"
    "Select the coercions representation of casts"
    (cast-rep 'Coercions)]
   ["--type-based-casts"
    "Select the type-based cast representation of casts"
    (cast-rep 'Twosomes)]
   [("-R" "--cast-representation")
    cast-representation
    ("select cast runtime representation"
     "default: Coercions")
    (case (symbol->string cast-representation)
      [(Type-Based) (cast-rep 'Twosomes)]
      [(Coercions)  (cast-rep 'Coercions)]
      [(Hyper-Coercions) (error 'schml "Hyper-Coercions not yet supported")])]
   #:once-each
   ["--no-dyn-operations"
    "disable optimization of dynamic function, reference, and tuples usage"
    (dynamic-operations? #f)]
   [("-m" "--start-memory")
    kilobytes
    "select the runtime's starting heap size"
    (cond
      [(string->number kilobytes)
       ;; TODO make kilobytes the default memory unit
       ;; TODO make a better name of memory default
       (lambda (k) (mem-dflt (* k 1024)))]
      [else
       (error 'schml "invalid argument given for memory size: ~v" kilobytes)])]
   [("-r" "--recursive")
    "recursively compile directory"
    (recursive-parameter #t)]
   #:args (target)
   (cond
     [(string->path target) =>
      (λ (path)
        (cond
          [(recursive-parameter) (compile-recursively path)]
          [else (compile path)]))]
     [else (error 'schml "invalid target path: ~v" target)])))
