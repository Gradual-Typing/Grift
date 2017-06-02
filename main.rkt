#!/usr/bin/env racket
#lang racket/base

(require "src/compile.rkt"
	 racket/cmdline
	 racket/match
	 racket/runtime-path
	 racket/system)

(provide (all-from-out "src/compile.rkt"))

(define-runtime-path this-dir ".")
(define (print-version-info)
 (unless (system "git rev-parse --verify HEAD" #:set-pwd? this-dir)
   (error 'schml "This should not be used for non-development versions"))
 (system "git status" #:set-pwd? this-dir))

(module+ main
  (define recursive-parameter (make-parameter #f))
  (define schml-did-something? (make-parameter #f))
  (command-line
   #:program "schml"
   #:once-any
   ["--coercions"
    "Select the coercions representation of casts"
    (cast-representation 'Coercions)]
   ["--hyper-coercions"
    "Select the coercions representation of casts"
    (cast-representation 'Hyper-Coercions)]
   ["--type-based-casts"
    "Select the type-based cast representation of casts"
    (cast-representation '|Type-Based Casts|)]
   [("-R" "--cast-representation")
    cast-rep
    ("select cast runtime representation"
     "default: Coercions")
    (case cast-rep
      [("Type-Based") (cast-representation '|Type-Based Casts|)]
      [("Coercions")  (cast-representation 'Coercions)]
      [("Hyper-Coercions") (cast-representation 'Hyper-Coercions)]
      [else (error 'schml "unrecognized cast representation: ~a" cast-rep)])]
   #:once-each
   [("--check-asserts")
    "Compile coded with assertions"
    (check-asserts? #t)]
   [("--assert-statically-typed")
    "Raise an error unless the program is statically typed"
    (program-must-be-statically-typed?)]
   [("--version")
    "Output schml compiler version info"
    (schml-did-something? #t)
    (print-version-info)]
   #:once-any
   [("--reference-semantics") semantics
    "Monotonic, Proxied (defualt Proxied)"
    (match semantics
      ["Monotonic" (reference-semantics 'Monotonic)]
      ["Proxied"   (reference-semantics 'Proxied)]
      [other (error 'reference-semantics "flag provided: ~v" other)])]
   [("--monotonic-references")
    "vectors and references default to monotonic semantics"
    (reference-semantics 'Monotonic)]
   #:once-each
   [("-o") output-str
    "specify output path for executable"
    (output-path (string->path output-str))]
   [("--keep-c") name
    "keep the c intermediate representation"
    (c-path (build-path name))]
   [("--keep-s") name
    "keep the assembly representation"
    (s-path (build-path name))]
   ["--c-flag" flag
     "pass extra c flag to the C compiler"
    (c-flags (cons flag (c-flags)))]
   [("-O") level
    "set the optimization level"
    (define l (string->number level))
    (c-flags (cons (format "-O~a" l) (c-flags)))]
   [("--disable-bounds-checks")
    "code genererated won't check if indices are in bounds"
    (bounds-checks? #f)]
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
   [("--profile")
    "Invoke c compiler with profiling flags"
    (c-flags (cons "-pg" (c-flags)))]
   #:once-any
   ["--Boehm" "Use Boehm Conservative Collector" (garbage-collector 'Boehm)]
   ["--No-GC" "Do not Collect Garbage"           (garbage-collector 'None)]
   #:args args
   (match args
     [(list)
      (cond
       [(not (schml-did-something?))
        (error 'schml "no input given")])]
     [(list (app string->path (and (not #f) path)))
      (cond
       [(recursive-parameter) (compile-directory path)]
       [else (compile path)])]
     [else (error 'schml "invalid arguments ~a" args)])
     (void)))
