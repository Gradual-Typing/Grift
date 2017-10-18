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
  ;; Todo now that we have a main-function to overide do
  ;; we still need this flag.
  (define grift-did-something? (make-parameter #f))
  (define recursive-parameter (make-parameter #f))
  ;; The function that gets run after command line parsing
  ;; we use a parameter because command line parsing can
  ;; override the main function.
  (define grift-main-function
    (make-parameter
     (lambda (args)
       (match args
         [(list)
          (cond
            [(not (grift-did-something?))
             (error 'grift "no input given")])]
         [(list (app string->path (and (not #f) path)))
          (cond
            [(recursive-parameter) (compile-directory path)]
            [else (compile path)])]
         [else (error 'grift "invalid arguments ~a" args)]))))
  (command-line
   #:program "grift"
   #:once-any
   ["--static"
    "Static varient of lambda calculus" 
    (cast-representation 'Static)]
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
    ((format "select cast representation: (~a)" (cast-representation)))
    (case cast-rep
      [("Static")
       (cast-representation 'Static)]
      [("Type-Based Casts")
       (cast-representation '|Type-Based Casts|)]
      [("Coercions")
       (cast-representation 'Coercions)]
      [("Hyper-Coercions")
       (cast-representation 'Hyper-Coercions)]
      [else (error 'grift "unrecognized cast representation: ~a" cast-rep)])]
   #:once-each
   [("--check-asserts")
    ((format "Compile coded with assertions (~a)" (check-asserts?)))
    (check-asserts? #t)]
   [("--assert-statically-typed")
    "Raise an error unless the program is statically typed"
    (program-must-be-statically-typed?)]
   [("--version")
    "Output grift compiler version info"
    (grift-did-something? #t)
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
   #:once-any
   ;; TODO find out if anyone is relying on this flag
   [("--open-coded-casts")
    "turn on cast specialization"
    (specialize-cast-code-generation? #t)]
   [("-S" "--specialize-casts") bool
    "enable(#t)/disable(#f) specializing the code generation of casts"
    (specialize-cast-code-generation?
     (match bool
       ["#t" #t] ["#f" #f]
       [_ (error 'grift "expected boolean flag -S/--specialize-casts")]))]
   #:once-any
   ["--no-dyn-operations"
    "disable optimization of dynamic function, reference, and tuples usage"
    (dynamic-operations? #f)]
   [("-D" "--dyn-operations") bool
    "enable(#t)/disable(#f) optimized code on operators acting on dynamic values"
    (dynamic-operations?
     (match bool
       ["#t" #t] ["#f" #f]
       [_ (error 'grift "expected boolean flag -D/--dyn-operations")]))]
   #:once-each
   [("-H" "--hybrid-coercions") bool
    ("enable(#t)/disable(#f) using mixed cast/coercion runtime"
     "not applicable to type-based casts")
    (hybrid-cast/coercion-runtime?
     (match bool
       ["#t" #t] ["#f" #f]
       [_ (error 'grift "expected boolean flag -H/--hybrid-coercions")]))]
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
   [("--emit-vars-with-original-source-location")
    "rename compiled variables so that they have the source location that generated them"
    (emit-vars-with-original-source-location? #t)]
   [("-m" "--start-memory")
    kilobytes
    "select the runtime's starting heap size"
    (cond
      [(string->number kilobytes) => 
       (lambda (k)
         (if (exact-nonnegative-integer? k)
             (init-heap-kilobytes k)
             (error 'grift "invalid initial heap size: ~a" k)))]
      [else
       (error 'grift "invalid argument given for memory size: ~v" kilobytes)])]
   [("--types-hash-table-slots")
    num
    "select the runtime's starting hash table number of slots"
    (cond
      [(string->number num) => 
       (lambda (k)
         (if (exact-positive-integer? k)
             (init-types-hash-table-slots k)
             (error 'grift "invalid initial types hashtable size: ~a" k)))]
      [else
       (error 'grift "invalid argument given for hashtable size: ~v" num)])]
   [("--types-hash-table-load-factor")
    num
    "select the runtime's hashtable load factor"
    (cond
      [(string->number num) => 
       (lambda (k)
         (if (and (flonum? k) (positive? k))
             (types-hash-table-load-factor k)
             (error 'grift "invalid types hashtable load factor: ~a" k)))]
      [else
       (error 'grift "invalid argument given for hashtable load factor: ~v" num)])]
   [("-r" "--recursive")
    "recursively compile directory"
    (recursive-parameter #t)]
   [("-d" "--debug-logging") debug-file
    "enable debuging logging to file (1=stdout, 2=stderr)"
    (begin
      (schml-log-level 'debug)
      (match debug-file
        ["1" (schml-log-port (current-output-port))]
        ["2" (schml-log-port (current-error-port))]
        [other
         (define of-port (open-output-file other
                                           #:mode 'text
                                           #:exists 'replace))
         (schml-log-port of-port)]))]
   [("-g" "--with-debug-symbols")
    "Invoke c compiler so that debugging symbols are retained."
    (c-flags (cons "-g" (c-flags)))]
   [("--profile")
    "Invoke c compiler with profiling flags"
    (c-flags (cons "-pg" (c-flags)))]
   #:once-any
   ["--Boehm" "Use Boehm Conservative Collector" (garbage-collector 'Boehm)]
   ["--No-GC" "Do not Collect Garbage"           (garbage-collector 'None)]
      #:once-any
   ["--config" file
    ("specify a file from which to read the configuration"
     "this configuration shouldn't conflict with command line flags")
    (define p (string->path file))
    (unless(file-exists? p)
      (error 'grift "file not found: ~v" p))
    (define c (call-with-input-file read p))
    (define (symbolic-association? x)
      (and (pair? x) (symbol? (car x))))
    (unless (and (list? c)
                 (andmap symbolic-association? c))
      (error 'grift "invalid configuration: ~v" c))
    (reconfigure c)]
   ["--display-config"
    "print out the configuration of the compiler"
    (grift-main-function
     (lambda (args)
        (match args
          [(list) (display-configuration (current-output-port))]
          [(list (app string->path (and (not #f) p)))
           (when (file-exists? p)
             (error 'grift
                    "--display-config will not override file: ~a"
                    p))
           (call-with-output-file p display-configuration)])))]
   #:args args
   ((grift-main-function) args)))
