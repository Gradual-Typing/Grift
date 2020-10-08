#!/usr/bin/env racket
#lang racket/base

(require "src/compile.rkt"
         "src/backend/runtime-location.rkt"
	 racket/cmdline
	 racket/match
	 racket/runtime-path
	 racket/system)

(provide (all-from-out "src/compile.rkt"))

(define (make-runtime-with-param param param_runtime.o-path)
  (when (not (directory-exists? param_runtime.o-path))
    (define-values (pwd _1 _2) (split-path runtime.o-path))
    (parameterize ([current-directory pwd])
      (unless (system (string-append "make " param) #:set-pwd? #t)
        (printf (format "\nError: Running make failed in ~a" pwd))))))

(define-runtime-path this-dir ".")
(define (print-version-info)
 (unless (system "git rev-parse --verify HEAD" #:set-pwd? this-dir)
   (error 'grift "This should not be used for non-development versions"))
 (system "git status" #:set-pwd? this-dir))

(module+ main
  (define recursive-parameter (make-parameter #f))
  (define grift-did-something? (make-parameter #f))
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
   #:once-any
   [("--llvm")
    "enable LLVM backend"
    (backend 'LLVM)]
   [("--backend")
    value
    ((format "switch between C and LLVM backends (~a)" (backend)))
    (backend
     (case value
      [("C" "c") 'C]
      [("LLVM" "llvm") 'LLVM]))]
   #:once-each
   ["--data-fn-proxies" "enable inefficient function proxy representation"
    (fn-proxy-representation 'Data)]
   [("--no-optimize-tailcalls")
    "disable LLVM backends tail call optimization"
    (optimize-tail-calls? #f)]
   [("--c-calling-convention")
    "use C calling convention instead of better options"
    (calling-convention 'C)]
   [("--check-asserts")
    ((format "Compile code with assertions (~a)" (check-asserts?)))
    (check-asserts? #t)]
   [("--with-contracts" "-C") "enable contract checking in the compiler" (with-contracts #t)]
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
   [("--keep-ir") name
    "keep the c intermediate representation"
    (ir-code-path (build-path name))]
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
   [("--log-filter")
    filter
    ("filter debugging logging to specific files"
     "can be a filename, glob, or regular expression for a relative path") 
    (grift-logger-filter filter)]
   [("-d" "--debug-logging") debug-file
    "enable debugging logging to file (1=stdout, 2=stderr)"
    (begin
      (grift-log-level 'debug)
      (match debug-file
        ["1" (grift-log-port (current-output-port))]
        ["2" (grift-log-port (current-error-port))]
        [other
         (define of-port (open-output-file other
                                           #:mode 'text
                                           #:exists 'replace))
         (grift-log-port of-port)]))]
   [("--no-inline-proxied-branch")
    "Do not inline proxied operations"
    (inline-proxied-branch? #f)]
   [("--inline-monotonic-branch")
    "Do not inline monotonic operations"
    (inline-monotonic-branch? #f)]
   [("--cast-profiler")
    "Turn on the runtime cast profiler"
    (cast-profiler? #t)]
   #:once-any
   ["--Boehm" "Use Boehm Conservative Collector" (garbage-collector 'Boehm)]
   ["--No-GC" "Do not Collect Garbage"           (garbage-collector 'None)]
   #:once-any
   [("-g" "--with-debug-symbols")
    "Invoke c compiler so that debugging symbols are retained."
    (c-flags (cons "-g" (c-flags)))
    (make-runtime-with-param "debug" debug_runtime.o-path)
    (runtime-path debug_runtime.o-path)]
   [("-p" "--profile")
    "Invoke c compiler with profiling flags"
    (c-flags (cons "-pg" (c-flags)))
    (make-runtime-with-param "profile" profile_runtime.o-path)
    (runtime-path profile_runtime.o-path)]
   #:once-any
   ["--crcps"
    "Enable coercion-passing style translation"
    (cast-representation 'Coercions)
    (specialize-cast-code-generation? #f)
    (enable-tail-coercion-composition? #t)]
   ["--no-crcps" "Disable coercion-passing style translation"
    (enable-tail-coercion-composition? #f)]
   #:args args
   #;
   (when display-grift-configuration?
     (for-each-grift-parameter
      (lambda (k v)
        (printf "~a = ~a\n" k (v)))))
   (match args
     [(list)
      (cond
        [(not (grift-did-something?))
         (error 'grift "no input given")])]
     [(list (app string->path (and (not #f) path)))
      (cond
        [(recursive-parameter) (compile-directory path)]
        [else
         (when (and (with-debug-symbols)
                    (not (ir-code-path)))
           (ir-code-path
            (path-replace-extension
             path
             (case (backend)
               [(C) ".c"]
               [(LLVM) ".ll"]))))
         (compile path)])]
     [else (error 'grift "invalid arguments ~a" args)])
   (void)))
