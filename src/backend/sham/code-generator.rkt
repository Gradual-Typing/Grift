#lang typed/racket/no-check

(require
 ffi/unsafe
 racket/file
 racket/logging
 sham/ast
 sham/ir
 sham/env
 (only-in sham/logging sham-logger)
 sham/llvm/ffi/all
 sham/llvm/llvm-config
 syntax/location
 "../../configuration.rkt"
 "../../logging.rkt"
 "./generate-sham.rkt"
 "../c/code-generator.rkt"
 (for-syntax racket/system)
 (for-syntax "../runtime-location.rkt")
 "../runtime-location.rkt"
 "../lib.rkt")

(provide
 generate-code)

(llvm-initialize-all)

(define (llvm-call name)
  (define path (build-path (llvm-config "--bindir") name))
  (lambda a (apply system* path a)))

(define llc (llvm-call "llc"))
(define opt (llvm-call "opt"))
(define clang (llvm-call "clang"))

;; Basic driver for the entire backend
(: generate-code (Data5-Lang . -> . Path))
(define (generate-code uil) 
  (debug uil)
  (let* ([o-path (get-write-file (build-path "a") "" (output-path))]
         [keep-ll-code-file? (not (not (ir-code-path)))]
         [keep-s? (not (not (s-path)))]
         [ll-path (get-write-file o-path ".ll" (ir-code-path))]
         [s-path (get-write-file o-path ".s" (s-path))])

    ;; Empty LLVM State
    (define context (LLVMContextCreate))
    ;; Create The Sham IR
    (define sham-module (generate-sham uil))
    (debug sham-module)
    ;; Create the LLVM Module using sham
    (define llvm-module
      (let ([th (λ () (env-get-llvm-module (build-llvm-module sham-module)))])
        (cond
          [(grift-log-port)
           =>
           (λ (p)
             (with-logging-to-port p th (grift-log-level) #:logger sham-logger))]
          [else (th)])))

    #;
    (let-values ([(error? messages) (LLVMVerifyModule llvm-module 'LLVMReturnStatusAction)])
      (when error?
        (error
         'grift/backend/sham/generate-code
         (format
          "Grift Internal Error: couldn't verify llvm module\nLLVM: ~a"
          messages))))

    (match-let ([(vector error? error-message) (LLVMPrintModuleToFile llvm-module ll-path)])
      (when error?
        (error 'grift/backend/sham/generate-code
               (format
                (string-join
                 `("Grift Internal Error: couldn't generate llvm module file: ~a"
                   "LLVM: ~a")
                 "\n")
                ll-path
                error-message))))
    
    (when (optimize-tail-calls?)
      (unless (opt "-tailcallopt" "-tailcallelim" "-S" "-o" ll-path ll-path)
        (error 'grift/backend/sham/generate-code "failed to optimize tailcalls in llvm module")))
    
    (when keep-s?
      (clang  ll-path "-O3" "-S" "-o" s-path))

    (clang "-o" o-path
           (if (optimize-ir?) "-O3" "")
           (if (with-debug-symbols) "-g" "")
           "-Wno-override-module" ;; Keep us from emitting a warning that I think is harmless
           (format "-DINIT_TYPES_HT_SLOTS=~a" (init-types-hash-table-slots))
           (format "-DTYPES_HT_LOAD_FACTOR=~a" (types-hash-table-load-factor))
           (format "-DGC_INITIAL_HEAP_SIZE=~a" (* (init-heap-kilobytes) 1024))
           (if (cast-profiler?) "-DCAST_PROFILER" "")
           runtime-entry.c-path
           ll-path
           "-lm" runtime.o-path
           boehm-gc.a-path "-pthreads")

    (unless (or (with-debug-symbols) keep-ll-code-file?)
      (delete-file ll-path))
    
    o-path))


(define (runtime-entry-flags)
  (list
   (format "-DINIT_TYPES_HT_SLOTS=~a" (init-types-hash-table-slots))
   (format "-DTYPES_HT_LOAD_FACTOR=~a" (types-hash-table-load-factor))
   (format "-DGC_INITIAL_HEAP_SIZE=~a" (* (init-heap-kilobytes) 1024))
   (if (cast-profiler?) "-DCAST_PROFILER" "")))

