#lang typed/racket/no-check

(require
 ffi/unsafe
 racket/file
 sham/ast
 sham/ir
 sham/env
 sham/llvm/ffi/all
 sham/llvm/llvm-config
 syntax/location
 "../../configuration.rkt"
 "../../logging.rkt"
 "./generate-sham.rkt"
 "../c/code-generator.rkt"
 (for-syntax racket/system)
 (for-syntax "../runtime-location.rkt")
 "../runtime-location.rkt")

(provide
 generate-code)

(llvm-initialize-all)

(define bin-path (llvm-config "--bindir"))
(define llc-path (build-path bin-path "llc"))
(define (llc . a) (apply system* llc-path a))

;; Basic driver for the entire backend
(: generate-code (Data5-Lang . -> . Path))
(define (generate-code uil) 
  (debug uil)
  (let* ([keep-ll-code-file? (ir-code-path)]
         [o-path (normalize-path (or (output-path) (build-path "a.out")))]
         [keep-s? (s-path)]
         [s-path (or keep-s? (path-replace-extension o-path ".s"))])

    ;; Empty LLVM State
    (define context (LLVMContextCreate))
    ;; Create The Sham IR
    (define sham-module (generate-sham uil))
    (debug off sham-module)
    ;; Create the LLVM Module using sham
    (define llvm-module
      (parameterize ([current-logger grift-logger])
        (env-get-llvm-module
         (build-llvm-module sham-module))))

    ;; Compile module with llc
    (define ll-path (path-replace-extension o-path ".ll"))
    (match-define (vector error? error-message)
      (LLVMPrintModuleToFile llvm-module ll-path))
    (when error?
      (error 'grift/backend/sham/generate-code
             (format
              (string-join
               `("couldn't generate llvm module file: ~a"
                 "LLVM: ~a")
               "\n")
              ll-path
              error-message)))
    
    (log-grift-debug-filter
     (format
      "LLVM Module Precompilation:\n~a\n\n"
      (file->string ll-path)))

    (unless (llc "-asm-show-inst" "-asm-verbose" "-O3" ll-path "-o" s-path)
      (error 'grift/backend/sham/generate-code "failed to compile llvm module"))

    (delete-file ll-path)

    (log-grift-debug-filter
     (format
      "LLVM ASM Output:\n~a\n\n"
      (file->string s-path)))

    
    ;; Link the executable
    (cc/runtime o-path s-path (append-flags (append (runtime-entry-flags) (c-flags)))
                #:runtime-entry runtime-entry.c-path)
    
    (unless keep-s?
      (delete-file s-path))
    
    o-path))


(define (runtime-entry-flags)
  (list
   (format "-DINIT_TYPES_HT_SLOTS=~a" (init-types-hash-table-slots))
   (format "-DTYPES_HT_LOAD_FACTOR=~a" (types-hash-table-load-factor))
   (format "-DGC_INITIAL_HEAP_SIZE=~a" (* (init-heap-kilobytes) 1024))
   (if (cast-profiler?) "-DCAST_PROFILER" "")))

