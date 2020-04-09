#lang racket
;; The intention of the test is to ensure that sham is installed and
;; that the llvm portion is working. Does our install script
;; successfully install sham and if the llvm is present can we
;; generate code. Sham is build around a JIT system which I don't care
;; about working correctly, and in fact doesn't work on my laptop. The
;; sham test suite would check the functionality of the JIT.
(module+ test
  (require
   racket/runtime-path
   rackunit
   ffi/unsafe
   sham/ast
   sham/ir
   sham/env
   sham/llvm/ffi/all
   sham/llvm/llvm-config)

  (llvm-initialize-all)
  (define context (LLVMContextCreate))

  (define module
    (env-get-llvm-module
     (build-llvm-module
      (dmodule
       (hash)
       'module
       (list
        (function^ 'main () i64
                   (block^ (app^ (re #f 'printf i64 (list i8*))
                                 (cstring "atan: %f\n")
                                 (app^ (re #f 'atan f64)
                                       (fl64 10.0)))
                           (return (si64 42)))))))))

  (define-runtime-path basename "./test42")
  (define bc-path (path-add-extension basename ".bc"))
  (define bin-path (llvm-config "--bindir"))
  (define asm-path (path-add-extension basename ".s"))

  (check-equal?
   0
   (LLVMWriteBitcodeToFile module (path->string bc-path)))

  (check-true
   (system*
    (build-path bin-path "llc")
    "-O3"
    "--relocation-model=pic"
    bc-path))

  (check-true
   (system*
    (build-path bin-path "clang")
    asm-path
    "-lm"
    "-o"
    basename))

  (check-equal?
   42
   (system*/exit-code basename))

  (delete-file basename)
  (delete-file bc-path)
  (delete-file asm-path)
  (LLVMDisposeModule module)
  (LLVMContextDispose context))
