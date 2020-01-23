#lang racket

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
        (function^
         'main () i64
         (return (ui64 42))))))))

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
