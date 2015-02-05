#lang typed/racket

(require schml/src/language
         schml/src/helpers)
(require schml/src/schml/reduce-to-cast-calculus
         schml/src/casts/impose-cast-semantics
         schml/src/closures/make-closures-explicit
         schml/src/data/convert-representation
         schml/src/backend-c/code-generator)

(provide (all-defined-out))
(provide (struct-out Config))

(: default-exec-path-string String)
(define default-exec-path-string "a.out")
(: default-c-path-string String)
(define default-c-path-string "a.c")

;; This is the main compiler it is composed of several micro
;; compilers for successivly lower level languages. 
(: compile/conf (Path Config . -> . Boolean))
(define (compile/conf path config)
  (when (trace? 'Source 'All 'Vomit)
    (logf "Source:\n~a\n\n" (file->string path #:mode 'text)))
  (let* (;; read(lex), parse, typecheck, insert casts
         [c0  : Cast0-Lang (reduce-to-cast-calculus path config)]
         [_   (when (trace? 'Cast0 'All 'Vomit) (logf "Cast0:\n~v\n\n" c0))]

         ;; lower casts into a weakly typed language with lexical closures
         [l0  : Lambda0-Lang (impose-cast-semantics c0 config)]
         [_   (when (trace? 'Lambda0 'All 'Vomit) (logf "Lambda0:\n~a\n\n" l0))]

         ;; convert lambdas to flat functions and closure data structures
         [d0  : Data0-Lang (make-closures-explicit l0 config)]
         [_   (when (trace? 'Data0 'All 'Vomit) (logf "Data0:\n~v\n\n" d0))]

         ;; change how the language is representated in order to make
         ;;    the conversion to c easy
         [uil : Data2-Lang (convert-representation d0 config)]
         [_   (when (trace? 'UIL0 'All 'Vomit) (logf "UIL0:\n~v\n\n" uil))])
    (c-backend-generate-code uil config)))

;; compile file at path
;; exec-path = path/name of final executable
(: compile (->* ((U String Path))
                (#:exec-path Path #:c-path Path #:semantics Semantics #:log-path (Option Path))
                Boolean))
(define (compile path
     #:exec-path [exec-path (string->path default-exec-path-string)]
     #:c-path    [c-path    (string->path default-c-path-string)]
     #:log-path  [log-path  #f]
     #:semantics [semantics 'Lazy-D])
  (let* ([path  (simple-form-path (if (string? path) (string->path path) path))])
    (let ([th (lambda () (compile/conf path (Config semantics exec-path c-path)))])
      (if log-path
          (call-with-output-file
              log-path #:exists 'replace #:mode 'text
              (lambda ([log : Output-Port])
                (parameterize ([current-log-port log]
                               [print-as-expression #t]
                               [print-graph #t]
                               [print-struct #t])
                  (th))))
          (th)))))


(: envoke-compiled-program (->* () (#:exec-path (Option Path) #:config (Option Config)) Boolean))
(define (envoke-compiled-program #:exec-path [path #f] #:config [config #f])
  (cond
   [config (system (path->string (Config-exec-path config)))]
   [path   (system (path->string path))]
   [else   (system default-exec-path-string)]))

(module+ main
  (let ([args (current-command-line-arguments)])
    (cond
     [(= 0 (vector-length args)) (display "please specify what file to run!\n")]
     [(< 1 (vector-length args)) (display "please only specify one file to run!\n")]
     [(compile (vector-ref args 0)) (display "success :)\n")]
     [else (display "success :)\n")])))
