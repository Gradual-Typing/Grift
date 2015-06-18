#lang typed/racket

(require "./language.rkt"
         "./helpers.rkt")
(require "./schml/reduce-to-cast-calculus.rkt"
         "./casts/impose-cast-semantics.rkt"
         "./data/convert-representation.rkt"
         "./backend-c/code-generator.rkt")
(provide (all-defined-out))
(provide (struct-out Config))

;; Default places for everything, but there is no default source
(define c-path : (Parameterof Path)
  (make-parameter (build-path "a.c")))
(define keep-c? : (Parameterof Boolean)
  (make-parameter #f))
(define asm-path : (Parameterof (Option Path))
  (make-parameter #f))
(define target-path : (Parameterof Path)
  (make-parameter (build-path "a.out")))
(define log-path : (Parameterof (Option Path))
  (make-parameter #f))

;; Semantic Option
(define semantics : (Parameterof Semantics)
  (make-parameter 'Lazy-D))

;; Interaction with the c compiler
(define c-flags : (Parameterof (Listof String))
  (make-parameter '()))

;; This is the main compiler it is composed of several micro
;; compilers for successivly lower level languages.
(: compile/conf (Path Config . -> . Path))
(trace-define (compile/conf path config)
  (let* (;; read(lex), parse, typecheck, insert casts
         [c0  : Cast0-Lang  (reduce-to-cast-calculus path config)]
         ;; specify behavior/representation of casts, and all language constructs
         [d0  : Data0-Lang (impose-cast-semantics c0 config)]
         ;; change how the language represents expressions to get a c like ast
         [uil : Data5-Lang (convert-representation d0 config)])
    ;; generate c code and compile it
    (c-backend-generate-code uil config)))

;; compile file at path
;; exec-path = path/name of final executable
(: compile (-> (U String Path) Path))
(trace-define (compile path)
  (let* ([path  (simple-form-path (if (string? path) (string->path path) path))])
    (let ([log-path (log-path)])
      (when log-path
        (current-log-port (open-output-file log-path #:exists 'replace #:mode 'text)))
      (parameterize ([print-as-expression #t]
                     [print-graph #t]
                     [print-struct #t])
        (compile/conf path
                      (Config path
                              (semantics)
                              (target-path)
                              (c-path)
                              (keep-c?)
                              (c-flags)
                              (asm-path)))))))

(: envoke-compiled-program (->* () (#:exec-path (Option Path) #:config (Option Config)) Boolean))
(define (envoke-compiled-program #:exec-path [path #f] #:config [config #f])
  (cond
   [config (system (path->string (Config-exec-path config)))]
   [path   (system (path->string path))]
   [else   (system (path->string (target-path)))]))

#;
(module+ main
  (let ([args (current-command-line-arguments)])
    (cond
      [(= 0 (vector-length args)) (display "please specify what file to run!\n")]
      [(< 1 (vector-length args)) (display "please only specify one file to run!\n")]
      [(compile (vector-ref args 0)) (display "success :)\n")]
      [else (display "success :)\n")])))
