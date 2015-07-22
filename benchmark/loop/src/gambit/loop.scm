(declare
 (standard-bindings)
 (extended-bindings)
 (block))

(let ([args (command-line)]
      [err  (lambda ()
              (display "Usage: loop.scm <fixnum>\n")
              (exit 1))])
  (cond
   [(not (= (length args) 2)) (err)]
   [(string->number (cadr args)) =>
    (lambda (iters)
      (cond
       [(not (fixnum? iters)) (err)]
       [else
        (time
         (let loop ([i iters])
           (if (fx= 0 i)
               '()
               (loop (fx- i 1)))))]))]
   [else (err)]))
