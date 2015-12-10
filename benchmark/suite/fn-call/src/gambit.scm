(letrec ([f (lambda (n) n)])
  (time
   (let loop (i 0)
     (if (< i 1000000)
         (f 0)
         (loop (- i 1))))))
