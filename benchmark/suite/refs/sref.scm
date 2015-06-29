(set! x 0)
(let loop ([i 999999999])
   (cond
     [(zero? i) '()]
     [else (set! x (+ x i)) (loop (- i 1))]))
