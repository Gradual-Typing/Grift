(let loop ([i 999999999])
   (cond
     [(zero? i) i]
     [else (loop (- i 1))]))
