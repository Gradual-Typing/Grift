(* 9/27/2017 - added types for typed racket *)
(* 10/5/2017 - ported to ocaml *)

let rec tak x y z =
  if y >= x then z
  else tak (tak (x - 1) y z)
           (tak (y - 1) z x)
           (tak (z - 1) x y)

let () =
  let x = read_int () in
  let y = read_int () in
  let z = read_int () in
  Printf.printf "%d\n" (tak x y z)
