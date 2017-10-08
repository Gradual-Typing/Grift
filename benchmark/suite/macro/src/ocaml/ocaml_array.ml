(* taken from ... *)
(* ARRAY1 -- One of the Kernighan and Van Wyk benchmarks. *)
(* 9/27/2017 added types to support typed-racket by Andre Kuhlenschmidt *)
(* 9/28/2017 ported to ocaml *)
let create_x n =
  let result = Array.make n 0 in
  for i = 0 to n - 1 do
    result.(i) <- i;
  done;
  result
  
let create_y x =
  let n = Array.length x in
  let result = Array.make n 0 in
  for i = n - 1 downto 0 do
    result.(i) <- x.(i)
  done;
  result

let my_try n = Array.length (create_y (create_x n))

let rec go m n r =
  if m > 0 then
    go (m - 1) n (my_try n)
  else r

let () =
  let input1 = int_of_string(input_line stdin) in
  let input2 = int_of_string(input_line stdin) in
  Printf.printf "%d\n" (go input1 input2 0)

