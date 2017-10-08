let swap a i j =
  if i <> j then
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
    
let partition a p r =
  let i : int ref = ref (p - 1) in
  let x = a.(r) in
  for j = p to r - 1 do
    if a.(j) <= x then
      (i := !i + 1;
       swap a !i j)
  done;
  swap a (!i + 1) r;
  !i + 1
  
let rec sort a p r =
  if p < r then
    let q = partition a p r in
    sort a p (q - 1);
    sort a (q + 1) r

let () =
  let size = read_int () in
  let a = Array.make size 0 in
  for i = 0 to size - 1 do
    a.(i) <- read_int ()
  done; 
  sort a 0 (size-1);
  Printf.printf "%d\n" a.(size-1)
