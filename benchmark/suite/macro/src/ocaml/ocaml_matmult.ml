
let create l1 l2 =
  let x = Array.make (l1 * l2) 0 in
  for i = 0 to l1 - 1 do
    for j = 0 to l2 - 1 do
      x.((l2 * i) + j) <- j + 1
    done
  done;
  x

let mult x x1 x2 y y1 y2 =
  let r = Array.make (y2 * x1) 0 in
  for i = 0 to x1 - 1 do
    for j = 0 to y2 - 1 do
      if j < y2 then
        for k = 0 to y1 -1 do
          r.(i * y2 + j) <- r.(i*y2+j) + (x.(i * x2 + k) * y.(k * y2 + j))
        done
    done
  done;                        
  r

let () =
  let size = read_int () in
  let ar = size in
  let ac = size in
  let br = size in
  let bc = size in
  let a = create ar ac in
  let b = create br bc in
  let r = mult a ar ac b br bc in
  Printf.printf "%d" r.(ar * bc - 1)

