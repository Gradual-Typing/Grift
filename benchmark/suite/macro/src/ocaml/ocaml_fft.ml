
let () =
  let n = read_int () in
  let data = Array.make n 0.0 in
  let pi_2 = 6.28318530717959 in
  let rec loop i j =
    if i < n
    then
      ((if i < j then
          let tmp = data.(i) in
          data.(i) <- data.(j);
          data.(j) <- tmp
        else
          let tmp = data.(i+1) in
          data.(i+1) <- data.(j + 1);
          data.(j+1) <- tmp);
       loop2 (n/2) j i)
  and loop2 m j i =
    if (m >= 2) && (j >= m)
    then loop2 (m/2) (j-m) i
    else loop (i+2) (j+m)
  and loop3 mmax =
    if mmax < n then
      let theta = pi_2 /. (float_of_int mmax) in
      let wpr = let x = sin (0.5 *. theta) in
                -2.0 *. (x *. x) in
      let wpi = sin theta in
      loop4 1.0 0.0 0 mmax wpr wpi;
      loop3 (mmax * 2)
  and loop4 wr wi m mmax wpr wpi =
    if m < mmax then
      loop5 m mmax wr wi m wpr wpi
  and loop5 i mmax wr wi m wpr wpi =
    if i < n then
      let j = i + mmax in
      let tmpr = (wr *. data.(j)) -. (wi *. data.(j+1)) in
      let tmpi = (wr *. data.(j+1)) +. (wi *. data.(j)) in
      data.(j) <- data.(i) -. tmpr;
      data.(j+1) <- data.(i+1) -. tmpi;
      data.(i) <- data.(i) +. tmpr;
      data.(i+1) <- data.(i+1) +. tmpi;
      loop5 (j+mmax) mmax wr wi m wpr wpi
    else
      loop4 (((wr *. wpr) -. (wi *. wpi)) +. wr)
            (((wi *. wpr) +. (wr *. wpi)) +. wi)
            (m + 2) mmax wpr wpi in
  loop 0 0; (* bit-reversal section *)
  loop3 2;(* Danielson-Lanczos section *)
  Printf.printf "%f\n" data.(0)
