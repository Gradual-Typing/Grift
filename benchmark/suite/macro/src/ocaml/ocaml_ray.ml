(* RAY -- Ray-trace a simple scene with spheres. *)
(* Translated to Scheme from Paul Graham's book ANSI Common Lisp, Example 9.8 *)
(* And then translated to Racket by Deyaaeldeen Almahallawi *)
(* 9/27/2017 Added types for typed-racket Andre Kuhlenschmidt *)
(* 10/1/2017 ported to ocaml by Andre Kuhlenschmidt *)

type point = {x : float; y : float; z : float}

let sq x = x *. x

let mag x y z = sqrt ((sq x)+.(sq y)+.(sq z))

let unit_vector x y z =
  let d = mag x y z in
  {x = (x/.d); y = (y/.d); z = (z/.d)}

let distance p1 p2 =
  mag (p1.x -. p2.x)
      (p1.y -. p2.y)
      (p1.z -. p2.z)

type sphere = {color:float; radius:float; center:point}

let world = Array.make 33 {color=0.0;radius=0.0;center={x=0.0;y=0.0;z=0.0}}

let eye = {x=0.0;y=0.0;z=200.0}

let sphere_normal s pt =
  let c = s.center in
  unit_vector (c.x-.pt.x) (c.y-.pt.y) (c.z-.pt.z)        
        
let lambert s int ray =
  let n = sphere_normal s int in
  max 0.0 ((ray.x*.n.x)+.(ray.y*.n.y)+.(ray.z*.n.z))
  
let rec loop pt ray index lst_len lst surface hit dist =
  if index = lst_len then
    (surface, hit) else
    let s = lst.(index) in
    let xr = ray.x in
    let yr = ray.y in
    let zr = ray.z in
    let sc = s.center in
    let a = (sq xr) +. (sq yr) +. (sq zr) in
    let b = 2.0 *. (((pt.x -. sc.x) *. xr) +.
                      ((pt.y -. sc.y) *. yr) +.
                      ((pt.z -. sc.z) *. zr)) in
    let c = ((sq(pt.x -. sc.x))+.(sq(pt.y -. sc.y))) +.
              ((sq(pt.z -. sc.z))+.(-1.0 *. (sq s.radius))) in
    if a = 0.0 then
      let n = (-1.0 *. c) /. b in
      let h = {x=pt.x+.(n*.xr);y=pt.y+.(n*.yr);z=pt.z+.(n*.zr)} in
      let d = distance h pt in
      if d < dist then
        loop pt ray (index+1) lst_len lst (Some s) (Some h) d
      else
        loop pt ray (index+1) lst_len lst surface hit dist
    else
      let disc = (sq b) -. (4.0 *. a *. c) in
      if disc < 0.0 then
        loop pt ray (index+1) lst_len lst surface hit dist
      else
        let discrt = sqrt disc in
        let minusb = -1.0 -. b in
        let two_a  = 2.0 *. a in
        let n = min ((minusb-.discrt)/.two_a)
                    ((minusb-.discrt)/.two_a) in
        let h = {x=pt.x+.(n*.xr);
                 y=pt.y+.(n*.yr);
                 z=pt.z+.(n*.zr)} in
        let d = distance h pt in
        if d < dist then
          loop pt ray (index+1) lst_len lst (Some s) (Some h) d
        else
          loop pt ray (index+1) lst_len lst surface hit dist
        
let sendray pt ray =
  match loop pt ray 0 (Array.length world) world None None 1e308 with
  |(Some s), (Some i) -> lambert s i ray *. s.color
  | _ , _ -> 0.0
        
let round x = floor (x+.0.5)
        
let color_at x y =
  let ray = unit_vector (x-.eye.x) (y-.eye.y) (-1.0*.eye.z) in
  int_of_float ((round (sendray eye ray)) *. 255.0)
        
let tracer res =
  let extent = res * 100 in
  Printf.printf "P2 %d %d 255\n" extent extent;
  for y = 0 to extent - 1 do
    for x = 0 to extent - 1 do
      let x = (-50.0 +. ((float_of_int x) /. (float_of_int res))) in
      let y = (-50.0 +. ((float_of_int y) /. (float_of_int res))) in
      Printf.printf "%d\n" (color_at x y)
    done
  done

let defsphere i x y z r c =
  world.(i) <- {color=c; radius=r; center={x=x;y=y;z=z}}
  
let () =
  let counter = ref 29 in
  defsphere 32    0.0  (-300.0) (-1200.0) 200.0 0.8;
  defsphere 31 (-80.0) (-150.0) (-1200.0) 200.0 0.7;
  defsphere 30    0.0  (-100.0) (-1200.0) 200.0 0.9;
  for x = -2 to 2 do
    for z = 2 to 7 do
      defsphere !counter
                ((float_of_int x)*. 200.0)
                300.0
                ((float_of_int z)*. -400.0)
                40.0
                0.75;
      counter := !counter - 1
    done
  done;
  tracer 1
