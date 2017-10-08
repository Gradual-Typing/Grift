(* 9/27/2017 added types to support typed-racket by Andre Kuhlenschmidt *)
(* 10/1/2017 ported typed-racket benchmark to ocaml Andre Kuhlenschmidt *)

(* 
   Objective Caml is a fast modern type-inferring functional programming language 
   descended from the ML (Meta Language) family.
   O'Caml is as fast as C/C++ 
   http://www.ocaml.org/
*)

let inv_sqrt_2x_pi = 0.39894228040143270286

let cummulative_normal_distribution inputX = 
  let sign = inputX < 0.0 in
  let x_input = if sign then inputX *. -1.0 else inputX in
  let exp_values = exp (-0.5 *. x_input *. x_input) in
  let n_prime_of_x = exp_values *. inv_sqrt_2x_pi in
  let x_k2 = 1.0 /. (1.0 +. (0.2316419 *. x_input)) in
  let x_k2_2 = x_k2   *. x_k2 in
  let x_k2_3 = x_k2_2 *. x_k2 in
  let x_k2_4 = x_k2_3 *. x_k2 in
  let x_k2_5 = x_k2_4 *. x_k2 in
  let x1 =  0.319381530 *. x_k2 in
  let x2 = -0.356563782 *. x_k2_2 in
  let x3 =  1.781477937 *. x_k2_3 in 
  let x4 = -1.821255978 *. x_k2_4 in
  let x5 =  1.330274429 *. x_k2_5 in 
  let x  = x1 +. (x5 +. (x4 +. (x2 +. x3))) in
  let x  = 1.0 -. (x *. n_prime_of_x) in
  if sign then 1.0 -. x else x

let black_scholes spot strike rate volatility time option_type timet =
  let logp = log (spot /. strike) in
  let pow = 0.5 *. (volatility *. volatility) in
  let den = volatility *. (sqrt time) in
  let d1 = (logp +. (time *. (rate +. pow))) /. den in
  let d2 = d1 -. den in
  let n_of_d1 = cummulative_normal_distribution d1 in
  let n_of_d2 = cummulative_normal_distribution d2 in
  let fut_val = strike *. (exp (-1.0 *. (rate *. time))) in
  let price =
    if option_type = 0
    then (spot *. n_of_d1) -. (fut_val *. n_of_d2)
    else (fut_val *. (1.0 -. n_of_d2)) -. (spot *. (1.0 -. n_of_d1))
  in
  price

 
type stock_option =
  { mutable spot_price   : float;
    mutable strike_price : float;
    mutable rfi_rate     : float;
    mutable divr         : float;
    mutable volatility   : float;
    mutable time         : float;
    mutable option_type  : char;
    mutable divs         : float;
    mutable derivegem_val: float}

(*
Haven't figured out how to read a single character from stdin
(define (read-option-type)
  (let ([c (read-char)])
    (when (eof-object? c)
      (error 'blackscholes.rkt "invalid input: expected option type"))
    (if (fx= (char->integer c) (char->integer #\P))
        c
        (if (fx= (char->integer c) (char->integer #\C))
            c
            (if (fx= (char->integer c) (char->integer #\space))
                (read-option-type)
                (error 'blackscholes.rkt "invalid input: expected option type"))))))*)

let make_option spot_price strike_price rfi_rate divr
  volatility time option_type divs derivegem_val =
  {spot_price = spot_price;   
   strike_price = strike_price; 
   rfi_rate = rfi_rate;     
   divr = divr;         
   volatility = volatility;   
   time = time;         
   option_type = option_type;  
   divs = divs;         
   derivegem_val = derivegem_val}
    
let read_option () =
  Scanf.bscanf
    Scanf.Scanning.stdin
    "%f %f %f %f %f %f %c %f %f\n"
    make_option

let number_of_runs = 100
let fake_data = 
  {spot_price   = 0.0;   
   strike_price = 0.0; 
   rfi_rate     = 0.0;     
   divr         = 0.0;         
   volatility   = 0.0;
   time         = 0.0;
   option_type  = 'P';
   divs         = 0.0;
   derivegem_val= 0.0}

let () =
  let number_of_options = read_int () in
  let data = Array.make number_of_options fake_data in
  for i = 0 to number_of_options - 1 do
    data.(i) <- read_option ()
  done;
  (* This is really dumb but it is from the original benchmark *)
  let spots = Array.make number_of_options 0.0 in
  let strikes = Array.make number_of_options 0.0 in
  let rates = Array.make number_of_options 0.0 in
  let volatilities = Array.make number_of_options 0.0 in
  let otypes = Array.make number_of_options 0 in
  let otimes = Array.make number_of_options 0.0 in
  for i = 0 to number_of_options - 1 do
    let od = data.(i) in 
    otypes.(i) <- if od.option_type = 'P' then 1 else 0;
    spots.(i) <- od.spot_price;
    strikes.(i) <- od.strike_price;
    rates.(i) <- od.rfi_rate;
    volatilities.(i) <- od.volatility;
    otimes.(i) <- od.time;
  done;
  let prices = Array.make number_of_options 0.0 in
  for j = 0 to number_of_runs - 1 do
    for i = 0 to number_of_options - 1 do
      prices.(i) <- black_scholes spots.(i) strikes.(i) rates.(i)
                                  volatilities.(i) otimes.(i) otypes.(i) 0.0
    done
  done;
  for i = 0 to number_of_options - 1 do
    Printf.printf "%.*f\n" 18 prices.(i);
  done
