(*
int clock_getres(clockid_t clk_id, struct timespec *res);
int clock_gettime(clockid_t clk_id, struct timespec *tp);
int clock_settime(clockid_t clk_id, const struct timespec *tp);
struct timespec {
 time_t   tv_sec;        /* seconds */
 long     tv_nsec;       /* nanoseconds */
};
 *)   
(*type timespec
let timespec : timspec structure typ = structure "timespec"
let tv_sec  = field timspec "tv_sec"  time_t
let tv_usec = field timspec "tv_nsec" long
seal timeval
 *)                                        

(*
let gettime () = 0.0
 *)
let time f x =
  let start = Unix.gettimeofday () in
  f x;
  let stop = Unix.gettimeofday () in
  Printf.printf "time (sec): %f\n" (stop -. start)
    

