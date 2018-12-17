external clock_linux_get_clock_id :
  unit -> int * int * int * int * int * int * int option * int option
  = "clock_linux_get_clock_id"

let ( realtime
    , monotonic
    , realtime_coarse
    , monotonic_coarse
    , monotonic_raw
    , boot_time
    , process_cpu_time
    , thread_cpu_time ) =
  clock_linux_get_clock_id ()

external clock_linux_get_time : int -> int64 = "clock_linux_get_time"

let get () = clock_linux_get_time monotonic
