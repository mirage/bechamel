external clock_mach_get_time : unit -> int64 = "clock_mach_get_time"

let get () = clock_mach_get_time ()
