external clock_mach_get_time : unit -> int64 = "clock_mach_get_time"
external clock_mach_init : unit -> unit = "clock_mach_init"

let () = clock_mach_init ()
let get () = clock_mach_get_time ()
