external clock_linux_get_time : unit -> (int64[@unboxed]) =
  "clock_linux_get_time_bytecode"
  "clock_linux_get_time_native" [@@noalloc]

let get () = clock_linux_get_time ()
