type kind =
  [ `Realtime
  | `Monotonic
  | `Realtime_coarse
  | `Monotonic_coarse
  | `Monotonic_raw
  | `Boot_time
  | `Process_cpu_time
  | `Thread_cpu_time ]

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

type clock_id = int

external clock_linux_get_time : clock_id -> int64 = "clock_linux_get_time"

let get = function
  | `Realtime -> clock_linux_get_time realtime
  | `Monotonic -> clock_linux_get_time monotonic
  | `Realtime_coarse -> clock_linux_get_time realtime_coarse
  | `Monotonic_coarse -> clock_linux_get_time monotonic_coarse
  | `Monotonic_raw -> clock_linux_get_time monotonic_raw
  | `Boot_time -> clock_linux_get_time boot_time
  | `Process_cpu_time -> (
    match process_cpu_time with
    | Some i -> clock_linux_get_time i
    | None -> Fmt.invalid_arg "`Process_cpu_time unavailable" )
  | `Thread_cpu_time -> (
    match thread_cpu_time with
    | Some i -> clock_linux_get_time i
    | None -> Fmt.invalid_arg "`Thread_cpu_time unavailable" )

let kind_to_string = function
  | `Realtime -> "realtime"
  | `Monotonic -> "monotonic"
  | `Realtime_coarse -> "realtime-coarse"
  | `Monotonic_coarse -> "monotonic-coarse"
  | `Monotonic_raw -> "monotonic-raw"
  | `Boot_time -> "boot-time"
  | `Process_cpu_time -> "process-cpu-time"
  | `Thread_cpu_time -> "thread-cpu-time"

let pp_kind ppf kind = Fmt.string ppf (kind_to_string kind)

let kind_to_int = function
  | `Realtime -> realtime
  | `Monotonic -> monotonic
  | `Realtime_coarse -> realtime_coarse
  | `Monotonic_coarse -> monotonic_coarse
  | `Monotonic_raw -> monotonic_raw
  | `Boot_time -> boot_time
  | `Process_cpu_time -> (match process_cpu_time with
      | Some x -> x
      | None -> Fmt.invalid_arg "Clock_linux.kind_to_int")
  | `Thread_cpu_time -> (match thread_cpu_time with
      | Some x -> x
      | None -> Fmt.invalid_arg "Clock_linux.kind_to_int")

let string_to_kind = function
  | "realtime" -> `Realtime
  | "monotonic" -> `Monotonic
  | "realtime-coarse" -> `Realtime_coarse
  | "monotonic-coarse" -> `Monotonic_coarse
  | "monotonic-raw" -> `Monotonic_raw
  | "boot-time" -> `Boot_time
  | "process-cpu-time" -> `Process_cpu_time
  | "thread-cpu-time" -> `Thread_cpu_time
  | x -> Fmt.invalid_arg "Clock_linux.string_to_kind: %s" x

let int_to_kind x =
  if x = realtime then `Realtime
  else if x = monotonic then `Monotonic
  else if x = realtime_coarse then `Realtime_coarse
  else if x = monotonic_coarse then `Monotonic_coarse
  else if x = monotonic_raw then `Monotonic_raw
  else if x = boot_time then `Boot_time
  else if Some x = process_cpu_time then `Process_cpu_time
  else if Some x = thread_cpu_time then `Thread_cpu_time
  else Fmt.invalid_arg "Clock_linux.int_to_kind: %d" x

let max = 8

let compare_kind a b = kind_to_int a - kind_to_int b

module Set = Set.Make (struct
  type t = kind

  let compare = compare_kind
end)

module Map = Map.Make (struct
  type t = kind

  let compare = compare_kind
end)
