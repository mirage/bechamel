type t = Mtime.Span.t

let ( <.> ) f g x = f (g x)

let to_span o x =
  let open Mtime in
  span
    ((of_uint64_ns <.> Int64.of_float) (Mtime.s_to_ns *. (x *. o)))
    (of_uint64_ns 0L)

let second n = to_span 1. n

let millisecond n = to_span Mtime.ms_to_s n

let microsecond n = to_span Mtime.us_to_s n

let nanosecond n = to_span Mtime.ns_to_s n
