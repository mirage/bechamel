open Stdlib

type t = int64
type span = int64

let of_uint64_ns x = x
let to_uint64_ns x = x

let span t0 t1 =
  if Int64.unsigned_compare t0 t1 < 0 then Int64.sub t1 t0 else Int64.sub t0 t1

let s_to_ns = 1e9
let to_span o x = span (Int64.of_float (s_to_ns *. (x *. o))) 0L
let second n = to_span 1. n
let millisecond n = to_span 1e-3 n
let microsecond n = to_span 1e-6 n
let nanosecond n = to_span 1e-9 n
let span_compare = Int64.unsigned_compare
let span_of_uint64_ns x = x
let span_to_uint64_ns x = x
