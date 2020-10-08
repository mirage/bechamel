type t

type span

val of_uint64_ns : int64 -> t

val to_uint64_ns : t -> int64

val span : t -> t -> span

val second : float -> span

val millisecond : float -> span

val microsecond : float -> span

val nanosecond : float -> span

val span_of_uint64_ns : int64 -> span

val span_to_uint64_ns : span -> int64

val span_compare : span -> span -> int
