type ('a, 't) app

module Uniq : sig
  type t

  external inj : 'a -> ('a, t) app = "%identity"
  external prj : ('a, t) app -> 'a = "%identity"
  val unit : (unit, t) app
end

module Multiple : sig
  type t

  external inj : 'a array -> ('a, t) app = "%identity"
  external prj : ('a, t) app -> 'a array = "%identity"
end

type packed = private
  | V :
      { fn : [ `Init ] -> 'a -> 'b
      ; kind : ('a, 'v, 't) kind
      ; allocate : 'v -> ('a, 't) app
      ; free : ('a, 't) app -> unit
      }
      -> packed

and ('a, 'v, 'k) kind = private
  | Uniq : ('a, unit, Uniq.t) kind
  | Multiple : ('a, int, Multiple.t) kind

val uniq : ('a, unit, Uniq.t) kind
val multiple : ('a, int, Multiple.t) kind

module Elt : sig
  type t

  val unsafe_make : name:string -> (unit -> 'a) Staged.t -> t
  val key : t -> int
  val name : t -> string
  val fn : t -> packed
end

type t

type fmt_indexed =
  (string -> int -> string, Format.formatter, unit, string) format4

type fmt_grouped =
  (string -> string -> string, Format.formatter, unit, string) format4

val make : name:string -> (unit -> 'a) Staged.t -> t
(** [make ~name fn] is a naming benchmark measuring [fn]. [fn] can be
    constructed with {!Staged.stage}:

    {[
      let write =
        Test.make ~name:"unix-write"
          (Staged.stage @@ fun () -> Unix.write Unix.stdout "Hello World!")
    ]} *)

val make_with_resource :
     name:string
  -> ('a, 'f, 'g) kind
  -> allocate:(unit -> 'a)
  -> free:('a -> unit)
  -> ('a -> 'b) Staged.t
  -> t
(** [make_with_resource ~name k ~allocate ~free fn] is a naming benchmark
    measuring [fn] with a pre-allocated resource ['v] (given by [allocate]).
    It permits to measure [fn] which requires a resource allocated {b before}
    the benchmark. Then, this resource can be free-ed {i via} the given [free]
    function.

    Depending on the [k] given, the resource can be allocated only one time or
    per [run]:
    - {!val:uniq}, the resource is allocated one ime before the benchmark and
      used by [fn] for all runs. Then, it is free-ed at the end of the
      benchmark.
    - {!val:multiple}, the resource is allocated multiple times (depending on
      how many times we want to {i run} [fn]) at the beginning of the benchmark
      and [fn] is executed with a structurally different resource per [run].
      Then, these resources are free-ed at the end of the benchmark.

    {b NOTE}: the {!val:multiple} case can ask a huge allocation (for instance,
    it can call 3000 times [allocate]). Be aware that such usage, depending on
    what you allocate, can lead an [Out_of_memory] exception. *)

val make_indexed :
     name:string
  -> ?fmt:fmt_indexed
  -> args:int list
  -> (int -> (unit -> 'a) Staged.t)
  -> t
(** [make_indexed ~name ~fmt ~args fn] is naming benchmarks indexed by an
    argument (by [args]). Name of each benchmark is [Fmt.strf fmt name arg]
    (default to ["%s:%d"]).

    {[
      let make_list words =
        Staged.stage @@ fun () ->
        let rec go n acc = if n = 0 then acc else go (n - 1) (n :: acc) in
        go ((words / 3) + 1) []

      let test =
        make_indexed ~name:"make_list" ~args:[ 0; 10; 100; 100 ] make_list
    ]}

    This kind of test is helpful to see results of the {b same} implementation
    with differents arguments (indexed by the given [int]). *)

val make_indexed_with_resource :
     name:string
  -> ?fmt:fmt_indexed
  -> args:int list
  -> ('a, 'f, 'g) kind
  -> allocate:(int -> 'a)
  -> free:('a -> unit)
  -> (int -> ('a -> 'b) Staged.t)
  -> t

val make_grouped : name:string -> ?fmt:fmt_grouped -> t list -> t
(** [make_grouped ~name ~fmt tests] is naming benchmarks. Name of each benchmark
    is [Fmt.strf fmt name arg] (default to [%s/%s]).

    {[
      let f0 = Test.make ~name:"fib1" ... ;; let f1 = Test.make ~name:"fib0"
      ... ;;

      let test = Test.make_grouped ~name:"fibonacci" [ f0; f1; ] ;;
    ]}

    This kind of test is helpful to compare results between many implementations. *)

val name : t -> string
(** [name t] returns the name of the test. *)

val names : t -> string list
(** [names t] returns names of sub-tests of [t] (such as {i indexed} tests or
    {i grouped} tests). *)

val elements : t -> Elt.t list
(** [elements t] returns all measuring functions of [t]. *)

val expand : t list -> Elt.t list
