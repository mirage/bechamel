let () = Printexc.record_backtrace true

open Bechamel

module Minor_allocated = struct
  type witness = unit
  type value = float ref
  type label = string

  let stat : Gc.stat ref = {contents= Gc.quick_stat ()}
  let load () = ()
  let unload () = ()
  let make () = ()
  let float x = !x
  let diff a b = {contents= !b -. !a}
  let epsilon () = {contents= 0.}
  let label () = "minor-allocated"

  let blit () v =
    stat := Gc.quick_stat () ;
    v := !stat.minor_words
end

module Major_allocated = struct
  type witness = unit
  type value = float ref
  type label = string

  let stat : Gc.stat ref = {contents= Gc.quick_stat ()}
  let load () = ()
  let unload () = ()
  let make () = ()
  let float x = !x
  let diff a b = {contents= !b -. !a}
  let epsilon () = {contents= 0.}
  let label () = "major-allocated"

  let blit () v =
    stat := Gc.quick_stat () ;
    v := !stat.major_words
end

module Monotonic_clock = struct
  type witness = int
  type value = int64 ref
  type label = string

  let load _witness = ()
  let unload _witness = ()
  let make () = Clock.monotonic
  let float x = Int64.to_float !x
  let label x = Clock.kind_to_string (Clock.int_to_kind x)
  let diff a b = {contents= Int64.sub !b !a}
  let epsilon () = {contents= 0L}
  let blit witness v = v := Clock.(clock_linux_get_time witness)
end

module Realtime_clock = struct
  type witness = int
  type value = int64 ref
  type label = string

  let load _witness = ()
  let unload _witness = ()
  let make () = Clock.realtime
  let float x = Int64.to_float !x
  let label x = Clock.kind_to_string (Clock.int_to_kind x)
  let diff a b = {contents= Int64.sub !b !a}
  let epsilon () = {contents= 0L}
  let blit witness v = v := Clock.(clock_linux_get_time witness)
end

let ext_minor_words = Measure.make (module Minor_allocated)
let ext_major_words = Measure.make (module Major_allocated)
let ext_monotonic_clock = Measure.make (module Monotonic_clock)
let ext_realtime_clock = Measure.make (module Realtime_clock)
let ins_minor_words = Measure.instance (module Minor_allocated) ext_minor_words
let ins_major_words = Measure.instance (module Major_allocated) ext_major_words

let ins_monotonic_clock =
  Measure.instance (module Monotonic_clock) ext_monotonic_clock

let ins_real_clock =
  Measure.instance (module Realtime_clock) ext_realtime_clock

let nothing words =
  Staged.stage (fun () ->
      let rec go n acc = if n = 0 then acc else go (n - 1) (n :: acc) in
      ignore (go ((words / 3) + 1) []) )

let test = Test.make_indexed ~name:"nothing" ~args:[0; 10; 100; 400] nothing
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()

let analyze responders measures =
  List.map
    (fun responder ->
      List.map
        (Analyze.ols ~responder:(Measure.label responder)
           ~predictors:Measure.(with_run []))
        measures )
    responders

let () =
  let results =
    Benchmark.all
      [ins_minor_words; ins_major_words; ins_monotonic_clock; ins_real_clock]
      test
    |> analyze
         [ins_minor_words; ins_major_words; ins_monotonic_clock; ins_real_clock]
  in
  Fmt.pr "%a.\n%!"
    Fmt.(Dump.list (Dump.list Analyze.(pp ~colors:Map.empty)))
    results
