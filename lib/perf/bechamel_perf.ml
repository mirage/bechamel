module Perf = Mperf

module Make (X : sig
  val kind : Perf.Attr.Kind.t
end) =
struct
  type witness = Perf.t

  let load witness = Perf.enable witness
  let unload witness = Perf.disable witness
  let make () = Perf.make (Perf.Attr.make X.kind)

  let label witness =
    let kind = Perf.kind witness in
    Perf.Attr.Kind.to_string kind

  let unit = label
  let get witness = Int64.to_float (Perf.read witness)
end

module Cycles = Make (struct
  let kind = Perf.Attr.Kind.Cycles
end)

module Instructions = Make (struct
  let kind = Perf.Attr.Kind.Instructions
end)

module Cache_references = Make (struct
  let kind = Perf.Attr.Kind.Cache_references
end)

module Cache_misses = Make (struct
  let kind = Perf.Attr.Kind.Cache_misses
end)

module Branch_instructions = Make (struct
  let kind = Perf.Attr.Kind.Branch_instructions
end)

module Branch_misses = Make (struct
  let kind = Perf.Attr.Kind.Branch_misses
end)

module Bus_cycles = Make (struct
  let kind = Perf.Attr.Kind.Bus_cycles
end)

module Stalled_cycles_frontend = Make (struct
  let kind = Perf.Attr.Kind.Stalled_cycles_frontend
end)

module Stalled_cycles_backend = Make (struct
  let kind = Perf.Attr.Kind.Stalled_cycles_backend
end)

module Ref_cpu_cycles = Make (struct
  let kind = Perf.Attr.Kind.Ref_cpu_cycles
end)

module Cpu_clock = Make (struct
  let kind = Perf.Attr.Kind.Cpu_clock
end)

module Task_clock = Make (struct
  let kind = Perf.Attr.Kind.Task_clock
end)

module Page_faults = Make (struct
  let kind = Perf.Attr.Kind.Page_faults
end)

module Context_switches = Make (struct
  let kind = Perf.Attr.Kind.Context_switches
end)

module Cpu_migrations = Make (struct
  let kind = Perf.Attr.Kind.Cpu_migrations
end)

module Page_faults_min = Make (struct
  let kind = Perf.Attr.Kind.Page_faults_min
end)

module Page_faults_maj = Make (struct
  let kind = Perf.Attr.Kind.Page_faults_maj
end)

module Alignment_faults = Make (struct
  let kind = Perf.Attr.Kind.Alignment_faults
end)

module Emulation_faults = Make (struct
  let kind = Perf.Attr.Kind.Emulation_faults
end)

module Dummy = Make (struct
  let kind = Perf.Attr.Kind.Dummy
end)

open Bechamel

module Extension = struct
  include Toolkit.Extension

  (* XXX(dinosaure): only software measures. *)

  let cpu_clock = Measure.register (module Cpu_clock)
  let task_clock = Measure.register (module Task_clock)
  let page_faults = Measure.register (module Page_faults)
  let context_switches = Measure.register (module Context_switches)
  let cpu_migrations = Measure.register (module Cpu_migrations)
  let page_faults_min = Measure.register (module Page_faults_min)
  let page_faults_maj = Measure.register (module Page_faults_maj)
  let alignment_faults = Measure.register (module Alignment_faults)
  let emulation_faults = Measure.register (module Emulation_faults)
  let dummy = Measure.register (module Dummy)
end

module Instance = struct
  include Toolkit.Instance

  let cpu_clock = Measure.instance (module Cpu_clock) Extension.cpu_clock
  let task_clock = Measure.instance (module Task_clock) Extension.task_clock
  let page_faults = Measure.instance (module Page_faults) Extension.page_faults

  let context_switches =
    Measure.instance (module Context_switches) Extension.context_switches

  let cpu_migrations =
    Measure.instance (module Cpu_migrations) Extension.cpu_migrations

  let page_faults_min =
    Measure.instance (module Page_faults_min) Extension.page_faults_min

  let page_faults_maj =
    Measure.instance (module Page_faults_maj) Extension.page_faults_maj

  let alignment_faults =
    Measure.instance (module Alignment_faults) Extension.alignment_faults

  let emulation_faults =
    Measure.instance (module Emulation_faults) Extension.emulation_faults

  let dummy = Measure.instance (module Dummy) Extension.dummy
end
