module Make (X : sig
  val kind : Perf.Attr.Kind.t
end) =
struct
  type witness = Perf.t
  type value = int64 ref
  type label = string

  let load witness = Perf.enable witness
  let unload witness = Perf.disable witness
  let make () = Perf.make (Perf.Attr.make X.kind)
  let float x = Int64.to_float !x

  let label witness =
    let kind = Perf.kind witness in
    Perf.Attr.Kind.to_string kind

  let diff a b = {contents= Int64.sub !b !a}
  let epsilon () = {contents= 0L}
  let blit witness v = v := Perf.read witness
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

  let cycles = Measure.make (module Cycles)
  let instructions = Measure.make (module Instructions)
  let cache_references = Measure.make (module Cache_references)
  let cache_misses = Measure.make (module Cache_misses)
  let branch_instructions = Measure.make (module Branch_instructions)
  let branch_misses = Measure.make (module Branch_misses)
  let bus_cycles = Measure.make (module Bus_cycles)
  let stalled_cycles_frontend = Measure.make (module Stalled_cycles_frontend)
  let stalled_cycles_backend = Measure.make (module Stalled_cycles_backend)
  let ref_cpu_cycles = Measure.make (module Ref_cpu_cycles)
  let cpu_clock = Measure.make (module Cpu_clock)
  let task_clock = Measure.make (module Task_clock)
  let page_faults = Measure.make (module Page_faults)
  let context_switches = Measure.make (module Context_switches)
  let cpu_migrations = Measure.make (module Cpu_migrations)
  let page_faults_min = Measure.make (module Page_faults_min)
  let page_faults_maj = Measure.make (module Page_faults_maj)
  let alignment_faults = Measure.make (module Alignment_faults)
  let emulation_faults = Measure.make (module Emulation_faults)
  let dummy = Measure.make (module Dummy)
end

module Instance = struct
  include Toolkit.Instance

  let cycles = Measure.instance (module Cycles) Extension.cycles

  let instructions =
    Measure.instance (module Instructions) Extension.instructions

  let cache_references =
    Measure.instance (module Cache_references) Extension.cache_references

  let cache_misses =
    Measure.instance (module Cache_misses) Extension.cache_misses

  let branch_instructions =
    Measure.instance (module Branch_instructions) Extension.branch_instructions

  let branch_misses =
    Measure.instance (module Branch_misses) Extension.branch_misses

  let bus_cycles = Measure.instance (module Bus_cycles) Extension.bus_cycles

  let stalled_cycles_frontend =
    Measure.instance
      (module Stalled_cycles_frontend)
      Extension.stalled_cycles_frontend

  let stalled_cycles_backend =
    Measure.instance
      (module Stalled_cycles_backend)
      Extension.stalled_cycles_backend

  let ref_cpu_cycles =
    Measure.instance (module Ref_cpu_cycles) Extension.ref_cpu_cycles

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
