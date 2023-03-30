(** This module provides several event counters from the [perf] API provided by
    Linux (2.6+) - and they are {b not} compatible with other systems. These
    counters are basically what the [perf] command provides.

    For some events, it is necessary {b to be root}. If you try to use these
    counters with insufficient privileges, [Bechamel_perf] will report you an
    permission error.

    If you want to use [Bechamel_perf] as a normal user, you should take a look
    on {{:https://www.kernel.org/doc/Documentation/sysctl/kernel.txt}the
    documentation about [perf_paranoid]}.
*)

open Bechamel

module Extension : sig
  val cycles : Mperf.t Measure.measure
  val instructions : Mperf.t Measure.measure
  val cache_references : Mperf.t Measure.measure
  val cache_misses : Mperf.t Measure.measure
  val branch_instructions : Mperf.t Measure.measure
  val branch_misses : Mperf.t Measure.measure
  val cpu_clock : Mperf.t Measure.measure
  val task_clock : Mperf.t Measure.measure
  val page_faults : Mperf.t Measure.measure
  val context_switches : Mperf.t Measure.measure
  val cpu_migrations : Mperf.t Measure.measure
  val page_faults_min : Mperf.t Measure.measure
  val page_faults_maj : Mperf.t Measure.measure
  val alignment_faults : Mperf.t Measure.measure
  val emulation_faults : Mperf.t Measure.measure
  val dummy : Mperf.t Measure.measure
end

module Instance : sig
  val cycles : Measure.witness
  val instructions : Measure.witness
  val cache_references : Measure.witness
  val cache_misses : Measure.witness
  val branch_instructions : Measure.witness
  val branch_misses : Measure.witness
  val cpu_clock : Measure.witness
  val task_clock : Measure.witness
  val page_faults : Measure.witness
  val context_switches : Measure.witness
  val cpu_migrations : Measure.witness
  val page_faults_min : Measure.witness
  val page_faults_maj : Measure.witness
  val alignment_faults : Measure.witness
  val emulation_faults : Measure.witness
  val dummy : Measure.witness
end
