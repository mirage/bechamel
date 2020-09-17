open Bechamel

type t = Benchmark.stats

let sampling_witness : Benchmark.sampling Json_encoding.encoding =
  let open Json_encoding in
  let a =
    case float
      (function `Geometric x -> Some x | _ -> None)
      (fun x -> `Geometric x) in
  let b =
    case int (function `Linear x -> Some x | _ -> None) (fun x -> `Linear x)
  in
  union [ a; b ]

let mtime_witness : Mtime.span Json_encoding.encoding =
  let open Json_encoding in
  conv Mtime.Span.to_uint64_ns Mtime.Span.of_uint64_ns int53

(* XXX(dinosaure): fix [int53]. *)

let label_witness : string Json_encoding.encoding = Json_encoding.string

let witness : t Json_encoding.encoding =
  let open Json_encoding in
  let start = req "start" int in
  let sampling = req "sampling" sampling_witness in
  let stabilize = req "stabilize" bool in
  let quota = req "quota" mtime_witness in
  let limit = req "limit" int in
  let instances = req "instances" (list label_witness) in
  let samples = req "samples" int in
  let time = req "time" mtime_witness in
  conv
    (fun (t : t) ->
      let open Benchmark in
      ( t.start,
        t.sampling,
        t.stabilize,
        t.quota,
        t.limit,
        t.instances,
        t.samples,
        t.time ))
    (fun (start, sampling, stabilize, quota, limit, instances, samples, time) ->
      let open Benchmark in
      { start; sampling; stabilize; quota; limit; instances; samples; time })
    (obj8 start sampling stabilize quota limit instances samples time)
