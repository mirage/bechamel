let runnable f i =
  for _ = 1 to i do
    ignore (Sys.opaque_identity (f ()))
  done
  [@@inline]

let record measure =
  let (Measure.V (m, (module M))) = Measure.prj measure in
  fun () -> M.get m

let stabilize_garbage_collector () =
  let rec go fail last_heap_live_words =
    if fail <= 0
    then
      failwith "Unable to stabilize the number of live words in the major heap" ;
    Gc.compact () ;
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words
    then go (fail - 1) stat.Gc.live_words in
  go 10 0

let exceeded_allowed_time allowed_time_span t =
  let t' = Mtime.of_uint64_ns (Monotonic_clock.now ()) in
  Mtime.Span.compare (Mtime.span t t') allowed_time_span > 0

type sampling = [ `Linear of int | `Geometric of float ]

type stats = {
  start : int;
  sampling : sampling;
  stabilize : bool;
  quota : Mtime.span;
  limit : int;
  instances : string list;
  samples : int;
  time : Mtime.span;
}

type configuration = {
  start : int;
  sampling : sampling;
  stabilize : bool;
  quota : Mtime.span;
  limit : int;
}

let cfg ?(limit = 3000) ?(quota = Time.second 1.) ?(sampling = `Geometric 1.01)
    ?(stabilize = true) ?(start = 1) () : configuration =
  { limit; start; quota; sampling; stabilize }

let run cfg measures test =
  let idx = ref 0 in
  let run = ref cfg.start in
  let (Test.V init) = Test.Elt.fn test in
  let fn = init `Init in

  let measures = Array.of_list measures in
  let length = Array.length measures in
  let m = Array.create_float (cfg.limit * (length + 1)) in
  let m0 = Array.create_float length in
  let m1 = Array.create_float length in

  Array.iter Measure.load measures ;
  let records = Array.init length (fun i -> record measures.(i)) in

  stabilize_garbage_collector () ;

  let init_time = Mtime.of_uint64_ns (Monotonic_clock.now ()) in

  while (not (exceeded_allowed_time cfg.quota init_time)) && !idx < cfg.limit do
    let current_run = !run in
    let current_idx = !idx in

    for i = 0 to length - 1 do
      m0.(i) <- records.(i) ()
    done ;

    runnable fn current_run ;

    for i = 0 to length - 1 do
      m1.(i) <- records.(i) ()
    done ;

    m.(current_idx * (length + 1)) <- float_of_int current_run ;
    for i = 1 to length do
      m.((current_idx * (length + 1)) + i) <- m1.(i - 1) -. m0.(i - 1)
    done ;

    let next =
      match cfg.sampling with
      | `Linear k -> current_run + k
      | `Geometric scale ->
          let next_geometric =
            int_of_float (float_of_int current_run *. scale) in
          if next_geometric >= current_run + 1
          then next_geometric
          else current_run + 1 in

    run := next ;
    incr idx
  done ;

  let final_time = Mtime.of_uint64_ns (Monotonic_clock.now ()) in
  Array.iter Measure.unload measures ;

  let samples = !idx in
  let labels = Array.map Measure.label measures in
  let stats : stats =
    {
      start = cfg.start;
      sampling = cfg.sampling;
      stabilize = cfg.stabilize;
      quota = cfg.quota;
      limit = cfg.limit;
      instances = Array.to_list labels;
      samples;
      time = Mtime.span init_time final_time;
    } in

  let measurement_raw idx =
    let run = m.(idx * (length + 1)) in
    let measures = Array.sub m ((idx * (length + 1)) + 1) length in
    Measurement_raw.make ~measures ~labels run in

  (stats, Array.init samples measurement_raw)

let all cfg measures test =
  let tests = Array.of_list (Test.elements test) in
  let tbl = Hashtbl.create (Array.length tests) in

  for i = 0 to Array.length tests - 1 do
    let results = run cfg measures tests.(i) in
    Hashtbl.add tbl (Test.Elt.name tests.(i)) results
  done ;
  tbl
