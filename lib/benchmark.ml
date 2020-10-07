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
  let t' = Monotonic_clock.now () in
  let t' = Time.of_uint64_ns t' in
  Time.span_compare (Time.span t t') allowed_time_span > 0

type sampling = [ `Linear of int | `Geometric of float ]

type stats = {
  start : int;
  sampling : sampling;
  stabilize : bool;
  quota : Time.span;
  limit : int;
  instances : string list;
  samples : int;
  time : Time.span;
}

type configuration = {
  start : int;
  sampling : sampling;
  stabilize : bool;
  quota : Time.span;
  kde : int option;
  limit : int;
}

type t = {
  stats : stats;
  lr : Measurement_raw.t array;
  kde : Measurement_raw.t array option;
}

let cfg ?(limit = 3000) ?(quota = Time.second 1.) ?(kde = None)
    ?(sampling = `Geometric 1.01) ?(stabilize = true) ?(start = 1) () :
    configuration =
  { limit; start; quota; sampling; kde; stabilize }

let run cfg measures test : t =
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

  let init_time = Time.of_uint64_ns (Monotonic_clock.now ()) in

  let total_run = ref 0 in

  while (not (exceeded_allowed_time cfg.quota init_time)) && !idx < cfg.limit do
    let current_run = !run in
    let current_idx = !idx in

    (* The returned measurements are a difference betwen a measurement [m0]
       taken before running the tested function [fn] and a measurement taken
       after [m1]. *)
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

    total_run := !total_run + !run ;
    run := next ;
    incr idx
  done ;

  let samples = !idx in
  let labels = Array.map Measure.label measures in

  let measurement_raw idx =
    let run = m.(idx * (length + 1)) in
    let measures = Array.sub m ((idx * (length + 1)) + 1) length in
    Measurement_raw.make ~measures ~labels run in
  let lr_raw = Array.init samples measurement_raw in

  (* Additional measurement for kde, if requested. Note that if these
     measurements go through, the time limit is twice the one without it.*)
  let kde_raw =
    match cfg.kde with
    | None -> None
    | Some kde_limit ->
        let mkde = Array.create_float (kde_limit * length) in
        let init_time' = Time.of_uint64_ns (Monotonic_clock.now ()) in
        let current_idx = ref 0 in
        while
          (not (exceeded_allowed_time cfg.quota init_time'))
          && !current_idx < kde_limit
        do
          for i = 0 to length - 1 do
            m0.(i) <- records.(i) ()
          done ;

          ignore (Sys.opaque_identity (fn ())) ;

          for i = 0 to length - 1 do
            m1.(i) <- records.(i) ()
          done ;

          for i = 0 to length - 1 do
            mkde.((!current_idx * length) + i) <- m1.(i) -. m0.(i)
          done ;

          total_run := !total_run + !run ;
          incr current_idx
        done ;
        let kde_raw idx =
          let measures = Array.sub mkde (idx * length) length in
          Measurement_raw.make ~measures ~labels 1. in

        Some (Array.init !current_idx kde_raw) in

  let final_time = Time.of_uint64_ns (Monotonic_clock.now ()) in
  Array.iter Measure.unload measures ;

  let stats =
    {
      start = cfg.start;
      sampling = cfg.sampling;
      stabilize = cfg.stabilize;
      quota = cfg.quota;
      limit = cfg.limit;
      instances = Array.to_list labels;
      samples;
      time = Time.span init_time final_time;
    } in

  { stats; lr = lr_raw; kde = kde_raw }

let all cfg measures test =
  let tests = Array.of_list (Test.elements test) in
  let tbl = Hashtbl.create (Array.length tests) in

  for i = 0 to Array.length tests - 1 do
    let results = run cfg measures tests.(i) in
    Hashtbl.add tbl (Test.Elt.name tests.(i)) results
  done ;
  tbl
