let unzip t =
  let a =
    Array.init (Array.length t) (fun i ->
        let x, _, _ = Array.get t i in
        x ) in
  let b =
    Array.init (Array.length t) (fun i ->
        let _, x, _ = Array.get t i in
        x ) in
  let c =
    Array.init (Array.length t) (fun i ->
        let _, _, x = Array.get t i in
        x ) in
  (a, b, c)

let src = Logs.Src.create "benchmark" ~doc:"logs benchmark's events"
module Log = (val Logs.src_log src : Logs.LOG)

let stabilize_garbage_collector () =
  let rec go fail last_heap_live_words =
    if fail <= 0 then
      failwith "Unable to stabilize the number of live words in the major heap" ;
    Gc.compact () ;
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words then
      go (fail - 1) stat.Gc.live_words
  in
  go 10 0

let unit = ()
let apply f = f unit
let ( <.> ) f g x = f (g x)

let to_span o x =
  let open Mtime in
  span
    ((of_uint64_ns <.> Int64.of_float) (Mtime.s_to_ns *. (x *. o)))
    (of_uint64_ns 0L)

let ms = to_span Mtime.ms_to_s
let us = to_span Mtime.us_to_s
let ns = to_span Mtime.ns_to_s
let s = to_span 1.
let default_quota = us 250.

let exceeded_allowed_time allowed_time_span t =
  let t' = Mtime.of_uint64_ns (Clock.get ()) in
  Mtime.Span.compare (Mtime.span t' t) allowed_time_span > 0

let runnable f i =
  for _ = 1 to i do ignore @@ Sys.opaque_identity (f ()) done [@@inline]

type stats =
  { start : int
  ; sampling : sampling
  ; stabilize : bool
  ; quota : Mtime.span
  ; run : int
  ; instances : Label.t list
  ; samples : int
  ; time : Mtime.span }
and sampling = [ `Linear of int | `Geometric of float ]

let run ?(start = 0) ?(sampling = `Geometric 1.01) ?(stabilize = false)
    ?(quota = default_quota) iter measures test =
  let stats =
    { start; sampling; stabilize; quota; run= iter; instances= List.map Measure.label measures
    ; samples= 0; time= Mtime.Span.zero } in
  let idx = ref 0 in
  let run = ref start in
  let module Run = struct
    type witness = unit
    type value = int ref
    type label = Label.t

    let load () = ()
    let unload () = ()
    let make () = ()
    let float x = float_of_int !x

    (* XXX(dinosaure): we short-cut verification about labels. *)
    let label () = Label.of_string "run"

    let diff a b =
      (* XXX(dinosaure): [run] can not differ before and after [fn]. *)
      if !a <> !b then invalid_arg "Invalid constant value" ;
      a

    let epsilon () = run
    let blit () v = v := !run
    let compare a b = !a - !b
  end in
  let module RunMaker = Measure.Switch.Measure (Run) in
  let measure_run = RunMaker.x in
  let extension_run = Measure.Extension.inj measure_run in
  let instance_run =
    let module Ext = (val extension_run) in
    Ext.T (Run.make (), Run.epsilon ())
  in
  let measures =
    Array.of_list (instance_run :: Toolkit.Instance.one :: measures)
  in
  (* extract fn *)
  let (Test.V fn) = Test.Elt.fn test in
  let fn = fn `Init in
  (* allocate global store *)
  let allocate_measurement_raw _ : float array =
    Array.map
      (fun x ->
        let (Measure.Extension.V (i, m)) = Measure.Extension.proj x in
        let (Measure.Switch.Measure (module M)) = m in
        let epsilon = snd i in
        M.float epsilon )
      measures
  in
  let measurement_raw = Array.init iter allocate_measurement_raw in
  (* XXX(dinosaure): [Polytable] is generative. *)
  let module Polytable = Polytable.Make (Measure.Value) in
  let store_0 = Polytable.create ~len:(Array.length measures) in
  let store_1 = Polytable.create ~len:(Array.length measures) in
  let read_0, read_1, keys =
    Array.map
      (fun x ->
        let (Measure.Extension.V
              (instance, (Measure.Switch.Measure (module M) as m))) =
          Measure.Extension.proj x
        in
        let witness = fst instance in
        let value = Measure.Switch.to_value m in
        let key = Polytable.Key.create value in
        let (Measure.Switch.Value (module V)) = value in
        Polytable.add store_0 key (V.epsilon ()) ;
        (* allocate values *)
        Polytable.add store_1 key (V.epsilon ()) ;
        (* allocate values *)
        let blitter = Measure.Switch.blit m witness in
        ( (fun () -> Polytable.set store_0 key blitter)
        , (fun () -> Polytable.set store_1 key blitter)
        , Polytable.Key.pack key ) )
      measures
    |> unzip
  in
  let init_time = Mtime.of_uint64_ns (Clock.get ()) in
  while
    (not (exceeded_allowed_time quota init_time))
    && !idx < Array.length measurement_raw
  do
    let current_run = !run in
    let current_idx = !idx in
    if stabilize || current_run = 0 then stabilize_garbage_collector () ;
    Array.iter
      (fun x ->
        let (Measure.Extension.V (instance, measure)) =
          Measure.Extension.proj x
        in
        let (Measure.Switch.Measure (module Measure)) = measure in
        let witness = fst instance in
        Measure.load witness )
      measures ;
    Array.iter apply read_0 ;

    runnable fn current_run ;

    Array.iter apply read_1 ;
    Array.iter
      (fun x ->
        let (Measure.Extension.V (instance, measure)) =
          Measure.Extension.proj x
        in
        let (Measure.Switch.Measure (module Measure)) = measure in
        let witness = fst instance in
        Measure.unload witness )
      measures ;
    (* save local store *)
    Array.iteri
      (fun x (Polytable.Key.V key) ->
        match (Polytable.find store_0 key, Polytable.find store_1 key) with
        | Some a, Some b ->
            let (Measure.Switch.Value (module V)) = Polytable.Key.info key in
            let res = V.(float (diff a b)) in
            measurement_raw.(current_idx).(x) <- res
        | _, _ -> assert false )
      keys ;
    let next =
      match sampling with
      | `Linear k -> current_run + k
      | `Geometric scale ->
          let next_geometric =
            int_of_float (float_of_int current_run *. scale)
          in
          (max : int -> int -> int) next_geometric (current_run + 1)
    in
    run := next ;
    incr idx
  done ;
  let final_time = Mtime.of_uint64_ns (Clock.get ()) in
  let total = !idx in
  let stats = { stats with samples= !idx; time= Mtime.span init_time final_time } in
  let labels =
    Array.map
      (fun x ->
        let (Measure.Extension.V (instance, measure)) =
          Measure.Extension.proj x
        in
        let (Measure.Switch.Measure (module Measure)) = measure in
        let witness = fst instance in
        Measure.label witness )
      measures
  in
  stats, Array.map
    (fun m ->
      let run = m.(0) in
      (* XXX(dinosaure): we put by hands [Run] extension at the begin of
         [measures]. *)
      Measurement_raw.make run ~measures:m ~labels )
    (Array.sub measurement_raw 0 total)

let all ?(start = 0) ?(sampling = `Geometric 1.01) ?stabilize ?run:(iter= 3000) ?(quota= default_quota)
    measures test =
  Logs.debug (fun f -> f "Start to benchmark %s." (Test.name test)) ;
  let tests = Array.of_list (Test.set test) in
  let tbl = Hashtbl.create (Array.length tests) in
  for i = 0 to Array.length tests - 1
  do
    Logs.debug (fun f ->
        f "Start to run %s (run: %d, quota: %a)." (Test.Elt.name tests.(i)) iter
          Mtime.Span.pp quota ) ;
    let results = run ~start ~sampling ?stabilize ~quota iter measures tests.(i) in
    Hashtbl.add tbl (Test.Elt.name tests.(i)) results
  done ; tbl
