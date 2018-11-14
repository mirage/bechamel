let unzip t =
  let a =
    Array.init (Array.length t) (fun i ->
        let x, _, _ = Array.unsafe_get t i in
        x )
  in
  let b =
    Array.init (Array.length t) (fun i ->
        let _, x, _ = Array.unsafe_get t i in
        x )
  in
  let c =
    Array.init (Array.length t) (fun i ->
        let _, _, x = Array.unsafe_get t i in
        x )
  in
  (a, b, c)

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
  let t' = Mtime_clock.now () in
  Mtime.(Span.compare (span t' t) allowed_time_span > 0)

let run ?(sampling = `Geometric 1.01) ?(stabilize = false)
    ?(quota = default_quota) iter measures test =
  let idx = ref 0 in
  let run = ref 0 in
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
        let (Measure.Extension.V (_, m)) = Measure.Extension.proj x in
        let (Measure.Switch.Measure (module M)) = m in
        let epsilon = M.epsilon () in
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
  let init_time = Mtime_clock.now () in
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
    for _ = 1 to current_run do
      ignore (fn ())
    done ;
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
  let total = !idx in
  Array.map
    (fun m ->
      let run = m.(0) in
      (* XXX(dinosaure): we put by hands [Run] extension at the begin of
         [measures]. *)
      Measurement_raw.make run ~measures:m ~labels )
    (Array.sub measurement_raw 0 total)

(* [run_loop n test] returns the elapsed time of running [n >= 0L] times
   [test]. *)
let run_loop ~sampling n test =
  let (Test.V fn) = Test.Elt.fn test in
  let fn = fn `Init in
  let t0 = Mtime_clock.now () in
  let run = ref 0 in
  for _ = 1 to n do
    let current_run = !run in
    for _ = 1 to !run do
      ignore (fn ())
    done ;
    let next =
      match sampling with
      | `Linear k -> current_run + k
      | `Geometric scale ->
          let next_geometric =
            int_of_float (float_of_int current_run *. scale)
          in
          (max : int -> int -> int) next_geometric (current_run + 1)
    in
    run := next
  done ;
  let t1 = Mtime_clock.now () in
  Mtime.span t0 t1

let _null_loop ~sampling n =
  run_loop ~sampling n
    (Test.Elt.unsafe_make ~name:"ignore" (Staged.stage ignore))

(* Run function [f] count times, return time taken (>= 0.). *)
let time_it ~sampling n fn = run_loop ~sampling n fn
let () = Random.self_init ()

let estimate ~sampling bmin bmax fn =
  let rec reduce ~bmin ~bmax n_min n_max =
    let interval = n_max - n_min in
    if interval <= 1 then n_max
    else
      let new_iter = Random.int (n_max - n_min) + n_min in
      let time = time_it ~sampling new_iter fn in
      if Mtime.Span.compare time bmax < 0 then
        reduce ~bmin:time ~bmax new_iter n_max
      else if Mtime.Span.compare time bmax > 0 then
        reduce ~bmin ~bmax n_min new_iter
      else new_iter
  in
  let rec loop ~previous_iter iter =
    let time = time_it ~sampling iter fn in
    if Mtime.Span.compare time bmax < 0 then
      loop ~previous_iter:iter (iter lsl 1)
    else if Mtime.Span.compare time bmax > 0 then
      reduce ~bmin:time ~bmax previous_iter iter
    else if Mtime.Span.compare time bmin < 0 then assert false
    else iter
  in
  loop ~previous_iter:1 1

let all ?(sampling = `Geometric 1.01) ?stabilize ?run:iter ?quota measures test
    =
  let tests = Test.set test in
  List.map
    (fun test ->
      let quota, iter =
        match (quota, iter) with
        | Some x, None -> (x, estimate ~sampling Mtime.Span.zero x test)
        | None, Some x -> (time_it ~sampling x test, x)
        | Some quota, Some iter -> (quota, iter)
        | None, None ->
            let iter = estimate ~sampling Mtime.Span.zero default_quota test in
            (time_it ~sampling iter test, iter)
      in
      run ~sampling ?stabilize ~quota iter measures test )
    tests
