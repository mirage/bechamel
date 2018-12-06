let () = Printexc.record_backtrace true

open Toolkit
open Bechamel

let nothing words =
  Staged.stage (fun () ->
      let rec go n acc = if n = 0 then acc else go (n - 1) (n :: acc) in
      ignore (go ((words / 3) + 1) []) )

let test = Test.make_indexed ~name:"nothing" ~args:[0; 10; 100; 400] nothing
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()

let colors =
  List.fold_left
    (fun a (k, v) -> Label.Map.add k v a)
    Label.Map.empty
    [(Measure.label Instance.cpu_clock, `Red); (Measure.run, `Yellow)]

let analyze kind instances measures =
  List.map
    (fun label ->
      List.map (Analyze.analyze kind (Measure.label label)) measures )
    instances

let () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[|run|]
  in
  let ransac = Analyze.ransac ~filter_outliers:false ~predictor:Measure.run in
  let results =
    Benchmark.all
      ~quota:Benchmark.(s 1.)
      Instance.[minor_allocated; major_allocated; cpu_clock]
      test
    |> fun m ->
    let instances = Instance.[minor_allocated; major_allocated; cpu_clock] in
    let ols = analyze ols instances m in
    let ransac = analyze ransac instances m in
    (ols, ransac)
  in
  Fmt.pr "%a.\n%!"
    Fmt.(
      Dump.pair
        (Dump.list (Dump.list Analyze.OLS.(pp ~colors)))
        (Dump.list (Dump.list Analyze.RANSAC.(pp ~colors))))
    results
