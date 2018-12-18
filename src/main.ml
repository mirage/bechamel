let () = Printexc.record_backtrace true

open Bechamel
open Toolkit

let nothing words =
  Staged.stage (fun () ->
      let rec go n acc = if n = 0 then acc else go (n - 1) (n :: acc) in
      ignore (go ((words / 3) + 1) []) )

let test = Test.make_indexed ~name:"nothing" ~args:[0; 10; 100; 400] nothing

let dictionary value =
  let open Json_encoding in
  assoc value

let () =
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[|run|] in
  let ransac = Analyze.ransac ~filter_outliers:false ~predictor:Measure.run in
  let instances = Instance.[minor_allocated; major_allocated; monotonic_clock] in
  let ols, ransac =
    Benchmark.all ~run:3000
      ~quota:Benchmark.(s 1.)
      instances
      test
    |> fun results ->
    let ols = Analyze.all ols Instance.monotonic_clock results in
    let ransac = Analyze.all ransac Instance.monotonic_clock results in
    (ols, ransac) in
  let ols_results = open_out "ols-results.json" in
  let ols_json =
    Json_encoding.construct (dictionary Analyze.OLS.Json.witness)
      (Hashtbl.fold (fun name o acc -> (name, o) :: acc) ols []) in
  let ransac_results = open_out "ransac-results.json" in
  let ransac_json =
    Json_encoding.construct (dictionary Analyze.RANSAC.Json.witness)
      (Hashtbl.fold (fun name o acc -> (name, o) :: acc) ransac []) in
  Ezjsonm.to_channel ols_results (Ezjsonm.wrap ols_json) ;
  Ezjsonm.to_channel ransac_results (Ezjsonm.wrap ransac_json) ;
  ()

