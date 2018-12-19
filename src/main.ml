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
  let instances = Instance.[minor_allocated; major_allocated; monotonic_clock] in
  let results =
    Benchmark.all ~run:3000 ~quota:Benchmark.(s 1.) instances test
    |> Analyze.all ols Instance.monotonic_clock in
  let output = open_out "results.json" in
  let json =
    Json_encoding.construct (dictionary Analyze.OLS.Json.witness)
      (Hashtbl.fold (fun name o acc -> (name, o) :: acc) results []) in
  Ezjsonm.to_channel output (Ezjsonm.wrap json) ;
  ()

