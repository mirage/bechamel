let () = Printexc.record_backtrace true

open Bechamel
open Toolkit

let make_list words =
  Staged.stage (fun () ->
      let rec go n acc = if n = 0 then acc else go (n - 1) (n :: acc) in
      ignore (go ((words / 3) + 1) []) )

let test = Test.make_indexed ~name:"list" ~fmt:"%s %d" ~args:[0; 10; 100; 400; 1000] make_list

let benchmark () =
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[|run|] in
  let instances = Instance.[ minor_allocated
                           ; major_allocated
                           ; monotonic_clock ] in
  let raw_results = Benchmark.all ~run:3000 ~quota:Benchmark.(s 1.) instances test in
  let results = List.map (fun instance -> Analyze.all ols instance raw_results) instances in
  let results = Analyze.merge ols instances results in
  results, raw_results

let compare k0 k1 =
  let a = ref 0 and b = ref 0 in
  Scanf.sscanf k0 "%s %d" (fun _ a' -> a := a') ;
  Scanf.sscanf k1 "%s %d" (fun _ b' -> b := b') ;
  !a - !b
let nothing _ = Ok ()

let () =
  let results = benchmark () in
  match Bechamel_js.(emit ~dst:(Channel stdout) nothing ~compare ~x_label:Measure.run ~y_label:(Measure.label Instance.monotonic_clock) results) with
  | Ok () -> ()
  | Error (`Msg err) -> invalid_arg err
