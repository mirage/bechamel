let () = Printexc.record_backtrace true

open Bechamel
open Toolkit

let make_list words =
  Staged.stage (fun () ->
      let rec go n acc = if n = 0 then acc else go (n - 1) (n :: acc) in
      ignore (go ((words / 3) + 1) []) )

let test = Test.make_indexed ~name:"make a list of" ~fmt:"%s %d" ~args:[0; 10; 100; 400; 1000] make_list

let benchmark () =
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[|run|] in
  let instances = Instance.[ minor_allocated
                           ; major_allocated
                           ; monotonic_clock ] in
  let raw_results = Benchmark.all ~run:3000 ~quota:Benchmark.(s 1.) instances test in
  List.map (fun instance -> Analyze.all ols instance raw_results) instances
  |> Analyze.merge ols instances

let () = Bechamel_notty.Unit.add Instance.monotonic_clock "ns"
let () = Bechamel_notty.Unit.add Instance.minor_allocated "w"
let () = Bechamel_notty.Unit.add Instance.major_allocated "mw"

type rect = Bechamel_notty.rect = { w: int; h: int }
let rect w h = { w; h; }

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match Notty_unix.winsize Unix.stdout with
    | Some (_, _) -> { w= 80; h= 1 }
    | None -> { w= 80; h= 1 } in
  img (window, benchmark ()) |> eol |> output_image
