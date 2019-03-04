let () = Printexc.record_backtrace true

open Bechamel
open Toolkit

let make_list words =
  Staged.stage (fun () ->
      let rec go n acc = if n = 0 then acc else go (n - 1) (n :: acc) in
      ignore (go ((words / 3) + 1) []) )

let test = Test.make_indexed ~name:"make a list of" ~fmt:"%s %d" ~args:[0; 10; 100; 400; 1000] make_list

let dictionary value =
  let open Json_encoding in
  assoc value

let dictionary_labelized t =
  let open Json_encoding in
  Json_encoding.custom
    (fun l -> `O (List.map (fun (n, v) -> Label.to_string n, construct t v) l))
    (fun v -> match v with
       | `O l ->
         let destruct n t v =
           try Json_encoding.destruct t v
           with Cannot_destruct (p, exn) -> raise (Cannot_destruct (`Field n :: p, exn)) in
         List.map (fun (n, v) ->
             let label = Json_encoding.destruct Label.Json.witness (`String n) in
             label, destruct (label :> string) t v) l
       | _ -> assert false)
    ~schema:Json_schema.any

let hashtbl_to_list hashtbl =
  Hashtbl.fold (fun k v a -> (k, v) :: a) hashtbl []
let hashtbl_map f hashtbl =
  let ret = Hashtbl.create (Hashtbl.length hashtbl) in
  Hashtbl.iter (fun k v -> Hashtbl.add ret k (f v)) hashtbl ; ret

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
