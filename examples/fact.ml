open Bechamel
open Toolkit

let () = Random.self_init ()

let imp_fact x =
  let y = ref 0 in
  let r = ref 1 in
  while !y < x do
    y := !y + 1 ;
    r := !r * !y
  done ;
  !r

let rec fun_fact x = if x = 0 then 1 else x * fun_fact (x - 1)

let random_max = 32767.

let ( <.> ) f g x = f (g x)

let normal n =
  let m = n + (n mod 2) in
  let values = Array.create_float m in
  for i = 0 to (m / 2) - 1 do
    let x = ref 0. and y = ref 0. and rsq = ref 0. in
    while
      x := (Random.float random_max /. random_max *. 2.0) -. 1. ;
      y := (Random.float random_max /. random_max *. 2.0) -. 1. ;
      rsq := (!x *. !x) +. (!y *. !y) ;
      !rsq >= 1. || !rsq = 0.
    do
      ()
    done ;
    let f = sqrt (-2.0 *. log !rsq /. !rsq) in
    values.(i * 2) <- !x *. f ;
    values.((i * 2) + 1) <- !y *. f
  done ;
  Array.map (Float.to_int <.> ( *. ) random_max) values

let imp_fact n =
  let vs = normal n in
  Staged.stage @@ fun () -> Array.iter (ignore <.> imp_fact <.> abs) vs

let fun_fact n =
  let vs = normal n in
  Staged.stage @@ fun () -> Array.iter (ignore <.> fun_fact <.> abs) vs

let test0 =
  Test.make_indexed ~name:"functional" ~fmt:"%s %d" ~args:[ 10; 50; 100 ]
    fun_fact

let test1 =
  Test.make_indexed ~name:"imperative" ~fmt:"%s %d" ~args:[ 10; 50; 100 ]
    imp_fact

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ] in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) () in
  let raw_results =
    Benchmark.all cfg instances
      (Test.make_grouped ~name:"factorial" ~fmt:"%s %s" [ test0; test1 ]) in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let compare k0 k1 =
  let a = ref 0 and b = ref 0 in
  Scanf.sscanf k0 "%s %s %d" (fun _ _ a' -> a := a') ;
  Scanf.sscanf k1 "%s %s %d" (fun _ _ b' -> b := b') ;
  !a - !b

let nothing _ = Ok ()

let () =
  let results = benchmark () in
  let results =
    let open Bechamel_js in
    emit ~dst:(Channel stdout) nothing ~compare ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock)
      results in
  match results with Ok () -> () | Error (`Msg err) -> invalid_arg err
