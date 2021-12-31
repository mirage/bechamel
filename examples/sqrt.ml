open Bechamel

let () = Random.self_init ()
let random_max = 32767.
let ( <.> ) f g x = f (g x)

let normal n =
  let m = n + (n mod 2) in
  let values = Array.create_float m in
  for i = 0 to (m / 2) - 1 do
    let x = ref 0. and y = ref 0. and rsq = ref 0. in
    while
      x := (Random.float random_max /. random_max *. 2.0) -. 1.;
      y := (Random.float random_max /. random_max *. 2.0) -. 1.;
      rsq := (!x *. !x) +. (!y *. !y);
      !rsq >= 1. || !rsq = 0.
    do
      ()
    done;
    let f = sqrt (-2.0 *. log !rsq /. !rsq) in
    values.(i * 2) <- !x *. f;
    values.((i * 2) + 1) <- !y *. f
  done;
  Array.map (( *. ) random_max) values

let sqrt n =
  let vs = normal n in
  Staged.stage @@ fun () ->
  for i = 0 to Array.length vs - 1 do
    ignore (sqrt vs.(i))
  done

let test = Test.make_indexed ~name:"sqrt" ~fmt:"%s %d" ~args:[ 10; 50 ] sqrt

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances = Bechamel_perf.Instance.[ cpu_clock ] in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 0.5) ~kde:(Some 1000) ()
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let () =
  let results, _ = benchmark () in
  match
    Hashtbl.fold
      (fun _ v a -> Hashtbl.fold (fun k v a -> (k, v) :: a) v [] :: a)
      results []
  with
  | [ results ] ->
      let print (k, ols) = Fmt.pr "%s: %a\n%!" k Analyze.OLS.pp ols in
      List.iter print results
  | _ -> assert false
