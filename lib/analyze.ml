(* Linear regression inputs *)
let make_lr_inputs ~responder ~predictors m =
  let responder_accessor = Measurement_raw.get ~label:responder in
  let predictors_accessor =
    Array.map (fun label -> Measurement_raw.get ~label) predictors
  in
  ( Array.init (Array.length m) (fun i ->
        Array.map (fun a -> a m.(i)) predictors_accessor )
  , Array.init (Array.length m) (fun i -> responder_accessor m.(i)) )

type t = {labels: Label.t array; responder: Label.t; estimates: float array}

(* Ordinary Least Square *)
let ols ~responder ~predictors m =
  let matrix, vector = make_lr_inputs ~responder ~predictors m in
  let estimates = Linear_algebra.ols ~in_place:true matrix vector in
  {labels= predictors; responder; estimates}

module Map = Map.Make (String)

let pp ?(colors = Map.empty) ppf x =
  Fmt.pf ppf "{ @[" ;
  for i = 0 to Array.length x.labels - 1 do
    let style =
      match Map.find_opt (x.labels.(i) :> string) colors with
      | Some x -> x
      | None -> `None
    in
    Fmt.pf ppf "%s per %s = %a;@ "
      (x.responder :> string)
      (x.labels.(i) :> string)
      Fmt.(styled style float)
      x.estimates.(i)
  done ;
  Fmt.pf ppf "#end@] }"
