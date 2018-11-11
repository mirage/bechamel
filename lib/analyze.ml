module OLS = struct
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
end

module RANSAC = struct
  (* returns [a, b] such that [f(x) = a*x + b] minimize the distance between
     [sum(fun (x -> (f(x) - v(x))^2)] *)
  let affine_adjustment (r : (float * float) array) =
    let len = float (Array.length r) in
    let mean_x =
      let sum_x = Array.fold_right (fun (x, _) acc -> x +. acc) r 0. in
      sum_x /. len
    in
    let mean_y =
      let sum_y = Array.fold_right (fun (_, y) acc -> y +. acc) r 0. in
      sum_y /. len
    in
    let variance_x =
      let sumvar =
        Array.fold_right
          (fun (x, _) acc ->
            let v = x -. mean_x in
            (v *. v) +. acc )
          r 0.
      in
      sumvar /. len
    in
    let covariance_x_y =
      let sumcovar =
        Array.fold_right
          (fun (x, y) acc ->
            let v = (x -. mean_x) *. (y -. mean_y) in
            v +. acc )
          r 0.
      in
      sumcovar /. len
    in
    let a = covariance_x_y /. variance_x in
    let b = mean_y -. (a *. mean_x) in
    (a, b)

  let quality data (a, b) =
    let acc = ref 0. in
    for i = 0 to Array.length data - 1 do
      let x, y = data.(i) in
      let diff =
        let d = (a *. x) +. b -. y in
        d *. d
      in
      acc := !acc +. diff
    done ;
    !acc /. float (Array.length data)

  let ransac_filter_distance (x, y) (a, b) =
    let level = max x (max y (max a b)) in
    abs_float ((a *. x) +. b -. y) /. level

  let ransac_param data =
    { Ransac.model= affine_adjustment
    ; data
    ; subset_size= 10
    ; rounds= 100
    ; distance= ransac_filter_distance
    ; filter_distance= 0.05
    ; minimum_valid= Array.length data / 3
    ; error= quality }

  let sum a = Array.fold_left ( +. ) 0. a

  let standard_error ~a ~b (r : (float * float) array) =
    let estimate x = (a *. x) +. b in
    let dy (x, y) =
      let d = y -. estimate x in
      d *. d
    in
    let sum_dy = sum (Array.map dy r) in
    let mean_x =
      sum (Array.map (fun (x, _) -> x) r) /. float (Array.length r)
    in
    let dx (x, _) =
      let d = x -. mean_x in
      d *. d
    in
    sqrt (sum_dy /. float (Array.length r - 2)) /. sqrt (sum (Array.map dx r))

  type t =
    { label: Label.t
    ; mean_value: float
    ; constant: float
    ; max_value: float * float
    ; min_value: float * float
    ; standard_error: float }

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>%a per run = = %f;@] }" Label.pp t.label t.mean_value

  let result_column c m =
    (Measurement_raw.run m, Measurement_raw.get ~label:c m)

  let ransac ?(filter_outliers = true) c ml =
    let a = Array.map (result_column c) ml in
    let mean_value, constant =
      if filter_outliers then
        match Ransac.ransac (ransac_param a) with
        | None ->
            (* Couldn't extract a model, just return crude affine adjustment *)
            affine_adjustment a
        | Some {Ransac.model; _} -> model
      else affine_adjustment a
    in
    let min_value =
      Array.fold_left
        (fun (row_min, val_min) (row, value) ->
          let value = (value -. constant) /. row in
          if val_min < value || value <= 0. then (row_min, val_min)
          else (row, value) )
        (0., max_float) a
    in
    let correct_float f = classify_float f = FP_normal in
    let max_value =
      Array.fold_left
        (fun (row_max, val_max) (row, value) ->
          let value = (value -. constant) /. row in
          if val_max > value || not (correct_float value) then
            (row_max, val_max)
          else (row, value) )
        (0., min_float) a
    in
    let standard_error = standard_error ~a:mean_value ~b:constant a in
    {label= c; mean_value; constant; min_value; max_value; standard_error}
end

type 'a t =
  | OLS : {predictors: Label.t array} -> OLS.t t
  | RANSAC : {filter_outliers: bool} -> RANSAC.t t

let ols ~predictors = OLS {predictors}
let ransac ~filter_outliers = RANSAC {filter_outliers}

let analyze : type a. a t -> Label.t -> Measurement_raw.t array -> a =
 fun kind label m ->
  match kind with
  | OLS {predictors} -> OLS.ols ~predictors ~responder:label m
  | RANSAC {filter_outliers} -> RANSAC.ransac ~filter_outliers label m
