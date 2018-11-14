[@@@warning "-32-34"]

module OLS = struct
  module Ci95 = struct
    (* 95% confidence interval *)
    type t = {r: float; l: float}

    let abs_err t ~estimate = (t.l -. estimate, t.r -. estimate)

    let rel_err t ~estimate =
      let low, high = abs_err t ~estimate in
      (low /. estimate, high /. estimate)

    let bad = {r= Float.neg_infinity; l= Float.neg_infinity}
  end

  module Coefficient = struct
    type t = {predictor: Label.t; estimate: float; mutable ci95: Ci95.t option}
  end

  (* Linear regression inputs *)
  let make_lr_inputs ~responder ~predictors m =
    let responder_accessor = Measurement_raw.get ~label:responder in
    let predictors_accessor =
      Array.map (fun label -> Measurement_raw.get ~label) predictors
    in
    ( Array.init (Array.length m) (fun i ->
          Array.map (fun a -> a m.(i)) predictors_accessor )
    , Array.init (Array.length m) (fun i -> responder_accessor m.(i)) )

  type v =
    { labels: Label.t array
    ; responder: Label.t
    ; estimates: float array
    ; r_square: float option }

  and t = (v, Rresult.R.msg) result

  let r_square m ~responder ~predictors r =
    let predictors_matrix, responder_vector =
      make_lr_inputs ~responder ~predictors m
    in
    let sum_responder = Array.fold_left ( +. ) 0. responder_vector in
    let mean = sum_responder /. float (Array.length responder_vector) in
    let tot_ss = ref 0. in
    let res_ss = ref 0. in
    let predicted i =
      let x = ref 0. in
      for j = 0 to Array.length r - 1 do
        x := !x +. (predictors_matrix.(i).(j) *. r.(j))
      done ;
      !x
    in
    for i = 0 to Array.length responder_vector - 1 do
      tot_ss := !tot_ss +. ((responder_vector.(i) -. mean) ** 2.) ;
      res_ss := !res_ss +. ((responder_vector.(i) -. predicted i) ** 2.)
    done ;
    1. -. (!res_ss /. !tot_ss)

  (* Ordinary Least Square *)
  let ols ?r_square:(do_r_square = false) ~responder ~predictors m =
    let matrix, vector = make_lr_inputs ~responder ~predictors m in
    match Linear_algebra.ols ~in_place:true matrix vector with
    | Ok estimates ->
        let r_square =
          if do_r_square then
            Some (r_square m ~responder ~predictors estimates)
          else None
        in
        Ok {labels= predictors; responder; estimates; r_square}
    | Error _ as err -> err

  let pp ?(colors = Label.Map.empty) ppf x =
    Fmt.pf ppf "{ @[" ;
    let style_responder =
      match Label.Map.find_opt x.responder colors with
      | Some x -> x
      | None -> `None
    in
    for i = 0 to Array.length x.labels - 1 do
      let style_label =
        match Label.Map.find_opt x.labels.(i) colors with
        | Some x -> x
        | None -> `None
      in
      Fmt.pf ppf "%a per %a = %f;@ "
        Fmt.(styled style_responder Label.pp)
        x.responder
        Fmt.(styled style_label Label.pp)
        x.labels.(i) x.estimates.(i)
    done ;
    Fmt.pf ppf "r-square = %a@] }" Fmt.(Dump.option float) x.r_square

  let pp ?colors ppf x =
    Fmt.result ~ok:(pp ?colors) ~error:Rresult.R.pp_msg ppf x
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
    { predictor: Label.t
    ; responder: Label.t
    ; mean_value: float
    ; constant: float
    ; max_value: float * float
    ; min_value: float * float
    ; standard_error: float }

  let pp ?(colors = Label.Map.empty) ppf t =
    let style_responder =
      match Label.Map.find_opt t.responder colors with
      | Some style -> style
      | None -> `None
    in
    let style_predictor =
      match Label.Map.find_opt t.predictor colors with
      | Some style -> style
      | None -> `None
    in
    Fmt.pf ppf "{ @[<hov>%a per %a = %f;@ standard-error = %f;@] }"
      (Fmt.styled style_responder Label.pp)
      t.responder
      (Fmt.styled style_predictor Label.pp)
      t.predictor t.mean_value t.standard_error

  let result_column ~predictor ~responder m =
    ( Measurement_raw.get ~label:predictor m
    , Measurement_raw.get ~label:responder m )

  let ransac ?(filter_outliers = true) ~predictor ~responder ml =
    let a = Array.map (result_column ~predictor ~responder) ml in
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
    { predictor
    ; responder
    ; mean_value
    ; constant
    ; min_value
    ; max_value
    ; standard_error }
end

type 'a t =
  | OLS : {predictors: Label.t array; r_square: bool} -> OLS.t t
  | RANSAC : {filter_outliers: bool; predictor: Label.t} -> RANSAC.t t

let ols ~r_square ~predictors = OLS {predictors; r_square}
let ransac ~filter_outliers ~predictor = RANSAC {filter_outliers; predictor}

let analyze : type a. a t -> Label.t -> Measurement_raw.t array -> a =
 fun kind label m ->
  match kind with
  | OLS {predictors; r_square} ->
      OLS.ols ~r_square ~predictors ~responder:label m
  | RANSAC {filter_outliers; predictor} ->
      RANSAC.ransac ~filter_outliers ~predictor ~responder:label m
