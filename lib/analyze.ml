module OLS = struct
  module Ci95 = struct
    (* 95% confidence interval *)
    type t = { r : float; l : float }

    let pp ppf x = Fmt.pf ppf "@[<hov>%f to %f@]" x.r x.l
    let bad = { r = neg_infinity; l = neg_infinity }
  end

  (* Linear regression inputs *)
  let make_lr_inputs ?indices ~responder ~predictors m =
    let responder_accessor = Measurement_raw.get ~label:responder in
    let predictors_accessor =
      Array.map (fun label -> Measurement_raw.get ~label) predictors
    in
    match indices with
    | Some indices ->
        ( Array.map
            (fun i -> Array.map (fun a -> a m.(i)) predictors_accessor)
            indices
        , Array.map (fun i -> responder_accessor m.(i)) indices )
    | None ->
        ( Array.init (Array.length m) (fun i ->
              Array.map (fun a -> a m.(i)) predictors_accessor)
        , Array.init (Array.length m) (fun i -> responder_accessor m.(i)) )

  type t =
    { predictors : string array
    ; responder : string
    ; value : (v, [ `Msg of string ]) result
    }

  and v =
    { estimates : float array
    ; ci95 : Ci95.t array option
    ; r_square : float option
    }

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
      done;
      !x
    in
    for i = 0 to Array.length responder_vector - 1 do
      tot_ss := !tot_ss +. ((responder_vector.(i) -. mean) ** 2.);
      res_ss := !res_ss +. ((responder_vector.(i) -. predicted i) ** 2.)
    done;
    1. -. (!res_ss /. !tot_ss)

  (* XXX(dinosaure): see core_bench and [(1/e)^bootstrap_threshold <
     0.05/predictors] which describe area on top of logarithm curve (where
     maximum seems close to 6~7). *)
  let bootstrap_threshold = 10

  let can_bootstrap ~responder ~predictors m =
    let matrix, _ = make_lr_inputs ~responder ~predictors m in
    let non_zero = Array.make (Array.length predictors) 0 in
    let non_zero_cols = ref 0 in
    Array.iter
      (fun row ->
        for i = 0 to Array.length non_zero - 1 do
          if row.(i) <> 0.0 then (
            non_zero.(i) <- non_zero.(i) + 1;
            if non_zero.(i) = bootstrap_threshold then incr non_zero_cols)
        done)
      matrix;
    if !non_zero_cols = Array.length non_zero then true else false

  let () = Random.self_init ()

  let random_indices_in_place ~max arr =
    let len = Array.length arr in
    for i = 0 to len - 1 do
      arr.(i) <- Random.int max
    done

  let quantile_of_array ?(failures = 0) ~len ~low ~high arr =
    Array.sort (compare : float -> float -> int) arr;
    let index q = int_of_float ((float len *. q) +. (0.5 *. float failures)) in
    let extended_get i = if i >= len then infinity else arr.(i) in
    let l = extended_get ((min : int -> int -> int) (index low) (len - 1)) in
    let r = extended_get ((max : int -> int -> int) (index high) failures) in
    Ci95.{ l; r }

  let bootstrap ~trials m ~responder ~predictors =
    let p = Array.length predictors in
    match can_bootstrap ~responder ~predictors m with
    | false -> assert false
    | true ->
        let bootstrap_fails = ref 0 in
        let indices = Array.make (Array.length m) 0 in
        let bootstrap_coeffs = Array.init p (fun _ -> Array.make trials 0.0) in
        for i = 0 to trials - 1 do
          random_indices_in_place indices ~max:(Array.length m);
          let matrix, vector =
            make_lr_inputs ~indices ~responder ~predictors m
          in
          match Linear_algebra.ols ~in_place:true matrix vector with
          | Ok bt_ols_result ->
              for p = 0 to p - 1 do
                bootstrap_coeffs.(p).(i) <- bt_ols_result.(p)
              done
          | _ ->
              incr bootstrap_fails;
              for p = 0 to p - 1 do
                bootstrap_coeffs.(p).(i) <- neg_infinity
              done
        done;
        Array.init p (fun i ->
            if trials = 0 then Ci95.bad
            else
              quantile_of_array bootstrap_coeffs.(i) ~failures:!bootstrap_fails
                ~len:trials ~low:0.025 ~high:0.975)

  (* Ordinary Least Square *)
  let ols ?bootstrap:(trials = 0) ?r_square:(do_r_square = false) ~responder
      ~predictors m =
    let matrix, vector = make_lr_inputs ~responder ~predictors m in
    match Linear_algebra.ols ~in_place:true matrix vector with
    | Ok estimates ->
        let r_square =
          if do_r_square then Some (r_square m ~responder ~predictors estimates)
          else None
        in
        let ci95 =
          match trials with
          | 0 -> None
          | trials -> Some (bootstrap ~trials ~responder ~predictors m)
        in
        { predictors; responder; value = Ok { estimates; ci95; r_square } }
    | Error _ as err -> { predictors; responder; value = err }

  let pp ~predictors ~responder ppf v =
    Fmt.pf ppf "{ @[";
    for i = 0 to Array.length predictors - 1 do
      Fmt.pf ppf "%s per %s = %f" responder predictors.(i) v.estimates.(i);
      (match v.ci95 with
      | Some ci95 -> Fmt.pf ppf " (confidence: %a)" Ci95.pp ci95.(i)
      | None -> ());
      Fmt.pf ppf ";@ "
    done;
    Fmt.pf ppf "r² = %a@] }" Fmt.(Dump.option float) v.r_square

  let pp ppf x =
    match x.value with
    | Ok v -> pp ~predictors:x.predictors ~responder:x.responder ppf v
    | Error (`Msg err) -> Format.fprintf ppf "%s" err

  let predictors { predictors; _ } = Array.to_list predictors
  let responder { responder; _ } = responder

  let estimates { value; _ } =
    match value with
    | Ok { estimates; _ } -> Some (Array.to_list estimates)
    | Error _ -> None

  let r_square { value; _ } =
    match value with Ok { r_square; _ } -> r_square | Error _ -> None
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
            (v *. v) +. acc)
          r 0.
      in
      sumvar /. len
    in
    let covariance_x_y =
      let sumcovar =
        Array.fold_right
          (fun (x, y) acc ->
            let v = (x -. mean_x) *. (y -. mean_y) in
            v +. acc)
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
    done;
    !acc /. float (Array.length data)

  let ransac_filter_distance (x, y) (a, b) =
    let level = max x (max y (max a b)) in
    abs_float ((a *. x) +. b -. y) /. level

  let ransac_param data =
    { Ransac.model = affine_adjustment
    ; data
    ; subset_size = 10
    ; rounds = 100
    ; distance = ransac_filter_distance
    ; filter_distance = 0.05
    ; minimum_valid = Array.length data / 3
    ; error = quality
    }

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
    { predictor : string
    ; responder : string
    ; mean_value : float
    ; constant : float
    ; max_value : float * float
    ; min_value : float * float
    ; standard_error : float
    }

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>%s per %s = %f;@ standard-error = %f;@] }" t.responder
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
        | Some { Ransac.model; _ } -> model
      else affine_adjustment a
    in
    let min_value =
      Array.fold_left
        (fun (row_min, val_min) (row, value) ->
          let value = (value -. constant) /. row in
          if val_min < value || value <= 0. then (row_min, val_min)
          else (row, value))
        (0., max_float) a
    in
    let correct_float f = classify_float f = FP_normal in
    let max_value =
      Array.fold_left
        (fun (row_max, val_max) (row, value) ->
          let value = (value -. constant) /. row in
          if val_max > value || not (correct_float value) then (row_max, val_max)
          else (row, value))
        (0., min_float) a
    in
    let standard_error = standard_error ~a:mean_value ~b:constant a in
    { predictor
    ; responder
    ; mean_value
    ; constant
    ; min_value
    ; max_value
    ; standard_error
    }

  let responder { responder; _ } = responder
  let predictor { predictor; _ } = predictor
  let mean { mean_value; _ } = mean_value
  let constant { constant; _ } = constant
  let min { min_value; _ } = min_value
  let max { max_value; _ } = max_value
  let error { standard_error; _ } = standard_error
end

type 'a t =
  | OLS :
      { predictors : string array; r_square : bool; bootstrap : int }
      -> OLS.t t
  | RANSAC : { filter_outliers : bool; predictor : string } -> RANSAC.t t

let ols ~r_square ~bootstrap ~predictors =
  OLS { predictors; r_square; bootstrap }

let ransac ~filter_outliers ~predictor = RANSAC { filter_outliers; predictor }

let one : type a. a t -> Measure.witness -> Benchmark.t -> a =
 fun kind e { lr = m; _ } ->
  let label = Measure.label e in
  match kind with
  | OLS { predictors; r_square; bootstrap } ->
      OLS.ols ~bootstrap ~r_square ~predictors ~responder:label m
  | RANSAC { filter_outliers; predictor } ->
      RANSAC.ransac ~filter_outliers ~predictor ~responder:label m

let all :
    type a.
       a t
    -> Measure.witness
    -> (string, Benchmark.t) Hashtbl.t
    -> (string, a) Hashtbl.t =
 fun kind e ms ->
  let ret = Hashtbl.create (Hashtbl.length ms) in
  Hashtbl.iter (fun name m -> Hashtbl.add ret name (one kind e m)) ms;
  ret

let merge :
    type a.
       a t
    -> Measure.witness list
    -> (string, a) Hashtbl.t list
    -> (string, (string, a) Hashtbl.t) Hashtbl.t =
 fun _ instances results ->
  let ret = Hashtbl.create (List.length instances) in
  List.iter2
    (fun instance result -> Hashtbl.add ret (Measure.label instance) result)
    instances results;
  ret
