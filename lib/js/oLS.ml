open Bechamel

type t =
  { x_label : Label.t
  ; y_label : Label.t
  ; estimate : float
  ; r_square : float option
  ; ci95 : ci95 option }
and ci95 = { r : float; l : float; }

let ci95_witness : ci95 Json_encoding.encoding =
  let open Json_encoding in
  let l = req "low" float in
  let r = req "high" float in
  conv
    (fun t -> t.l, t.r)
    (fun (l, r) -> { l; r; })
    (obj2 l r)

let witness : t Json_encoding.encoding =
  let open Json_encoding in
  let x_label = req "xLabel" Label.Json.witness in
  let y_label = req "yLabel" Label.Json.witness in
  let estimate = req "estimate" float in
  let r_square = opt "r_square" float in
  let ci95 = opt "ci95" ci95_witness in
  conv
    (fun t -> t.x_label, t.y_label, t.estimate, t.r_square, t.ci95)
    (fun (x_label, y_label, estimate, r_square, ci95) ->
       { x_label; y_label; estimate; r_square; ci95; })
    (obj5 x_label y_label estimate r_square ci95)

let of_ols_result ~x_label ~y_label ols =
  if not (Label.equal (Analyze.OLS.responder ols) y_label)
  then Fmt.invalid_arg "y-label:%a (expect:%a) does not exist in OLS result" Label.pp y_label Label.pp (Analyze.OLS.responder ols) ;
  if not (List.exists (Label.equal x_label) (Analyze.OLS.predictors ols))
  then Fmt.invalid_arg "x-label:%a does not exist in OLS result" Label.pp x_label ;
  match Analyze.OLS.estimates ols with
  | None -> Fmt.invalid_arg "OLS results are not available"
  | Some estimates ->
    let predictors = Analyze.OLS.predictors ols in
    let estimate =
      let exception Found in
      let estimate = ref None in
      try List.iter2
            (fun predictor e ->
               if Label.equal x_label predictor
               then ( estimate := Some e
                    ; raise Found ))
            predictors estimates ; assert false
      with Found -> match !estimate with
        | Some estimate -> estimate
        | None -> assert false in
    { x_label
    ; y_label
    ; estimate
    ; r_square= Analyze.OLS.r_square ols
    ; ci95= None}
