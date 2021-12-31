open Bechamel

type t = { estimate : float; r_square : float option; ci95 : ci95 option }
and ci95 = { r : float; l : float }

let ci95_witness : ci95 Json_encoding.encoding =
  let open Json_encoding in
  let l = req "low" float in
  let r = req "high" float in
  conv (fun t -> (t.l, t.r)) (fun (l, r) -> { l; r }) (obj2 l r)

let witness : t Json_encoding.encoding =
  let open Json_encoding in
  let estimate = req "estimate" float in
  let r_square = opt "r_square" float in
  let ci95 = opt "ci95" ci95_witness in
  conv
    (fun t -> (t.estimate, t.r_square, t.ci95))
    (fun (estimate, r_square, ci95) -> { estimate; r_square; ci95 })
    (obj3 estimate r_square ci95)

let of_ols_result ~x_label ~y_label ols =
  let has_y_label = String.equal (Analyze.OLS.responder ols) y_label in
  let has_x_label =
    List.exists (String.equal x_label) (Analyze.OLS.predictors ols)
  in

  if (not has_y_label) || not has_x_label then
    Rresult.R.error_msgf "x:%s or y:%s does not exist in result: @[<hov>%a@]"
      x_label y_label Analyze.OLS.pp ols
  else
    match Analyze.OLS.estimates ols with
    | None ->
        Rresult.R.error_msgf "Result is errored: @[<hov>%a@]" Analyze.OLS.pp ols
    | Some estimates ->
        let predictors = Analyze.OLS.predictors ols in
        let estimate =
          let exception Found in
          let estimate = ref None in
          try
            List.iter2
              (fun predictor e ->
                if String.equal x_label predictor then (
                  estimate := Some e;
                  raise Found))
              predictors estimates;
            assert false
          with Found -> (
            match !estimate with
            | Some estimate -> estimate
            | None -> assert false)
        in
        Ok { estimate; r_square = Analyze.OLS.r_square ols; ci95 = None }
