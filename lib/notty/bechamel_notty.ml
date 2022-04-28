open Bechamel
open Notty

type 'a result = (string, 'a) Hashtbl.t
type 'a results = (string, 'a result) Hashtbl.t

let ( <.> ) f g x = f (g x)

module Unit = struct
  let units = Hashtbl.create 16

  let add instance unit =
    if String.length unit > 5 then
      Fmt.invalid_arg "A unit shoud be smaller than 5 bytes: %s" unit;
    Hashtbl.add units (Measure.label instance) unit

  let label label = I.string ~attr:A.empty label

  let unit_of_label label =
    try Hashtbl.find units label with Not_found -> label
end

module Order = struct
  type t = Increasing | Decreasing

  let increasing = Increasing
  let decreasing = Decreasing
  let orders = Hashtbl.create 16
  let add instance order = Hashtbl.add orders (Measure.label instance) order

  let order_of_label label =
    try Hashtbl.find orders label with Not_found -> Increasing

  let compare_of_order order a b =
    match order with
    | Increasing -> (compare : float option -> float option -> int) a b
    | Decreasing -> (compare : float option -> float option -> int) b a

  let compare_of_label = compare_of_order <.> order_of_label
end

let fmt_value : _ format6 = "%6.04f %s/%s"
let max_length_of_values = 23

let ols_value : predictor:string -> Analyze.OLS.t -> image =
 fun ~predictor v ->
  if not (List.mem predictor (Analyze.OLS.predictors v)) then
    Fmt.invalid_arg "Predictor %s was not computed in %a." predictor
      Analyze.OLS.pp v;

  let attr =
    match Analyze.OLS.r_square v with
    | Some r_square ->
        if r_square <= 0.5 then A.(bg red ++ st bold)
        else if r_square <= 0.75 then A.(fg red)
        else if r_square <= 0.90 then A.(fg yellow)
        else if r_square <= 0.98 then A.(fg green)
        else A.(bg green ++ st bold)
    | None -> A.(fg white)
  in
  let responder = Analyze.OLS.responder v in
  let unit_responder = Unit.unit_of_label responder in
  let unit_predictor = Unit.unit_of_label predictor in
  match Analyze.OLS.estimates v with
  | None -> I.string ~attr:A.(bg red ++ st bold) "#none"
  | Some values -> (
      match
        List.fold_left2
          (fun a v p -> if String.equal p predictor then Some v else a)
          None values (Analyze.OLS.predictors v)
      with
      | Some value ->
          let s = Fmt.str fmt_value value unit_responder unit_predictor in
          I.string ~attr s
      | None -> assert false)

(* XXX(dinosaure): should never occur. *)

let ransac_value : Analyze.RANSAC.t -> image =
 fun v ->
  let attr =
    let error = Analyze.RANSAC.error v in
    if error <= 0.5 then A.(bg red ++ st bold)
    else if error <= 0.75 then A.(fg red)
    else if error <= 0.90 then A.(fg yellow)
    else if error <= 0.98 then A.(fg green)
    else A.(bg green ++ st bold)
  in
  let responder = Analyze.RANSAC.responder v in
  let predictor = Analyze.RANSAC.predictor v in
  let unit_responder = Unit.unit_of_label responder in
  let unit_predictor = Unit.unit_of_label predictor in
  let s =
    Fmt.str fmt_value (Analyze.RANSAC.mean v) unit_responder unit_predictor
  in
  I.string ~attr s

let corner_tl ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x256D)
let corner_tr ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x256E)
let corner_bl ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x2570)
let corner_br ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x256F)
let break_t ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x252C)
let break_b ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x2534)
let break_l ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x251C)
let break_r ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x2524)
let cross ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x253C)
let line ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x2500)
let sideline ?(attr = A.empty) = I.uchar ~attr (Uchar.of_int 0x2502)
let grid xxs = xxs |> List.map I.hcat |> I.vcat

type rect = { w : int; h : int }

exception Break

let hashtbl_choose hashtbl =
  let k = ref None in
  let v = ref None in

  if Hashtbl.length hashtbl = 0 then
    Fmt.invalid_arg "hashtbl_choose: empty hashtbl";

  try
    Hashtbl.iter
      (fun k' v' ->
        k := Some k';
        v := Some v';
        raise Break)
      hashtbl;
    assert false
  with Break -> (
    match (!k, !v) with Some k, Some v -> (k, v) | _, _ -> assert false)

module One = struct
  let image_of_header ~rect result =
    let _, v = hashtbl_choose result in
    let responder = Analyze.OLS.responder v in
    let max_length_of_names =
      Hashtbl.fold (fun name _ -> max (String.length name)) result 0
    in

    grid
      [ [ corner_tl 1 1
        ; line (max_length_of_names + 4) 1
        ; break_t 1 1
        ; line (rect.w - 2 - 1 - (max_length_of_names + 4)) 1
        ; corner_tr 1 1
        ]
      ; [ sideline 1 1
        ; I.(string ~attr:A.(st italic) "name")
        ; I.void max_length_of_names 1
        ; sideline 1 1
        ; I.(string ~attr:A.empty responder |> hpad 2 0)
        ; I.void
            (rect.w
            - (max_length_of_names + 4 + 2 + 2 + 1 + String.length responder))
            1
        ; sideline 1 1
        ]
      ; [ break_l 1 1
        ; line (max_length_of_names + 4) 1
        ; cross 1 1
        ; line (rect.w - 2 - 1 - (max_length_of_names + 4)) 1
        ; break_r 1 1
        ]
      ]

  let image_of_field ~max_length_of_names ~rect ~predictor img (name, v) =
    let open Notty.Infix in
    let value = ols_value ~predictor v in

    let field =
      grid
        [ [ sideline 1 1
          ; I.(string ~attr:A.empty name |> hpad 2 0)
          ; I.void (max_length_of_names + 4 - 2 - String.length name) 1
          ; sideline 1 1
          ; I.(
              hsnap ~align:`Right
                (rect.w - 2 - 1 - (max_length_of_names + 4))
                value)
          ; sideline 1 1
          ]
        ]
    in
    img <-> field

  let best_and_worst_case (result : Analyze.OLS.t result) ~sort ~predictor ~rect
      =
    let tests = Hashtbl.fold (fun name v a -> (name, v) :: a) result [] in

    let values =
      List.map
        (fun (name, v) ->
          match Analyze.OLS.estimates v with
          | Some values ->
              List.fold_left2
                (fun a v p ->
                  if String.equal p predictor then (name, Some v) else a)
                (name, None) values (Analyze.OLS.predictors v)
          | None -> (name, None))
        tests
    in
    let tests = List.sort (fun (_, a) (_, b) -> sort a b) values in
    let (best, _), (worst, _) = (List.hd tests, List.hd (List.rev tests)) in
    let max_length_of_names =
      Hashtbl.fold (fun name _ -> max (String.length name)) result 0
    in

    grid
      [ [ break_l 1 1
        ; line (max_length_of_names + 4) 1
        ; cross 1 1
        ; line (rect.w - 2 - 1 - (max_length_of_names + 4)) 1
        ; break_r 1 1
        ]
      ; [ sideline 1 1
        ; I.(string ~attr:A.(st italic) "best")
        ; I.void max_length_of_names 1
        ; sideline 1 1
        ; I.(string ~attr:A.empty best |> hpad 2 0)
        ; I.void
            (rect.w - (max_length_of_names + 4 + 2 + 2 + 1 + String.length best))
            1
        ; sideline 1 1
        ]
      ; [ sideline 1 1
        ; I.(string ~attr:A.(st italic) "worst")
        ; I.void (max_length_of_names - 1) 1
        ; sideline 1 1
        ; I.(string ~attr:A.empty worst |> hpad 2 0)
        ; I.void
            (rect.w
            - (max_length_of_names + 4 + 2 + 2 + 1 + String.length worst))
            1
        ; sideline 1 1
        ]
      ; [ corner_bl 1 1
        ; line (max_length_of_names + 4) 1
        ; break_b 1 1
        ; line (rect.w - 2 - 1 - (max_length_of_names + 4)) 1
        ; corner_br 1 1
        ]
      ]

  let image_of_ols_result :
         ?sort:(string -> string -> int)
      -> rect:rect
      -> predictor:string
      -> Analyze.OLS.t result
      -> image =
   fun ?(sort = String.compare) ~rect ~predictor result ->
    let tests = Hashtbl.fold (fun name v a -> (name, v) :: a) result [] in
    let tests = List.sort (fun (a, _) (b, _) -> sort a b) tests in

    let header = image_of_header ~rect result in
    let max_length_of_names =
      Hashtbl.fold (fun name _ -> max (String.length name)) result 0
    in

    let header_and_body =
      List.fold_left
        (image_of_field ~max_length_of_names ~rect ~predictor)
        header tests
    in

    let open Notty.Infix in
    header_and_body
    <-> best_and_worst_case result
          ~sort:(Order.compare_of_label predictor)
          ~rect ~predictor
end

module Multiple = struct
  [@@@warning "-26-27"]

  let image_of_header ~rect (results : 'a results) =
    let instances = Hashtbl.fold (fun k _ a -> k :: a) results [] in
    let _, result = hashtbl_choose results in
    let max_length_of_names =
      Hashtbl.fold (fun name _ -> max (String.length name)) result 0
    in
    let max_length_of_instances =
      List.fold_right
        (fun label -> max (String.length label))
        instances max_length_of_values
    in
    let max_length_of_fields =
      max max_length_of_values max_length_of_instances
    in

    let tt =
      List.map
        (fun _ -> [ break_t 1 1; line (max_length_of_fields + 4) 1 ])
        instances
      |> List.concat
    in
    let tt = corner_tl 1 1 :: line (max_length_of_names + 4) 1 :: tt in
    let tt = tt @ [ corner_tr 1 1 ] in

    let cc =
      List.map
        (fun instance ->
          let rest = max_length_of_instances - String.length instance + 2 in
          [ sideline 1 1
          ; I.(string ~attr:A.empty instance |> I.hpad 2 0)
          ; I.void rest 0
          ])
        instances
      |> List.concat
    in
    let cc =
      sideline 1 1
      :: I.(string ~attr:A.(st italic) "name")
      :: I.void max_length_of_names 1
      :: cc
    in
    let cc = cc @ [ sideline 1 1 ] in

    let bb =
      List.map
        (fun _ -> [ cross 1 1; line (max_length_of_fields + 4) 1 ])
        instances
      |> List.concat
    in
    let bb = break_l 1 1 :: line (max_length_of_names + 4) 1 :: bb in
    let bb = bb @ [ break_r 1 1 ] in

    let open Notty in
    I.vcat [ I.hcat tt; I.hcat cc; I.hcat bb ]

  let image_of_ols_fields ~max_length_of_names ~max_length_of_instances ~rect
      ~predictor img (name, vs) =
    let values = List.map (ols_value ~predictor) vs in
    let max_length_of_fields =
      max max_length_of_values max_length_of_instances
    in

    let ll =
      [ sideline 1 1
      ; I.(string ~attr:A.empty name |> hpad 2 0)
      ; I.void (max_length_of_names + 4 - 2 - String.length name) 1
      ]
    in
    let cc =
      List.map
        (fun value ->
          [ sideline 1 1
          ; I.(hsnap ~align:`Right (max_length_of_fields + 4)) value
          ])
        values
      |> List.concat
    in
    let rr = [ sideline 1 1 ] in

    let open Notty.Infix in
    img <-> I.hcat (ll @ cc @ rr)

  let image_of_ols_results :
         ?sort:(string -> string -> int)
      -> rect:rect
      -> predictor:string
      -> Analyze.OLS.t results
      -> image =
   fun ?(sort = String.compare) ~rect ~predictor results ->
    let header = image_of_header ~rect results in
    let instances = Hashtbl.fold (fun k _ a -> k :: a) results [] in
    let _, result = hashtbl_choose results in
    let max_length_of_names =
      Hashtbl.fold (fun name _ -> max (String.length name)) result 0
    in
    let max_length_of_instances =
      List.fold_right
        (fun label -> max (String.length label))
        instances max_length_of_values
    in

    let tests = Hashtbl.fold (fun name _ a -> name :: a) result [] in
    let tests = List.sort sort tests in

    let header_and_body =
      List.fold_left
        (fun img name ->
          let vs =
            Hashtbl.fold
              (fun label result a -> Hashtbl.find result name :: a)
              results []
          in
          image_of_ols_fields ~max_length_of_names ~max_length_of_instances
            ~rect ~predictor img (name, vs))
        header tests
    in

    let max_length_of_fields =
      max max_length_of_values max_length_of_instances
    in

    let bottom =
      List.map
        (fun _ -> [ break_b 1 1; line (max_length_of_fields + 4) 1 ])
        instances
      |> List.concat
    in
    let bottom = corner_bl 1 1 :: line (max_length_of_names + 4) 1 :: bottom in
    let bottom = bottom @ [ corner_br 1 1 ] in

    let open Notty.Infix in
    header_and_body <-> I.hcat bottom
end
