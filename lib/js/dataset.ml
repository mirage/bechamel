open Bechamel

type t =
  { x_label : Label.t
  ; y_label : Label.t
  ; data : Point.t list }

let witness : t Json_encoding.encoding =
  let open Json_encoding in
  let x_label = req "xLabel" Label.Json.witness in
  let y_label = req "yLabel" Label.Json.witness in
  let data = req "data" (list Point.witness) in
  conv
    (fun t -> t.x_label, t.y_label, t.data)
    (fun (x_label, y_label, data) -> { x_label; y_label; data; })
    (obj3 x_label y_label data)

let of_measurement_raws ~x_label ~y_label raws =
  if not (Array.for_all (Measurement_raw.exists ~label:x_label) raws)
  then Fmt.invalid_arg "x-label:%a does not exist in measurement raws" Label.pp x_label ;
  if not (Array.for_all (Measurement_raw.exists ~label:y_label) raws)
  then Fmt.invalid_arg "y-label:%a does not exist in measurement raws" Label.pp y_label ;

  let to_point t =
    let x = Measurement_raw.get ~label:x_label t in
    let y = Measurement_raw.get ~label:y_label t in
    Point.make ~x ~y in
  let data = Array.map to_point raws in
  let data = Array.to_list data in

  { x_label; y_label; data; }

let construct = Json_encoding.construct witness

let deconstruct json = match Json_encoding.destruct witness json with
  | v -> Ok v
  | exception Invalid_argument msg -> Rresult.R.error_msg msg
