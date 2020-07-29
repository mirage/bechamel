open Bechamel

type t = Point.t list

let witness : t Json_encoding.encoding =
  let open Json_encoding in
  list Point.witness

let of_measurement_raws ~x_label ~y_label raws =
  if not (Array.for_all (Measurement_raw.exists ~label:x_label) raws) then
    Fmt.invalid_arg "x-label:%s does not exist in measurement raws" x_label;
  if not (Array.for_all (Measurement_raw.exists ~label:y_label) raws) then
    Fmt.invalid_arg "y-label:%s does not exist in measurement raws" y_label;

  let has_x_label =
    Array.for_all (Measurement_raw.exists ~label:x_label) raws
  in
  let has_y_label =
    Array.for_all (Measurement_raw.exists ~label:y_label) raws
  in

  if (not has_x_label) || not has_y_label then
    Rresult.R.error_msgf "x:%s or y:%s does not exist in dataset." x_label
      y_label
  else
    let to_point t =
      let x = Measurement_raw.get ~label:x_label t in
      let y = Measurement_raw.get ~label:y_label t in
      Point.make ~x ~y
    in
    let data = Array.map to_point raws in
    Ok (Array.to_list data)
