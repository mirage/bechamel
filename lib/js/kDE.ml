open Bechamel

type t = float list

let witness : t Json_encoding.encoding =
  let open Json_encoding in
  list float

let of_kde_raws ~label kde_raws =
  match kde_raws with
  | None -> Ok None
  | Some kde_raws ->
      let has_label = Array.for_all (Measurement_raw.exists ~label) kde_raws in

      if not has_label then
        Rresult.R.error_msgf "%s does not exist in kde data." label
      else
        let to_float t = Measurement_raw.get ~label t in
        let data = Array.map to_float kde_raws in
        Ok (Some (Array.to_list data))
