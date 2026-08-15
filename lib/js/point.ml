type t = { x : float; y : float }

let make ~x ~y = { x; y }

let witness =
  let open Json_encoding in
  let x = req "x" float in
  let y = req "y" float in
  conv (fun { x; y } -> (x, y)) (fun (x, y) -> { x; y }) (obj2 x y)

let err_msgf fmt =
  let kerr _ = Error (`Msg (Format.flush_str_formatter ())) in
  Format.kfprintf kerr Format.str_formatter fmt
