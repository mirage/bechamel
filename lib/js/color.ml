type t = int * int * int * float option

let black = (0, 0, 0, None)

let witness =
  let open Json_encoding in
  let fmt : _ format6 = "rgba(%d, %d, %d, %f)" in
  conv
    (fun (r, g, b, a) -> Fmt.strf fmt r g b (match a with Some a -> a | None -> 0.))
    (fun input ->
       let r = ref 0 and g = ref 0 and b = ref 0 and a = ref 0. in
       Scanf.sscanf input fmt (fun r' g' b' a' -> r := r'; g := g'; b := b'; a := a') ;
       (!r, !g, !b, Some !a))
    string
