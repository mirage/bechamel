type t = {run: float; measures: float array; labels: Label.t array}

let make ~measures ~labels run =
  if Array.length measures <> Array.length labels then
    invalid_arg "Measures and labels differ"
  else {run; measures; labels}

exception Find

let get_index ~label m =
  let i0 = ref 0 in
  try
    while !i0 < Array.length m.labels do
      if Label.equal m.labels.(!i0) label then raise Find ;
      incr i0
    done ;
    raise Not_found
  with Find -> !i0

let get ~label m =
  let i = get_index ~label m in
  m.measures.(i)

module Map = Map.Make (String)

let pp ?(colors = Map.empty) ppf x =
  Fmt.pf ppf "{ @[<hov>run = %f;@ " x.run ;
  for i = 0 to Array.length x.labels - 1 do
    let style =
      match Map.find_opt (x.labels.(i) :> string) colors with
      | Some x -> x
      | None -> `None
    in
    Fmt.pf ppf "%s = %a;@ "
      (x.labels.(i) :> string)
      Fmt.(styled style float)
      x.measures.(i)
  done ;
  Fmt.pf ppf "#end@] }"
