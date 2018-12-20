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

let run t = t.run

let get ~label m =
  let i = get_index ~label m in
  m.measures.(i)

module Json = struct
  let witness =
    let unzip l =
      let rec go (l1, l2) = function
        | [] -> (List.rev l1, List.rev l2)
        | (a, b) :: r -> go (a :: l1, b :: l2) r
      in
      go ([], []) l
    in
    let open Json_encoding in
    conv
      (fun m ->
        ("run", m.run)
        :: List.map
             (fun label -> (Label.to_string label, get ~label m))
             (Array.to_list m.labels) )
      (fun l ->
        match List.assoc "run" l with
        | run ->
            let values = List.filter (fun (label, _) -> label <> "run") l in
            let labels, measures = unzip values in
            { run
            ; measures= Array.of_list measures
            ; labels= Array.map Label.of_string (Array.of_list labels) }
        | exception Not_found ->
            Fmt.invalid_arg
              "Impossible to deserialize input, run field is missing." )
      (assoc float)

  let construct = Json_encoding.construct witness

  let deconstruct json =
    match Json_encoding.destruct witness json with
    | v -> Ok v
    | exception Invalid_argument msg -> Rresult.R.error_msg msg
end

module Map = Map.Make (String)

let pp ?(colors = Map.empty) ppf x =
  Fmt.pf ppf "{ @[<hov>run = %f;@ " x.run ;
  for i = 0 to Array.length x.labels - 1 do
    let style =
      match Map.find (x.labels.(i) :> string) colors with
      | x -> x
      | exception Not_found -> `None
    in
    Fmt.pf ppf "%s = %a;@ "
      (x.labels.(i) :> string)
      Fmt.(styled style float)
      x.measures.(i)
  done ;
  Fmt.pf ppf "#end@] }"
