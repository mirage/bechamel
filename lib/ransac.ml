(* (c) OCamlPro - under MIT license *)

let random_permutation a =
  let len = Array.length a in
  for i = 0 to Array.length a - 1 do
    let n = Random.int (len - i) + i in
    let v1 = a.(i) in
    let v2 = a.(n) in
    a.(i) <- v2 ; a.(n) <- v1
  done ;
  a

let random_indices n =
  let a = Array.init n (fun i -> i) in
  random_permutation a

let random_partition n a =
  let indices = random_indices (Array.length a) in
  ( Array.init n (fun i -> a.(indices.(i)))
  , Array.init (Array.length a - n) (fun i -> a.(indices.(i + n))) )

let array_filter f a =
  let in_size = ref 0 in
  for i = 0 to Array.length a - 1 do
    let v = a.(i) in
    if f v then (
      let after_in = !in_size in
      let v' = a.(after_in) in
      a.(i) <- v' ; a.(after_in) <- v ; incr in_size )
  done ;
  Array.sub a 0 !in_size

type ('a, 'b) input =
  { model: 'a array -> 'b
  ; data: 'a array
  ; subset_size: int
  ; rounds: int
  ; distance: 'a -> 'b -> float
  ; filter_distance: float
  ; minimum_valid: int
  ; error: 'a array -> 'b -> float }

type ('a, 'b) result = {model: 'b; input: 'a array; error: float}

let one_round (r : ('a, 'b) input) : ('a, 'b) result option =
  let in_subset, _out_of_subset =
    random_partition (min (Array.length r.data / 2) r.subset_size) r.data
  in
  let model = r.model in_subset in
  let fiting =
    array_filter (fun p -> r.distance p model < r.filter_distance) r.data
  in
  if Array.length fiting > r.minimum_valid then
    let input = Array.append in_subset fiting in
    let model = r.model input in
    Some {model; input; error= r.error input model}
  else None

let ransac r : (_, _) result option =
  let rec loop n (best : (_, _) result option) =
    if n >= r.rounds then best
    else
      let best =
        match (one_round r, best) with
        | res, None | None, res -> res
        | (Some {error; _} as new_best), Some {error= best_error; _}
          when error < best_error ->
            new_best
        | Some _, Some _ -> best
      in
      loop (n + 1) best
  in
  loop 0 None
