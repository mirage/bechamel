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
