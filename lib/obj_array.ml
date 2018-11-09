type t = Obj.t array

external create : int -> 'a -> 'a array = "caml_make_vect"

let create ~len x = create len x
let zero_obj = Obj.repr (0 : int)
let create_null ~len = create ~len zero_obj

let create ~len x =
  if Obj.tag x <> Obj.double_tag then create ~len x
  else
    let t = create_null ~len in
    let x = Sys.opaque_identity x in
    for i = 0 to len - 1 do
      Array.unsafe_set t i x
    done ;
    t

let length = Array.length

let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j) ;
  t.(j) <- tmp

let empty = [||]

type not_a_float = Not_a_float_0 | Not_a_float_1 of int

let _not_a_float_0 = Not_a_float_0
let _not_a_float_1 = Not_a_float_1 42

let get t i =
  Obj.repr ((Obj.magic (t : t) : not_a_float array).(i) : not_a_float)

let[@inline always] unsafe_set_with_caml_modify t i obj =
  Array.unsafe_set
    (Obj.magic (t : t) : not_a_float array)
    i
    (Obj.obj (Sys.opaque_identity obj) : not_a_float)

let[@inline always] unsafe_set_int_assuming_currently_int t i int =
  Array.unsafe_set (Obj.magic (t : t) : int array) i (Sys.opaque_identity int)

let set t i obj =
  let old_obj = get t i in
  if Obj.is_int old_obj && Obj.is_int obj then
    unsafe_set_int_assuming_currently_int t i (Obj.obj obj : int)
  else if not (Pervasives.( == ) old_obj obj) then
    unsafe_set_with_caml_modify t i obj
