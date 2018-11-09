module X : sig
  type 'a t

  val empty : 'a t
  val create : len:int -> 'a -> 'a t
  val unsafe_create_uninitialized : len:int -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val swap : _ t -> int -> int -> unit
  val length : 'a t -> int
end = struct
  type 'a t = Obj_array.t

  let empty = Obj_array.empty
  let create ~len x = Obj_array.create ~len (Obj.repr x)
  let unsafe_create_uninitialized ~len = Obj_array.create_null ~len
  let swap t i j = Obj_array.swap t i j
  let get arr i = Obj.obj (Obj_array.get arr i)
  let set arr i x = Obj_array.set arr i (Obj.repr x)
  let length = Obj_array.length
end

include X

let init ~f len =
  if len < 0 then invalid_arg "Uniform_array.init"
  else
    let res = unsafe_create_uninitialized ~len in
    for i = 0 to len - 1 do
      set res i (f i)
    done ;
    res

let of_array arr = init ~f:(Array.unsafe_get arr) (Array.length arr)
let to_array t = Array.init (length t) (fun i -> get t i)
