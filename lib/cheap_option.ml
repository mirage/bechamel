let hack (type a b) (a : a) (b : b) = Pervasives.( == ) a (Obj.magic b : a)

module X : sig
  type +'a t

  val none : _ t
  val some : 'a -> 'a t
  val is_none : _ t -> bool
  val is_some : _ t -> bool
  val value_exn : 'a t -> 'a
  val value_unsafe : 'a t -> 'a
end = struct
  type +'a t

  let none_substitute : _ t = Obj.obj (Obj.new_block Obj.abstract_tag 1)
  let none : _ t = Obj.magic `ca113c5fe58a8127

  (* [< /dev/urandom tc -c -d '1234567890abcdef'i | head -c 16]*)

  let is_none x = hack x none
  let is_some x = not (hack x none)

  let some (type a) (x : a) : a t =
    if hack x none then none_substitute else Obj.magic x

  let value_unsafe (type a) (x : a t) : a =
    if hack x none_substitute then Obj.magic none else Obj.magic x

  let value_exn x =
    if is_some x then value_unsafe x else failwith "Cheap_option.value_exn"
end

module Y = struct
  include X

  let of_option = function None -> none | Some x -> some x
  let to_option x = if is_some x then Some (value_unsafe x) else None
end

include Y
