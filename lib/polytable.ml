(* Copyright (c) 2016 Daniel C. Bünzli

   XXX(dinosaure): use an [array] instead a [Map.S]. *)

module TypeIdentifier = struct type _ t = .. end

module type TypeIdentifier = sig
  type t
  type _ TypeIdentifier.t += T : t TypeIdentifier.t
end

type 'a type_identifier = (module TypeIdentifier with type t = 'a)

module Refl = struct
  type ('a, 'b) t = Refl : ('a, 'a) t

  let refl = Refl
  let some_refl = Some refl
  let none = None

  let eq : type a b. a type_identifier -> b type_identifier -> (a, b) t option
      =
   fun a b ->
    let module A = (val a : TypeIdentifier with type t = a) in
    let module B = (val b : TypeIdentifier with type t = b) in
    match A.T with B.T -> some_refl | _ -> none
end

let type_identifier (type a) () =
  let module M = struct
    type t = a type _ TypeIdentifier.t += T : t TypeIdentifier.t
  end in
  (module M : TypeIdentifier with type t = a)

(* specialization: polymorphic compiler primitives can't be aliases as this
   does not play well with inlining - if aliased without a type annotation, the
   compiler would implement them using the generic code doing a C call, and
   it's this code that would be inlined - as a result we have to copy the
   declaration here. *)
external greaterequal : 'a -> 'a -> bool = "%greaterequal"

let greaterequal (x : int) y = greaterequal x y

module Make (Functor : S.FUNCTOR) = struct
  module Key = struct
    type 'a info = 'a Functor.t
    type 'a key = {uid: int; tid: 'a type_identifier; info: 'a info}

    let universal_identifier =
      let id = ref (-1) in
      fun () -> incr id ; !id

    let info k = k.info

    type t = V : 'a key -> t

    let equal (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid = 0
    let compare (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid
    let hash (V k) = k.uid
    let pack k = V k

    let create info =
      let uid = universal_identifier () in
      let tid = type_identifier () in
      {uid; tid; info}
  end

  type 'a key = 'a Key.key
  type binding = B : 'a key * 'a -> binding
  type t = {contents: binding Option_array.t; len: int}

  let create ~len = {contents= Option_array.create ~len; len}
  let mem t k = Option_array.is_some t.contents k.Key.uid

  let add : t -> 'a key -> 'a -> unit =
   fun t k v ->
    if k.Key.uid >= t.len then invalid_arg "Polytable.add"
    else Option_array.set_some t.contents k.Key.uid (B (k, v))

  let unit = ()

  let set : type a. t -> a key -> (a -> unit) -> unit =
   fun t k f ->
    if greaterequal k.Key.uid t.len then unit
    else
      match Option_array.get_some_exn t.contents k.Key.uid
      with B (k', v) -> (
        match Refl.eq k.Key.tid k'.Key.tid with
        | Some Refl.Refl -> f v
        | None -> unit )

  let rem : t -> 'a key -> unit =
   fun t k ->
    if k.Key.uid < t.len then Option_array.set_none t.contents k.Key.uid

  let find : type a. t -> a key -> a option =
   fun t k ->
    if k.Key.uid >= t.len then None
    else
      match Option_array.get t.contents k.Key.uid with
      | None -> None
      | Some (B (k', v)) -> (
        match Refl.eq k.Key.tid k'.Key.tid with
        | Some Refl.Refl -> Some v
        | None -> None )
end
