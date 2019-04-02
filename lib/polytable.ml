(* Copyright (c) 2016 Daniel C. Bünzli

   XXX(dinosaure): use an [array] instead a [Map.S]. *)

module TypeIdentifier = struct type _ t = .. end

module type TypeIdentifier = sig
  type t
  type _ TypeIdentifier.t += T : t TypeIdentifier.t
end

type 'a type_identifier = (module TypeIdentifier with type t = 'a)

module Option = struct
  let is_some = function Some _ -> true | None -> false
end

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
  type t = {contents: binding option array; len: int}

  let create ~len = {contents= Array.make len None; len}
  let mem t k = Option.is_some t.contents.(k.Key.uid)

  let add : t -> 'a key -> 'a -> unit =
   fun t k v ->
    if k.Key.uid >= t.len then invalid_arg "Polytable.add"
    else t.contents.(k.Key.uid) <- (Some (B (k, v)))

  let unit = ()

  let set : type a. t -> a key -> (a -> unit) -> unit =
   fun t k f ->
    if k.Key.uid >= t.len then unit
    else
      match t.contents.(k.Key.uid)
      with None -> assert false
         | Some (B (k', v)) -> (
             match Refl.eq k.Key.tid k'.Key.tid with
             | Some Refl.Refl -> f v
             | None -> unit )

  let rem : t -> 'a key -> unit =
   fun t k ->
    if k.Key.uid < t.len then t.contents.(k.Key.uid) <- None

  let find : type a. t -> a key -> a option =
   fun t k ->
    if k.Key.uid >= t.len then None
    else
      match t.contents.(k.Key.uid) with
      | None -> None
      | Some (B (k', v)) -> (
        match Refl.eq k.Key.tid k'.Key.tid with
        | Some Refl.Refl -> Some v
        | None -> None )
end
