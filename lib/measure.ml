let labels = Hashtbl.create 32

module Safe (M : S.MEASURE with type label = string) :
  S.MEASURE
  with type witness = M.witness
   and type value = M.value
   and type label = Label.t = struct
  type nonrec witness = M.witness
  type nonrec value = M.value
  type label = Label.t

  let () =
    let witness = M.make () in
    let label = M.label witness in
    try
      let () = Hashtbl.find labels label in
      Fmt.invalid_arg "Label %s already exists" label
    with Not_found ->
      Hashtbl.add labels label () ;
      ()

  let make () = M.make ()
  let load witness = M.load witness
  let unload witness = M.unload witness
  let diff a b = M.diff a b
  let float v = M.float v
  let label witness = Label.of_string (M.label witness)
  let epsilon () = M.epsilon ()
  let blit witness value = M.blit witness value
end

module type UNSAFE = S.MEASURE with type label = string
module type S = S.MEASURE with type label = Label.t

module Switch = struct
  type m = X
  type v = Y

  let _ = X
  let _ = Y

  type ('k, 'a) t =
    | Measure : ('w, 'a) measure -> (m, ('w, 'a) bind) t
    | Value : 'a value -> (v, 'a) t

  and ('w, 'a) measure = (module S with type witness = 'w and type value = 'a)

  and 'a value = (module S with type value = 'a)

  and ('w, 'a) bind = 'w * 'a

  module Measure (X : S) = struct let x = Measure (module X) end
  module Value (X : S) = struct let x = Value (module X) end

  let measure (type w a) (t : (m, (w, a) bind) t) : (w, a) measure =
    match t with Measure (module X) -> (module X)

  let value (type a) (t : (v, a) t) : a value =
    match t with Value (module X) -> (module X)

  let to_value (type w a) (t : (m, (w, a) bind) t) : (v, a) t =
    match t with Measure (module X) -> Value (module X : S with type value = a)

  let blit (type w a) (Measure (module X) : (m, (w, a) bind) t) (w : w) :
      a -> unit =
    X.blit w
end

module Measure = struct type 'a t = (Switch.m, 'a) Switch.t end
module Value = struct type 'a t = (Switch.v, 'a) Switch.t end
module Extension = Extension.Make (Measure)

let make (type w a)
    (module X : UNSAFE with type witness = w and type value = a) :
    (w, a) Switch.bind Extension.extension =
  let module Maker = Switch.Measure (Safe (X)) in
  let measure = Maker.x in
  Extension.inj measure

let instance (type w a)
    (module X : UNSAFE with type witness = w and type value = a)
    (x : (w, a) Switch.bind Extension.extension) : Extension.t =
  let module Ext = (val x) in
  Ext.T (X.make (), X.epsilon ())

let ( ++ ) : Extension.t -> Extension.t -> Label.t array =
 fun a b ->
  let (Extension.V (ia, ma)) = Extension.proj a in
  let (Extension.V (ib, mb)) = Extension.proj b in
  let (Switch.Measure (module MA)) = ma in
  let (Switch.Measure (module MB)) = mb in
  let wa = fst ia in
  let wb = fst ib in
  [|MA.label wa; MB.label wb|]

let label : Extension.t -> Label.t =
 fun x ->
  let (Extension.V (i, m)) = Extension.proj x in
  let (Switch.Measure (module M)) = m in
  let w = fst i in
  M.label w

let with_run : Extension.t list -> Label.t array =
 fun l ->
  Array.append [|Label.of_string "run"|] (Array.of_list (List.map label l))
