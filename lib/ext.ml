(* (c) Frédéric Bour *)

module Make (Functor : S.FUNCTOR) = struct
  type t = ..

  module type Extension = sig
    type x
    type t += T of x
  end

  type 'a extension = (module Extension with type x = 'a)
  type instance = V : 'a * 'a Functor.t -> instance

  let handlers = Hashtbl.create 16

  module Injection (X : sig
    type t

    val instance : t Functor.t
  end) : Extension with type x = X.t = struct
    type x = X.t
    type t += T of x

    let () =
      let instance = X.instance in
      Hashtbl.add handlers
        (Stdlib.Obj.Extension_constructor.id [%extension_constructor T]
         [@alert "-deprecated"])
        (function T x -> V (x, instance) | _ -> raise Not_found)
  end

  let inj (type a) (f : a Functor.t) : a extension =
    (module Injection (struct
      type t = a

      let instance = f
    end))

  let rec iter t lst =
    let[@warning "-partial-match"] (f :: r) = lst in
    try f t with _ -> (iter [@tailcall]) t r

  let prj (t : t) =
    let uid =
      Stdlib.Obj.Extension_constructor.((id (of_val t) [@alert "-deprecated"]))
    in
    iter t (Hashtbl.find_all handlers uid)
end
