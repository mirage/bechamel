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
        ((Stdlib.Obj.extension_id [%extension_constructor T])[@warning "-3"])
        (function T x -> V (x, instance) | _ -> raise Not_found)
  end

  let inj (type a) (f : a Functor.t) : a extension =
    ( module Injection (struct
      type t = a

      let instance = f
    end) )

  let proj (t : t) =
    let rec go = function
      | [] -> assert false (* totality *)
      | x :: r -> ( try x t with Not_found -> go r )
    in
    go
      (Hashtbl.find_all handlers
         (Stdlib.Obj.(extension_id (extension_constructor t)))[@warning "-3"])
end
