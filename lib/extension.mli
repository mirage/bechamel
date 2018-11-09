module Make (Functor : S.FUNCTOR) : sig
  type t = private ..

  module type Extension = sig
    type x
    type t += T of x
  end

  type 'a extension = (module Extension with type x = 'a)
  type instance = V : 'a * 'a Functor.t -> instance

  module Injection (X : sig
    type t

    val instance : t Functor.t
  end) : Extension with type x = X.t

  val inj : 'a Functor.t -> 'a extension
  val proj : t -> instance
end
