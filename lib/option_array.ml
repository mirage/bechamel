type 'a t = 'a Cheap_option.t Uniform_array.t

let empty = Uniform_array.empty
let create ~len = Uniform_array.create ~len Cheap_option.none
let length = Uniform_array.length
let get t i = Cheap_option.to_option (Uniform_array.get t i)
let get_some_exn t i = Cheap_option.value_exn (Uniform_array.get t i)
let is_none t i = Cheap_option.is_none (Uniform_array.get t i)
let is_some t i = Cheap_option.is_some (Uniform_array.get t i)
let set t i x = Uniform_array.set t i (Cheap_option.of_option x)
let set_some t i x = Uniform_array.set t i (Cheap_option.some x)
let set_none t i = Uniform_array.set t i Cheap_option.none
