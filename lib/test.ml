type packed = V : ([`Init] -> unit -> 'a) -> packed

module Elt = struct
  type t = {key: int; name: string; fn: packed}

  let unsafe_make ~name fn =
    {key= 0; name; fn= V (fun `Init -> Staged.unstage fn)}

  let key {key; _} = key
  let name {name; _} = name
  let fn {fn; _} = fn
end

type t = {name: string; set: Elt.t list}

type fmt_indexed =
  (string -> int -> string, Format.formatter, unit, string) format4

type fmt_grouped =
  (string -> string -> string, Format.formatter, unit, string) format4

let make ~name fn =
  { name
  ; set= [{Elt.key= 0; Elt.name; Elt.fn= V (fun `Init -> Staged.unstage fn)}]
  }

let make_indexed ~name ?(fmt : fmt_indexed = "%s:%d") ~args fn =
  { name
  ; set=
      List.map
        (fun key ->
          { Elt.key
          ; Elt.name= Fmt.strf fmt name key
          ; Elt.fn= V (fun `Init -> Staged.unstage (fn key)) } )
        args }

let name {name; _} = name
let set {set; _} = set
let expand ts = List.concat (List.map (fun t -> t.set) ts)

let make_grouped ~name ?(fmt : fmt_grouped = "%s/%s") ts =
  let ts = expand ts in
  { name
  ; set= List.map (fun t -> {t with Elt.name= Fmt.strf fmt name t.Elt.name}) ts
  }
