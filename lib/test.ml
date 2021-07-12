type packed = V : ([ `Init ] -> unit -> 'a) -> packed

module Elt = struct
        type t = { key : int; name : string; fn : packed; finalizer: unit -> unit; }

  let unsafe_make ?(finalizer= ignore) ~name fn =
    { key = 0; name; fn = V (fun `Init -> Staged.unstage fn); finalizer; }

  let key { key; _ } = key

  let name { name; _ } = name

  let fn { fn; _ } = fn

  let finalizer { finalizer; _ } = finalizer
end

type fmt_indexed =
  (string -> int -> string, Format.formatter, unit, string) format4

type fmt_grouped =
  (string -> string -> string, Format.formatter, unit, string) format4

type t = { name : string; set : Elt.t list }

let make ~name ?(finalizer = ignore) fn =
  {
    name;
    set =
            [ { Elt.key = 0; Elt.name; Elt.fn = V (fun `Init -> Staged.unstage fn); finalizer; } ];
  }

let make_indexed ~name ?(finalizer= fun _ -> ignore) ?(fmt : fmt_indexed = "%s:%d") ~args fn =
  {
    name;
    set =
      List.map
        (fun key ->
          {
            Elt.key;
            Elt.name = Fmt.strf fmt name key;
            Elt.fn = V (fun `Init -> Staged.unstage (fn key));
            Elt.finalizer= finalizer key
          })
        args;
  }

let name { name; _ } = name

let names { set; _ } = List.map Elt.name set

let elements { set; _ } = set

let expand ts = List.concat (List.map (fun t -> t.set) ts)

let make_grouped ~name ?(fmt : fmt_grouped = "%s/%s") ts =
  let ts = expand ts in
  {
    name;
    set =
      List.map (fun t -> { t with Elt.name = Fmt.strf fmt name t.Elt.name }) ts;
  }
