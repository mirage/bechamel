type ('a, 't) app

module Uniq : sig
  type t

  external inj : 'a -> ('a, t) app = "%identity"
  external prj : ('a, t) app -> 'a = "%identity"
  val unit : (unit, t) app
end = struct
  type t

  external inj : 'a -> ('a, t) app = "%identity"
  external prj : ('a, t) app -> 'a = "%identity"

  let unit = inj ()
end

module Multiple : sig
  type t

  external inj : 'a array -> ('a, t) app = "%identity"
  external prj : ('a, t) app -> 'a array = "%identity"
end = struct
  type t

  external inj : 'a array -> ('a, t) app = "%identity"
  external prj : ('a, t) app -> 'a array = "%identity"
end

type packed =
  | V :
      { fn : [ `Init ] -> 'a -> 'b
      ; kind : ('a, 'v, 't) kind
      ; allocate : 'v -> ('a, 't) app
      ; free : ('a, 't) app -> unit
      }
      -> packed

and ('a, 'v, 'k) kind =
  | Uniq : ('a, unit, Uniq.t) kind
  | Multiple : ('a, int, Multiple.t) kind

let uniq = Uniq
let multiple = Multiple
let always v _ = v
let ( <.> ) f g x = f (g x)

module Elt = struct
  type t = { key : int; name : string; fn : packed }

  let unsafe_make ~name fn =
    { key = 0
    ; name
    ; fn =
        V
          { fn = (fun `Init -> Staged.unstage fn)
          ; kind = Uniq
          ; allocate = always Uniq.unit
          ; free = always ()
          }
    }

  let key { key; _ } = key
  let name { name; _ } = name
  let fn { fn; _ } = fn
end

type fmt_indexed =
  (string -> int -> string, Format.formatter, unit, string) format4

type fmt_grouped =
  (string -> string -> string, Format.formatter, unit, string) format4

type t = { name : string; set : Elt.t list }

let make ~name fn =
  { name
  ; set =
      [ { Elt.key = 0
        ; Elt.name
        ; Elt.fn =
            V
              { fn = (fun `Init -> Staged.unstage fn)
              ; kind = Uniq
              ; allocate = always Uniq.unit
              ; free = always ()
              }
        }
      ]
  }

open Unsafe

let make_multiple_allocate f = function
  | 0 -> [||]
  | len ->
      let vs = unsafe_array_make len (f ()) in
      for i = 1 to len - 1 do
        unsafe_array_set vs i (f ())
      done;
      vs

let make_multiple_free f arr =
  for i = 0 to Array.length arr - 1 do
    f (unsafe_array_get arr i)
  done

let make_with_resource :
    type a v k.
       name:string
    -> (a, v, k) kind
    -> allocate:(unit -> a)
    -> free:(a -> unit)
    -> (a -> 'b) Staged.t
    -> t =
 fun ~name kind ~allocate ~free fn ->
  match kind with
  | Uniq ->
      { name
      ; set =
          [ { Elt.key = 0
            ; Elt.name
            ; Elt.fn =
                V
                  { fn = (fun `Init -> Staged.unstage fn)
                  ; allocate = Uniq.inj <.> allocate
                  ; free = free <.> Uniq.prj
                  ; kind = Uniq
                  }
            }
          ]
      }
  | Multiple ->
      { name
      ; set =
          [ { Elt.key = 0
            ; Elt.name
            ; Elt.fn =
                V
                  { fn = (fun `Init -> Staged.unstage fn)
                  ; allocate = Multiple.inj <.> make_multiple_allocate allocate
                  ; free = make_multiple_free free <.> Multiple.prj
                  ; kind = Multiple
                  }
            }
          ]
      }

let make_indexed ~name ?(fmt : fmt_indexed = "%s:%d") ~args fn =
  { name
  ; set =
      List.map
        (fun key ->
          { Elt.key
          ; Elt.name = Fmt.str fmt name key
          ; Elt.fn =
              V
                { fn = (fun `Init -> Staged.unstage (fn key))
                ; kind = Uniq
                ; allocate = always Uniq.unit
                ; free = always ()
                }
          })
        args
  }

let make_indexed_with_resource :
    type a f g.
       name:string
    -> ?fmt:fmt_indexed
    -> args:int list
    -> (a, f, g) kind
    -> allocate:(int -> a)
    -> free:(a -> unit)
    -> (int -> (a -> 'b) Staged.t)
    -> t =
 fun ~name ?(fmt : fmt_indexed = "%s:%d") ~args kind ~allocate ~free fn ->
  match kind with
  | Uniq ->
      { name
      ; set =
          List.map
            (fun key ->
              { Elt.key
              ; Elt.name = Fmt.str fmt name key
              ; Elt.fn =
                  V
                    { fn = (fun `Init -> Staged.unstage (fn key))
                    ; kind = Uniq
                    ; allocate = (fun () -> Uniq.inj (allocate key))
                    ; free = free <.> Uniq.prj
                    }
              })
            args
      }
  | Multiple ->
      { name
      ; set =
          List.map
            (fun key ->
              { Elt.key
              ; Elt.name = Fmt.str fmt name key
              ; Elt.fn =
                  V
                    { fn = (fun `Init -> Staged.unstage (fn key))
                    ; kind = Multiple
                    ; allocate =
                        Multiple.inj
                        <.> make_multiple_allocate (fun () -> allocate key)
                    ; free = make_multiple_free free <.> Multiple.prj
                    }
              })
            args
      }

let name { name; _ } = name
let names { set; _ } = List.map Elt.name set
let elements { set; _ } = set
let expand ts = List.concat (List.map (fun t -> t.set) ts)

let make_grouped ~name ?(fmt : fmt_grouped = "%s/%s") ts =
  let ts = expand ts in
  { name
  ; set =
      List.map (fun t -> { t with Elt.name = Fmt.str fmt name t.Elt.name }) ts
  }
