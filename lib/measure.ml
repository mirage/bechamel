type 'a impl = (module S.MEASURE with type witness = 'a)

module Ext = Ext.Make (struct
  type 'a t = 'a impl
end)

type 'a measure = 'a Ext.extension

type witness = Ext.t

let register : type w. w impl -> w measure =
 fun (module M) ->
  (* if Hashtbl.mem labels (M.label ()) then Fmt.invalid_arg "Label %s already
     exist, find a new one." (M.label ()) ; *)
  Ext.inj (module M)

let instance : type w. w impl -> w measure -> witness =
 fun (module M) x ->
  let module Ext = (val x) in
  Ext.T (M.make ())

let load : witness -> unit =
 fun v ->
  let (Ext.V (m, (module M))) = Ext.prj v in
  M.load m

let unload : witness -> unit =
 fun v ->
  let (Ext.V (m, (module M))) = Ext.prj v in
  M.unload m

let label : witness -> string =
 fun v ->
  let (Ext.V (m, (module M))) = Ext.prj v in
  M.label m

type value = Ext.instance = V : 'w * 'w impl -> value

let prj w = Ext.prj w

let run = "run"
