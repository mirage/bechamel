type packed =
  | V : {
      fn : [ `Init ] -> 'a -> 'b;
      resource : int -> 'a array;
      free : 'a array -> unit;
    }
      -> packed

let always v _ = v
let unit n = Array.init n (always ())

module Elt = struct
  type t = { key : int; name : string; fn : packed }

  let unsafe_make ~name fn =
    {
      key = 0;
      name;
      fn =
        V
          {
            fn = (fun `Init -> Staged.unstage fn);
            resource = unit;
            free = always ();
          };
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
  {
    name;
    set =
      [
        {
          Elt.key = 0;
          Elt.name;
          Elt.fn =
            V
              {
                fn = (fun `Init -> Staged.unstage fn);
                resource = unit;
                free = always ();
              };
        };
      ];
  }

external unsafe_array_make : int -> 'a -> 'a array = "caml_make_vect"
external unsafe_array_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_array_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

let make_allocate f = function
  | 0 -> [||]
  | len ->
      let vs = unsafe_array_make len (f ()) in
      for i = 1 to len - 1 do
        unsafe_array_set vs i (f ())
      done ;
      vs

let make_free f arr =
  for i = 0 to Array.length arr - 1 do
    f (unsafe_array_get arr i)
  done

let make_with_resource ~name ~allocate ~free fn =
  {
    name;
    set =
      [
        {
          Elt.key = 0;
          Elt.name;
          Elt.fn =
            V
              {
                fn = (fun `Init -> Staged.unstage fn);
                resource = make_allocate allocate;
                free = make_free free;
              };
        };
      ];
  }

let make_indexed ~name ?(fmt : fmt_indexed = "%s:%d") ~args fn =
  {
    name;
    set =
      List.map
        (fun key ->
          {
            Elt.key;
            Elt.name = Fmt.str fmt name key;
            Elt.fn =
              V
                {
                  fn = (fun `Init -> Staged.unstage (fn key));
                  resource = unit;
                  free = always ();
                };
          })
        args;
  }

let make_indexed_with_resource ~name ?(fmt : fmt_indexed = "%s:%d") ~args
    ~allocate ~free fn =
  {
    name;
    set =
      List.map
        (fun key ->
          {
            Elt.key;
            Elt.name = Fmt.str fmt name key;
            Elt.fn =
              V
                {
                  fn = (fun `Init -> Staged.unstage (fn key));
                  resource = make_allocate (fun () -> allocate key);
                  free = make_free free;
                };
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
      List.map (fun t -> { t with Elt.name = Fmt.str fmt name t.Elt.name }) ts;
  }
