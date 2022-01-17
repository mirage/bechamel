open Bechamel

type t =
  { x_label : string
  ; y_label : string
  ; series : (string, Desc.t * Dataset.t * KDE.t option * OLS.t) Hashtbl.t
  }

let label_witness : string Json_encoding.encoding = Json_encoding.string

let witness ~compare : t Json_encoding.encoding =
  let open Json_encoding in
  let x_label = req "xLabel" label_witness in
  let y_label = req "yLabel" label_witness in
  let serie =
    let name = req "name" string in
    let dataset = req "dataset" Dataset.witness in
    let kde = opt "kde" KDE.witness in
    let ols = req "result" OLS.witness in
    let desc = req "description" Desc.witness in
    obj5 name desc dataset kde ols
  in
  let series = req "series" (list serie) in
  conv
    (fun t ->
      let l =
        Hashtbl.fold
          (fun k (desc, dataset, kde, ols) a ->
            (k, desc, dataset, kde, ols) :: a)
          t.series []
      in
      ( t.x_label
      , t.y_label
      , List.sort (fun (k0, _, _, _, _) (k1, _, _, _, _) -> compare k0 k1) l ))
    (fun (x_label, y_label, l) ->
      let series = Hashtbl.create (List.length l) in
      List.iter
        (fun (k, desc, dataset, kde, ols) ->
          Hashtbl.add series k (desc, dataset, kde, ols))
        l;
      { x_label; y_label; series })
    (obj3 x_label y_label series)

let of_ols_results ~x_label ~y_label ols_results raws =
  if not (Hashtbl.mem ols_results y_label) then
    Rresult.R.error_msgf "y:%s does not exist in OLS results" y_label
  else
    let results = Hashtbl.find ols_results y_label in
    let series = Hashtbl.create (Hashtbl.length results) in

    try
      Hashtbl.iter
        (fun serie ols ->
          let open Rresult.R in
          let Benchmark.{ stats; lr = raws; kde = raws_kde } =
            Hashtbl.find raws serie
          in
          let res =
            Dataset.of_measurement_raws ~x_label ~y_label raws >>= fun raws ->
            KDE.of_kde_raws ~label:y_label raws_kde >>= fun raws_kde ->
            OLS.of_ols_result ~x_label ~y_label ols >>| fun ols ->
            (stats, raws, raws_kde, ols)
          in
          match res with
          | Ok (stats, raws, raws_kde, ols) ->
              Hashtbl.add series serie (stats, raws, raws_kde, ols)
          | Error _ as err -> Rresult.R.error_msg_to_invalid_arg err)
        results;
      Ok { x_label; y_label; series }
    with Invalid_argument err -> Rresult.R.error_msg err

type value = [ `Null | `Bool of bool | `String of string | `Float of float ]

let flat json : Jsonm.lexeme list =
  let rec arr acc k = function
    | [] -> k (List.rev (`Ae :: acc))
    | (#value as x) :: r -> arr (x :: acc) k r
    | `A l :: r -> arr [ `As ] (fun l -> arr (List.rev_append l acc) k r) l
    | `O l :: r -> obj [ `Os ] (fun l -> arr (List.rev_append l acc) k r) l
  and obj acc k = function
    | [] -> k (List.rev (`Oe :: acc))
    | (n, x) :: r ->
        base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x
  and base k = function
    | `A l -> arr [ `As ] k l
    | `O l -> obj [ `Os ] k l
    | #value as x -> k [ x ]
  in

  base (fun l -> l) json

type buffer = bytes * int * int
type transmit = buffer -> buffer
type 'a or_error = ('a, [ `Msg of string ]) result

type 'a dst =
  | Manual : transmit -> buffer dst
  | Buffer : Buffer.t -> (Buffer.t -> unit or_error) dst
  | Channel : out_channel -> (out_channel -> unit or_error) dst

let manual transmit = Manual transmit

let buffer ~chunk =
  let buffer = Buffer.create chunk in
  Buffer buffer

let channel filename =
  let oc = open_out filename in
  Channel oc

type raws = (string, Benchmark.t) Hashtbl.t
type ols_results = (string, (string, Analyze.OLS.t) Hashtbl.t) Hashtbl.t

let emit :
    type a.
       dst:a dst
    -> a
    -> ?compare:(string -> string -> int)
    -> x_label:string
    -> y_label:string
    -> ols_results * raws
    -> unit or_error =
 fun ~dst a ?compare:(compare_label = String.compare) ~x_label ~y_label
     (ols_results, raw_results) ->
  let to_dst : type a. a dst -> Jsonm.dst = function
    | Manual _ -> `Manual
    | Buffer buffer -> `Buffer buffer
    | Channel oc -> `Channel oc
  in
  let encoder = Jsonm.encoder ~minify:true (to_dst dst) in
  let buf, off, len =
    match dst with
    | Manual _ ->
        let buf, off, len = a in
        (ref buf, ref off, ref len)
    | Buffer _ -> (ref Bytes.empty, ref 0, ref 0)
    | Channel _ -> (ref Bytes.empty, ref 0, ref 0)
  in
  let go json =
    let flat = flat json in

    List.iter
      (fun lexeme ->
        match Jsonm.encode encoder (`Lexeme lexeme) with
        | `Ok -> ()
        | `Partial -> (
            match dst with
            | Manual transmit ->
                let buf', off', len' =
                  transmit (!buf, !off, !len - Jsonm.Manual.dst_rem encoder)
                in
                buf := buf';
                off := off';
                len := len';
                Jsonm.Manual.dst encoder buf' off' len'
            | Buffer _ -> ()
            | Channel _ -> ()))
      flat;

    let rec go : type a. a dst -> a -> unit or_error =
     fun dst a ->
      match (Jsonm.encode encoder `End, dst) with
      | `Ok, Buffer buf -> a buf
      | `Ok, Channel oc -> a oc
      | `Ok, Manual _ -> Ok ()
      | `Partial, Manual transmit ->
          let buf', off', len' =
            transmit (!buf, !off, !len - Jsonm.Manual.dst_rem encoder)
          in
          buf := buf';
          off := off';
          len := len';
          Jsonm.Manual.dst encoder buf' off' len';
          go dst a
      (* XXX(dinosaure): [Jsonm] explains that these cases never occur. *)
      | `Partial, Buffer _ -> assert false
      | `Partial, Channel _ -> assert false
    in

    go dst a
  in

  let open Rresult.R in
  of_ols_results ~x_label ~y_label ols_results raw_results
  >>| Json_encoding.construct (witness ~compare:compare_label)
  >>= go
