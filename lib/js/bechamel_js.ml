open Bechamel

type t =
  { x_label : Label.t
  ; y_label : Label.t
  ; series : (string, Dataset.t * OLS.t) Hashtbl.t }

let witness ~compare : t Json_encoding.encoding =
  let open Json_encoding in
  let x_label = req "xLabel" Label.Json.witness in
  let y_label = req "yLabel" Label.Json.witness in
  let serie =
    let name = req "name" string in
    let dataset = req "dataset" Dataset.witness in
    let ols = req "ols" OLS.witness in
    obj3 name dataset ols in
  let series = req "series" (list serie) in
  conv
    (fun t ->
       let l = Hashtbl.fold (fun k (dataset, ols) a -> (k, dataset, ols) :: a) t.series [] in
       t.x_label, t.y_label, List.sort (fun (k0, _, _) (k1, _, _) -> compare k0 k1) l)
    (fun (x_label, y_label, l) ->
       let series = Hashtbl.create (List.length l) in
       List.iter (fun (k, dataset, ols) -> Hashtbl.add series k (dataset, ols)) l ;
       { x_label; y_label; series; })
    (obj3 x_label y_label series)

let of_ols_results ~x_label ~y_label ols_results raws =
  if not (Hashtbl.mem ols_results y_label)
  then Fmt.invalid_arg "y-label:%a does not exist in OLS results" Label.pp y_label ;
  let results = Hashtbl.find ols_results y_label in
  let series = Hashtbl.create (Hashtbl.length results) in
  Hashtbl.iter (fun serie ols ->
      let raws = Hashtbl.find raws serie in
      let raws = Dataset.of_measurement_raws ~x_label ~y_label raws in
      let ols = OLS.of_ols_result ~x_label ~y_label ols in
      Hashtbl.add series serie (raws, ols))
    results ;
  { x_label; y_label; series; }

type value = [ `Null | `Bool of bool | `String of string | `Float of float ]

let flat json : Jsonm.lexeme list =
  let rec arr acc k = function
    | [] -> k (List.rev (`Ae :: acc))
    | (#value as x) :: r -> arr (x :: acc) k r
    | `A l :: r -> arr [ `As ] (fun l -> arr (List.rev_append l acc) k r) l
    | `O l :: r -> obj [ `Os ] (fun l -> arr (List.rev_append l acc) k r) l

  and obj acc k = function
    | [] -> k (List.rev (`Oe :: acc))
    | (n, x) :: r -> base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x

  and base k = function
    | `A l -> arr [ `As ] k l
    | `O l -> obj [ `Os ] k l
    | #value as x -> k [ x ] in

  base (fun l -> l) json

type buffer = bytes * int * int
type transmit = buffer -> buffer

type 'a dst =
  | Manual : transmit -> buffer dst
  | Buffer : Buffer.t -> (Buffer.t -> unit) dst
  | Channel : out_channel -> (out_channel -> unit) dst

let manual transmit = Manual transmit

let buffer ~chunk =
  let buffer = Buffer.create chunk in
  Buffer buffer

let channel filename =
  let oc = open_out filename in
  Channel oc

type raws = (string, Measurement_raw.t array) Hashtbl.t
type ols_results = (Label.t, (string, Analyze.OLS.t) Hashtbl.t) Hashtbl.t

let emit
  : type a. dst:a dst -> a -> ?compare:(string -> string -> int) -> x_label:Label.t -> y_label:Label.t -> (ols_results * raws) -> unit
  = fun ~dst a ?(compare= String.compare) ~x_label ~y_label (ols_results, raw_results) ->
  let to_dst : type a. a dst -> Jsonm.dst = function
    | Manual _ -> `Manual
    | Buffer buffer -> (`Buffer buffer)
    | Channel oc -> (`Channel oc) in
  let encoder = Jsonm.encoder ~minify:true (to_dst dst) in
  let value = of_ols_results ~x_label ~y_label ols_results raw_results in
  let json = Json_encoding.construct (witness ~compare) value in
  let flat = flat json in

  let buf, off, len = match dst with
    | Manual _ ->
      let buf, off, len = a in ref buf, ref off, ref len
    | Buffer _ ->
      ref Bytes.empty, ref 0, ref 0
    | Channel _ ->
      ref Bytes.empty, ref 0, ref 0 in

  List.iter
    (fun lexeme -> match Jsonm.encode encoder (`Lexeme lexeme) with
       | `Ok -> ()
       | `Partial -> match dst with
         | Manual transmit ->
           let buf', off', len' = transmit (!buf, !off, !len - Jsonm.Manual.dst_rem encoder) in
           buf := buf' ; off := off' ; len := len' ;
           Jsonm.Manual.dst encoder buf' off' len'
         | Buffer _ -> ()
         | Channel _ -> ())
    flat ;

  let rec go : type a. a dst -> a -> unit = fun dst a -> match Jsonm.encode encoder `End, dst with
    | `Ok, Buffer buf -> a buf
    | `Ok, Channel oc -> a oc
    | `Ok, Manual _ -> ()
    | `Partial, Manual transmit ->
      let buf', off', len' = transmit (!buf, !off, !len - Jsonm.Manual.dst_rem encoder) in
      buf := buf' ; off := off' ; len := len' ;
      Jsonm.Manual.dst encoder buf' off' len' ;
      go dst a
    | `Partial, Buffer _ -> assert false
    | `Partial, Channel _ -> assert false in

  go dst a

