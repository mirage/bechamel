module Attr = struct
  type flag =
    | Disabled  (** off by default *)
    | Inherit  (** children inherit it *)
    | Exclude_user  (** don't count user *)
    | Exclude_kernel  (** don't count kernel *)
    | Exclude_hv  (** don't count hypervisor *)
    | Exclude_idle  (** don't count when idle *)
    | Enable_on_exec  (** next exec enables *)

  let flag_to_enum = function
    | Disabled -> 1
    | Inherit -> 2
    | Exclude_user -> 4
    | Exclude_kernel -> 8
    | Exclude_hv -> 16
    | Exclude_idle -> 32
    | Enable_on_exec -> 64

  module FSet = Set.Make (struct
    type t = flag

    let compare = compare
  end)

  module Kind = struct
    type t =
      (* Hardware *)
      | Cycles
      | Instructions
      | Cache_references
      | Cache_misses
      | Branch_instructions
      | Branch_misses
      | Bus_cycles
      | Stalled_cycles_frontend
      | Stalled_cycles_backend
      | Ref_cpu_cycles
      (* Software *)
      | Cpu_clock
      | Task_clock
      | Page_faults
      | Context_switches
      | Cpu_migrations
      | Page_faults_min
      | Page_faults_maj
      | Alignment_faults
      | Emulation_faults
      | Dummy

    let to_enum = function
      | Cycles -> 0
      | Instructions -> 1
      | Cache_references -> 2
      | Cache_misses -> 3
      | Branch_instructions -> 4
      | Branch_misses -> 5
      | Bus_cycles -> 6
      | Stalled_cycles_frontend -> 7
      | Stalled_cycles_backend -> 8
      | Ref_cpu_cycles -> 9
      | Cpu_clock -> 10
      | Task_clock -> 11
      | Page_faults -> 12
      | Context_switches -> 13
      | Cpu_migrations -> 14
      | Page_faults_min -> 15
      | Page_faults_maj -> 16
      | Alignment_faults -> 17
      | Emulation_faults -> 18
      | Dummy -> 19

    let string_of_t k =
      match k with
      | Cycles -> "Cycles"
      | Instructions -> "Instructions"
      | Cache_references -> "Cache_references"
      | Cache_misses -> "Cache_misses"
      | Branch_instructions -> "Branch_instructions"
      | Branch_misses -> "Branch_misses"
      | Bus_cycles -> "Bus_cycles"
      | Stalled_cycles_frontend -> "Stalled_cycles_frontend"
      | Stalled_cycles_backend -> "Stalled_cycles_backend"
      | Ref_cpu_cycles -> "Ref_cpu_cycles"
      (* Software *)
      | Cpu_clock -> "Cpu_clock"
      | Task_clock -> "Task_clock"
      | Page_faults -> "Page_faults"
      | Context_switches -> "Context_switches"
      | Cpu_migrations -> "Cpu_migrations"
      | Page_faults_min -> "Page_faults_min"
      | Page_faults_maj -> "Page_faults_maj"
      | Alignment_faults -> "Alignment_faults"
      | Emulation_faults -> "Emulation_faults"
      | Dummy -> "Dummy"

    let t_of_string s =
      match s with
      | "Cycles" -> Cycles
      | "Instructions" -> Instructions
      | "Cache_references" -> Cache_references
      | "Cache_misses" -> Cache_misses
      | "Branch_instructions" -> Branch_instructions
      | "Branch_misses" -> Branch_misses
      | "Bus_cycles" -> Bus_cycles
      | "Stalled_cycles_frontend" -> Stalled_cycles_frontend
      | "Stalled_cycles_backend" -> Stalled_cycles_backend
      | "Ref_cpu_cycles" -> Ref_cpu_cycles
      (* Software *)
      | "Cpu_clock" -> Cpu_clock
      | "Task_clock" -> Task_clock
      | "Page_faults" -> Page_faults
      | "Context_switches" -> Context_switches
      | "Cpu_migrations" -> Cpu_migrations
      | "Page_faults_min" -> Page_faults_min
      | "Page_faults_maj" -> Page_faults_maj
      | "Alignment_faults" -> Alignment_faults
      | "Emulation_faults" -> Emulation_faults
      | "Dummy" -> Dummy
      | _ -> invalid_arg "kind_of_string"

    let to_string t = string_of_t t |> String.uncapitalize_ascii
    let of_string s = String.capitalize_ascii s |> t_of_string
    let compare = compare
  end

  type t = { flags : FSet.t; kind : Kind.t }

  (** [make ?flags kind] is a perf event attribute of type [kind], with flags
      [flags]. *)
  let make ?(flags = []) kind = { flags = FSet.of_list flags; kind }

  let compare t1 t2 = compare t1.kind t2.kind
end

module KindMap = Map.Make (Attr.Kind)

type flag = Fd_cloexec | Fd_no_group | Fd_output | Pid_cgroup

let flag_to_enum = function
  | Fd_cloexec -> 1
  | Fd_no_group -> 2
  | Fd_output -> 4
  | Pid_cgroup -> 8

type t = { fd : Unix.file_descr; kind : Attr.Kind.t }

external perf_event_open :
     kind:int
  -> attr_flags:int
  -> pid:int
  -> cpu:int
  -> group_fd:int
  -> flags:int
  -> Unix.file_descr = "mperf_event_open_byte" "mperf_event_open_native"

external perf_event_ioc_enable : Unix.file_descr -> unit
  = "mperf_event_ioc_enable"

external perf_event_ioc_disable : Unix.file_descr -> unit
  = "mperf_event_ioc_disable"

external perf_event_ioc_reset : Unix.file_descr -> unit
  = "mperf_event_ioc_reset"

external enable_all : unit -> unit = "mperf_events_enable_all"
external disable_all : unit -> unit = "mperf_events_disable_all"

module FSet = Set.Make (struct
  type t = flag

  let compare = compare
end)

let make ?(pid = 0) ?(cpu = -1) ?group ?(flags = []) attr =
  let flags = FSet.of_list flags in
  let flags = FSet.fold (fun f acc -> acc + flag_to_enum f) flags 0 in

  let attr_flags =
    let open Attr in
    FSet.fold (fun f acc -> acc + Attr.(flag_to_enum f)) attr.flags 0
  in

  let group =
    match group with None -> -1 | Some { fd; _ } -> (Obj.magic fd : int)
  in
  let kind_enum = Attr.(Kind.(to_enum attr.kind)) in
  Attr.
    { fd =
        perf_event_open ~kind:kind_enum ~attr_flags ~pid ~cpu ~group_fd:group
          ~flags
    ; kind = attr.kind
    }

let kind c = c.kind

external get_int64 : bytes -> int -> int64 = "%caml_bytes_get64"
external swap64 : int64 -> int64 = "caml_int64_bswap"

let get_int64 buf off =
  if Sys.big_endian then swap64 (get_int64 buf off) else get_int64 buf off

let read c =
  let buf = Bytes.create 8 in
  let nb_read = Unix.read c.fd buf 0 8 in
  assert (nb_read = 8);
  get_int64 buf 0

let reset c = perf_event_ioc_reset c.fd
let enable c = perf_event_ioc_enable c.fd
let disable c = perf_event_ioc_disable c.fd
let close c = Unix.close c.fd

type execution =
  { process_status : Unix.process_status
  ; stdout : string
  ; stderr : string
  ; data : Int64.t KindMap.t
  }

let string_of_ic ic = really_input_string ic @@ in_channel_length ic

let string_of_file filename =
  let ic = open_in filename in
  try
    let res = string_of_ic ic in
    close_in ic;
    res
  with exn ->
    close_in ic;
    raise exn

let with_process_exn ?env ?timeout ?stdout ?stderr cmd attrs =
  let attrs =
    List.map
      Attr.(
        fun a ->
          { flags =
              List.fold_left
                (fun a f -> Attr.FSet.add f a)
                a.flags
                [ Disabled; Inherit; Enable_on_exec ]
          ; kind = a.kind
          })
      attrs
  in
  let counters = List.map make attrs in
  let tmp_stdout_name =
    match stdout with
    | None -> Filename.temp_file "ocaml-perf" "stdout"
    | Some s -> s
  in
  let tmp_stderr_name =
    match stderr with
    | None -> Filename.temp_file "ocaml-perf" "stderr"
    | Some s -> s
  in
  let tmp_stdout =
    Unix.(openfile tmp_stdout_name [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600)
  in
  let tmp_stderr =
    Unix.(openfile tmp_stderr_name [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600)
  in
  match Unix.fork () with
  | 0 ->
      (* child *)
      Unix.(
        handle_unix_error
          (fun () ->
            dup2 tmp_stdout stdout;
            close tmp_stdout;
            dup2 tmp_stderr stderr;
            close tmp_stderr;
            match env with
            | None -> execvp (List.hd cmd) (Array.of_list cmd)
            | Some env ->
                execvpe (List.hd cmd) (Array.of_list cmd) (Array.of_list env))
          ())
  | n ->
      (* parent *)
      (* Setup an alarm if timeout is specified. The alarm signal
         handles do nothing, but this will make waitpid fail with
         EINTR, unblocking the program. *)
      let (_ : int) = match timeout with None -> 0 | Some t -> Unix.alarm t in
      Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
      let _, process_status = Unix.waitpid [] n in
      List.iter disable counters;
      Unix.(
        close tmp_stdout;
        close tmp_stderr);
      let res =
        { process_status
        ; stdout = string_of_file tmp_stdout_name
        ; stderr = string_of_file tmp_stderr_name
        ; data =
            List.fold_left
              (fun a c -> KindMap.add c.kind (read c) a)
              KindMap.empty counters
        }
      in
      List.iter close counters;
      (* Remove stdout/stderr files iff they were left unspecified. *)
      (match stdout with None -> Unix.unlink tmp_stdout_name | _ -> ());
      (match stderr with None -> Unix.unlink tmp_stderr_name | _ -> ());
      res

let with_process ?env ?timeout ?stdout ?stderr cmd attrs =
  try `Ok (with_process_exn ?env ?timeout ?stdout ?stderr cmd attrs) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> `Timeout
  | exn -> `Exn exn
