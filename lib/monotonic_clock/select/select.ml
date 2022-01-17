let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let load_file filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  let () = really_input ic rs 0 ln in
  Bytes.unsafe_to_string rs

let sexp_linux = "(-lrt)"
let sexp_empty = "()"

let () =
  let system, output =
    match Sys.argv with
    | [| _; "--system"; system; "-o"; output |] ->
        let system =
          match system with
          | "linux" | "linux_eabihf" | "linux_elf" | "elf" -> `Linux
          | "freebsd" -> `FreeBSD
          | "windows" | "mingw64" | "cygwin" -> `Windows
          | "macosx" -> `MacOSX
          | v -> invalid_arg "Invalid argument of system option: %s" v
        in
        (system, output)
    | _ ->
        invalid_arg "Expected `%s --system <system> -o <output>' got `%s'"
          Sys.argv.(0)
          (String.concat " " (Array.to_list Sys.argv))
  in
  let oc_ml, oc_c, oc_sexp =
    ( open_out (output ^ ".ml")
    , open_out (output ^ "_stubs.c")
    , open_out (output ^ ".sexp") )
  in
  let ml, c =
    match system with
    | `Linux | `FreeBSD ->
        (load_file "clock_linux.ml", load_file "clock_linux_stubs.c")
    | `Windows ->
        (load_file "clock_windows.ml", load_file "clock_windows_stubs.c")
    | `MacOSX -> (load_file "clock_mach.ml", load_file "clock_mach_stubs.c")
  in
  let sexp = if system = `Linux then sexp_linux else sexp_empty in
  Printf.fprintf oc_ml "%s%!" ml;
  Printf.fprintf oc_c "%s%!" c;
  Printf.fprintf oc_sexp "%s%!" sexp;
  close_out oc_ml;
  close_out oc_c;
  close_out oc_sexp
