let string_find s target =
  let exception Not_equal in
  let substring_equal a a_i b =
    String.iteri (fun i c -> if c <> a.[a_i + i] then raise Not_equal) b
  in
  let rec loop i =
    match String.index_from_opt s i target.[0] with
    | None -> None
    | Some i when String.length s - i < String.length target -> None
    | Some i -> (
        match substring_equal s i target with
        | exception Not_equal -> loop (i + 1)
        | () -> Some i )
  in
  if String.length target = 0 then None else loop 0

let head, tail =
  let data = Html_file.data in
  let separator = "//BECHAMEL_CONTENTS//" in
  match string_find data separator with
  | None -> failwith "Separator not found"
  | Some i ->
      let ends = i + String.length separator in
      (String.sub data 0 i, String.sub data ends (String.length data - ends))

let stdin_to_stdout () =
  try
    while true do
      print_endline (read_line ())
    done
  with End_of_file -> ()

let () =
  (* Read first line so we don't show [head] if user forgot to redirect its json
     file and if it's already end of input, it will be an explicit uncaught
     exception instead of generating an invalid HTML file *)
  let first_line = read_line () in
  print_string head;
  print_string "contents = ";
  print_endline first_line;
  stdin_to_stdout ();
  print_string ";";
  print_string tail
