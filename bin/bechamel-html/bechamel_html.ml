(* [string_find str target] returns [None] if the string [target] cannot be
    find in [str] and [Some i] if the first occurence of [target] begins at
    position [i]. *)
let string_find str target =
  let exception Not_equal in
  let substring_equal a a_i b =
    String.iteri (fun i c -> if c <> a.[a_i + i] then raise Not_equal) b
  in
  let rec loop i =
    match String.index_from_opt str i target.[0] with
    | None -> None
    | Some i when String.length str - i < String.length target -> None
    | Some i -> (
        match substring_equal str i target with
        | exception Not_equal -> loop (i + 1)
        | () -> Some i)
  in
  if String.length target = 0 then None else loop 0

let stdin_to_stdout () =
  try
    while true do
      print_endline (read_line ())
    done
  with End_of_file -> ()

(* [cut ~error_msg str sep] cuts the string [str] into the two parts before and
    after [sep]. If [sep] not appears in [str] an error is raised with the text
    [error_msg]. *)
let cut ~error_msg str separator =
  match string_find str separator with
  | None -> failwith error_msg
  | Some i ->
      let ends = i + String.length separator in
      (String.sub str 0 i, String.sub str ends (String.length str - ends))

(* [cut_html html] cuts in 3 parts: the part before "/*style*/", the part
    between "/*style*/" and "//js_script" and the rest of the text *)
let cut_html html =
  let sep1 = "/*style*/" in
  let sep2 = "//js_script" in
  let part1, rest =
    cut ~error_msg:("Separator " ^ sep1 ^ " not found in html file.") html sep1
  in
  let part2, part3 =
    cut ~error_msg:("Separator " ^ sep2 ^ " not found in html file.") rest sep2
  in
  (part1, part2, part3)

(* Print the java script file, including the json data passed as stdin. *)
let print_js_script () =
  (* Read first line so we don't show [head] if user forgot to redirect its json
     file and if it's already end of input, it will be an explicit uncaught
     exception instead of generating an invalid HTML file *)
  let first_line = read_line () in
  let js = Js_file.data in
  let separator = "//BECHAMEL_CONTENTS//" in
  let head, tail =
    cut ~error_msg:"Separator not found in js file" js separator
  in
  print_string head;
  print_string "contents = ";
  print_endline first_line;
  stdin_to_stdout ();
  print_string tail

let () =
  let html = Html_file.data in
  let css = Css_file.data in
  let html1, html2, html3 = cut_html html in
  print_string html1;
  print_string css;
  print_string html2;
  print_js_script ();
  print_string html3
