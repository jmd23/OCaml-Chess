let convert_to_index (mv : char) =
  let code = int_of_char mv in
  if code > 56 then code - 97 else abs (code - 48 - 8)

let parse_input (inp : string) =
  List.of_seq (String.to_seq inp)
  |> List.filter (fun x -> x <> ' ')
  |> List.map convert_to_index

(* Parse the string response and make sure it's a legal move. if yes, then find
   the pieces and move them else print error to user *)
let handle_response res = print_endline ""

let validate_response res =
  let re = Str.regexp "^\\([a-h][1-8] \\)*[a-h][1-8]$" in
  Str.string_match re res 0

let rec prompt_user () =
  print_endline "Enter next move";
  match read_line () with
  | exception End_of_file -> print_endline ""
  | response ->
      if validate_response response then handle_response response
      else print_endline "Invalid input";
      prompt_user ()
