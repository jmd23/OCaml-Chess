open Game.Board
open Game.Player

let convert_to_index (mv : char) =
  let code = int_of_char mv in
  if code > 56 then code - 97 else abs (code - 48 - 8)

let parse_input (inp : string) =
  List.of_seq (String.to_seq inp)
  |> List.filter (fun x -> x <> ' ')
  |> List.map convert_to_index

(* Parse the string response and make sure it's a legal move. if yes, then find
   the pieces and move them else print error to user *)

let validate_response res =
  if res = "quit" then exit 0
  else
    let re = Str.regexp "^\\([a-h][1-8] \\)*[a-h][1-8]$" in
    Str.string_match re res 0

(* *)
let change_board_from_indeces board lst =
  let piece = get_square board (List.nth lst 1) (List.nth lst 0) in
  let board = set_square (List.nth lst 3) (List.nth lst 2) board piece in
  let board = set_square (List.nth lst 1) (List.nth lst 0) board Empty in
  board

let rec prompt_user board curr () =
  board |> board_to_string |> print_endline;
  print_endline (string_of_player curr ^ ", its your turn. Enter a move:");
  match read_line () with
  | exception End_of_file -> print_endline ""
  | response ->
      if validate_response response then (
        let lst = parse_input response in
        print_endline (string_of_int (List.nth lst 0));
        let new_board = change_board_from_indeces board lst in
        prompt_user new_board (switch_player curr) ())
      else print_endline "Invalid input";
      prompt_user board curr ()

let main () =
  print_endline "\n\nWelcome to Our Game of Chess!";
  print_endline "\nAt any point, enter a move like 'e2 e4', or 'quit' to exit";
  prompt_user starting_board White ()

let () = main ()
