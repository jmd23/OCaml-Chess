open Game
open Game.Board

let convert_to_index (mv : char) =
  let code = int_of_char mv in
  if code > 56 then code - 97 else abs (code - 48 - 8)

let parse_input (inp : string) =
  List.of_seq (String.to_seq inp)
  |> List.filter (fun x -> x <> ' ')
  |> List.map convert_to_index

(* Parse the string response and make sure it's a legal move. if yes, then find
   the pieces and move them else print error to user *)
let change_board_from_indices board lst =
  let piece = Board.get_square board (List.nth lst 1) (List.nth lst 0) in
  let board = Board.set_square (List.nth lst 3) (List.nth lst 2) board piece in
  let board = Board.set_square (List.nth lst 1) (List.nth lst 0) board Empty in
  board

(* *)

let rec prompt_user board () =
  let handle_response res board =
    let handle_command cmd board =
      match cmd with
      | Commands.Quit ->
          print_endline "Quiting now";
          exit 0
      | Commands.Move m ->
          let new_board = change_board_from_indices board m in
          prompt_user new_board ()
    in
    try
      let cmd = Commands.parse res in
      handle_command cmd board
    with
    | Commands.Malformed ->
        print_endline "Malformed";
        prompt_user board ()
    | Commands.Empty ->
        print_endline "Empty";
        prompt_user board ()
  in

  board |> board_to_string |> print_endline;
  print_endline "Enter next move";
  match read_line () with
  | exception End_of_file -> print_endline ""
  | response -> handle_response response board

let main () =
  print_endline "\n\nWelcome to Our Game of Chess!";
  prompt_user starting_board ()

let () = main ()
