open Game

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
  let board =
    Board.set_square (List.nth lst 1) (List.nth lst 0) board Board.Empty
  in
  board

(* *)

(* let rec prompt_user board () = let handle_response res board = let
   handle_command cmd board = match cmd with | Commands.Quit -> print_endline
   "Quiting now"; exit 0 | Commands.Move m -> let new_board =
   change_board_from_indices board m in prompt_user new_board () in try let cmd
   = Commands.parse res in handle_command cmd board with | Commands.Malformed ->
   print_endline "Malformed"; prompt_user board () | Commands.Empty ->
   print_endline "Empty"; prompt_user board () in

   (* board |> board_to_string |> print_endline; *) Board.print_board board;
   print_endline "Enter next move"; match read_line () with | exception
   End_of_file -> print_endline "" | response -> handle_response response
   board *)

let rec test_prompt st =
  let hr response st =
    let handle_command cmd st =
      match cmd with
      | Commands.Quit ->
          print_endline "\n Quiting";
          exit 0
      | Commands.Move m -> (
          let move_result = State.make_move st m in
          match move_result with
          | State.Legal s -> test_prompt s
          | State.Illegal_Move ->
              print_endline "\nIllegal move";
              test_prompt st
          | State.Illegal_Piece ->
              print_endline "\n Illegal piece";
              test_prompt st)
      | Commands.Undo -> (
          let undo_result = State.undo st in
          match undo_result with
          | State.Undo_Fail ->
              print_endline "Sorry can't undo any further.";
              test_prompt st
          | State.Undone s -> test_prompt s)
      | Commands.Redo -> (
          let redo_result = State.redo st in
          match redo_result with
          | State.Redo_Fail ->
              print_endline "Cannot Redo any further";
              test_prompt st
          | State.Redone s -> test_prompt s)
    in
    try
      let cmd = Commands.parse response in
      handle_command cmd st
    with
    | Commands.Malformed ->
        print_endline "Malformed";
        test_prompt st
    | Commands.Empty ->
        print_endline "Empty";
        test_prompt st
  in

  Board.print_board (State.get_current_board st);
  print_endline "\n Next move";
  match read_line () with
  | exception End_of_file -> print_endline "Prompt end of file"
  | response -> hr response st

let testing () =
  let handle_response (res : string) st =
    let handle_command cmd st =
      match cmd with
      | Commands.Quit ->
          print_endline "\n Quiting";
          exit 0
      | Commands.Move m -> (
          let move_result = State.make_move st m in
          match move_result with
          | State.Legal s -> test_prompt s
          | State.Illegal_Move ->
              print_endline "\nIllegal move";
              test_prompt st
          | State.Illegal_Piece ->
              print_endline "\n Illegal piece";
              test_prompt st)
      | Commands.Undo -> (
          let undo_result = State.undo st in
          match undo_result with
          | State.Undo_Fail ->
              print_endline "Sorry can't undo any further.";
              test_prompt st
          | State.Undone s -> test_prompt s)
      | Commands.Redo -> (
          let redo_result = State.redo st in
          match redo_result with
          | State.Redo_Fail ->
              print_endline "Cannot Redo any further";
              test_prompt st
          | State.Redone s -> test_prompt s)
    in
    try
      let cmd = Commands.parse res in
      handle_command cmd st
    with
    | Commands.Malformed ->
        print_endline "Malformed";
        test_prompt st
    | Commands.Empty ->
        print_endline "Empty";
        test_prompt st
  in

  let game_state = State.init_state () in
  Board.print_board (State.get_current_board game_state);
  print_endline "\n Beginning move";
  match read_line () with
  | exception End_of_file -> print_endline "End of file"
  | response -> handle_response response game_state

let main () =
  print_endline "\n\nWelcome to Our Game of Chess!";
  (* prompt_user Board.starting_board () *)
  testing ()

let () = main ()
