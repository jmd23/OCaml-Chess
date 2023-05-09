open Game

let rec promt_user st =
  let hr response st =
    let handle_command cmd st =
      match cmd with
      | Commands.Quit ->
          print_endline "\n Okay. Quiting now";
          Unix.sleep 2;
          exit 0
      | Commands.Move m -> (
          let move_result = State.make_move st m in
          match move_result with
          | State.Legal s -> promt_user s
          | State.Illegal_Move ->
              print_endline
                "\nYou attempted to make an illegal move.\nTry again";
              Unix.sleep 2;
              promt_user st
          | State.Illegal_Piece ->
              print_endline "\nYou attempted to move a wrong piece.\nTry again";
              Unix.sleep 2;
              promt_user st)
      | Commands.Undo -> (
          let undo_result = State.undo st in
          match undo_result with
          | State.Undo_Fail ->
              print_endline "Sorry can't undo any further.";
              promt_user st
          | State.Undone s -> promt_user s)
      | Commands.Redo -> (
          let redo_result = State.redo st in
          match redo_result with
          | State.Redo_Fail ->
              print_endline "Cannot Redo any further";
              promt_user st
          | State.Redone s -> promt_user s)
    in
    try
      let cmd = Commands.parse response in
      handle_command cmd st
    with
    | Commands.Malformed ->
        print_endline "Malformed";
        promt_user st
    | Commands.Empty ->
        print_endline "Empty";
        promt_user st
  in
  Board.print_board (State.get_current_board st);
  let in_check =
    State.in_check st (State.get_current_player st) (State.get_current_board st)
  in
  let legals_exist = State.has_legal_moves st in
  if not legals_exist then
    if in_check then
      print_endline
        ("Checkmate! "
        ^ Player.string_of_player
            (Player.switch_player (State.get_current_player st))
        ^ " has won!")
    else print_endline "Stalemate! It's a draw"
  else
    let player = State.get_current_player st in
    print_endline
      ("\n" ^ Player.string_of_player player ^ ", it is your turn. Enter a move");
    match read_line () with
    | exception End_of_file -> print_endline "Prompt end of file"
    | response -> hr response st

let start_game () =
  let handle_response (res : string) st =
    let handle_command cmd st =
      match cmd with
      | Commands.Quit ->
          print_endline "\n Okay. Quiting now";
          Unix.sleep 2;
          exit 0
      | Commands.Move m -> (
          let move_result = State.make_move st m in
          match move_result with
          | State.Legal s ->
              print_endline "Moving piece";
              Unix.sleep 1;
              promt_user s
          | State.Illegal_Move ->
              print_endline
                "\nYou attempted to make an illegal move.\nTry again";
              Unix.sleep 2;
              promt_user st
          | State.Illegal_Piece ->
              print_endline "\nYou attempted to move a wrong piece.\nTry again";
              Unix.sleep 2;
              promt_user st)
      | Commands.Undo -> (
          let undo_result = State.undo st in
          match undo_result with
          | State.Undo_Fail ->
              print_endline "Sorry can't undo any further.";
              Unix.sleep 2;
              promt_user st
          | State.Undone s -> promt_user s)
      | Commands.Redo -> (
          let redo_result = State.redo st in
          match redo_result with
          | State.Redo_Fail ->
              print_endline "Cannot Redo any further";
              Unix.sleep 2;
              promt_user st
          | State.Redone s -> promt_user s)
    in
    try
      let cmd = Commands.parse res in
      handle_command cmd st
    with
    | Commands.Malformed ->
        print_endline "\nSorry your input was malformed.\nPlease try again\n";
        Unix.sleep 2;
        promt_user st
    | Commands.Empty ->
        print_endline "Sorry your input was empty.\nPlease try again\n";
        Unix.sleep 2;
        promt_user st
  in

  let game_state = State.init_state () in
  Board.print_board (State.get_current_board game_state);
  print_endline
    "\n White, you start the game. Enter a move such as 'move e2 e4'";
  match read_line () with
  | exception End_of_file -> print_endline "End of file"
  | response -> handle_response response game_state

let main () =
  print_endline "\n\nWelcome to Our Game of Chess!\nWe hope you like it :)";
  start_game ()

let () = main ()
