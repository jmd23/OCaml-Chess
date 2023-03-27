open Game

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
  let player = State.get_current_player st in
  print_endline
    ("\n" ^ Player.string_of_player player ^ ", it is your turn. Enter a move");
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
  print_endline
    "\n White, you start the game. Enter a move such as 'move e2 e4'";
  match read_line () with
  | exception End_of_file -> print_endline "End of file"
  | response -> handle_response response game_state

let main () =
  print_endline "\n\nWelcome to Our Game of Chess!";
  (* prompt_user Board.starting_board () *)
  testing ()

let () = main ()
