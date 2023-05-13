open OUnit2
open Game

(* -------------------Board Tests---------------------- *)
let piece_to_string_test (name : string) (p : Board.piece)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Board.piece_to_string p)

let print_square_test (name : string) (sq : Board.square)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Board.print_square sq)

let row_to_string_test (name : string) j (sq_lst : Board.square list)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Board.row_to_string j sq_lst)

let set_row_test (name : string) j row value (expected_output : 'a list) : test
    =
  name >:: fun _ -> assert_equal expected_output (Board.set_row j row value)

let get_square_test (name : string) i j (board : Board.board) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (Board.get_square board i j)

let validate_owner_test (name : string) (plr : Player.player) (p : Board.piece)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (Board.validate_owner plr p)

let board_equal_test (name : string) (brd1 : Board.board) (brd2 : Board.board)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (Board.board_equal brd1 brd2)

(* -------------------Movement Tests---------------------- *)
let get_move move =
  match move with
  | Movement.Normal brd -> brd
  | Captured brd_pair -> fst brd_pair

let move_test (name : string) (board : Board.board) (plr : Player.player)
    (lst : int list) (expected_output : Board.board) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_move (Movement.move board plr lst))

(* -------------------Player Tests---------------------- *)
let switch_player_test (name : string) (player : Player.player)
    (expected_output : Player.player) : test =
  name >:: fun _ -> assert_equal expected_output (Player.switch_player player)

let string_of_player_test (name : string) (player : Player.player)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.string_of_player player)

(* -------------------State Tests---------------------- *)

exception Illegal_M
exception Illegal_P

let get_state res =
  match res with
  | State.Legal t -> t
  | State.Illegal_Move -> raise Illegal_M
  | State.Illegal_Piece -> raise Illegal_P

let compare_board (name : string) lst (st : State.state)
    (expected_output : Board.board) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.get_current_board (get_state (State.make_move st lst)))

let make_move_illegalmove_test (name : string) lst (st : State.state) : test =
  name >:: fun _ -> assert_equal State.Illegal_Move (State.make_move st lst)

let make_move_illegalpiece_test (name : string) lst (st : State.state) : test =
  name >:: fun _ -> assert_equal State.Illegal_Piece (State.make_move st lst)

let move_one_legal_compare (name : string) (lst : int list) (st : State.state) :
    test =
  name >:: fun _ ->
  assert_equal
    (Board.get_square Board.starting_board (List.nth lst 1) (List.nth lst 0))
    (Board.get_square
       (State.get_current_board (get_state (State.make_move st lst)))
       (List.nth lst 3) (List.nth lst 2))

let move_two_legal_compare (name : string) (lst1 : int list) (lst2 : int list)
    (st : State.state) : test =
  name >:: fun _ ->
  assert_equal
    (Board.get_square
       (State.get_current_board (get_state (State.make_move st lst1)))
       (List.nth lst2 1) (List.nth lst2 0))
    (Board.get_square
       (State.get_current_board
          (get_state
             (State.make_move (get_state (State.make_move st lst1)) lst2)))
       (List.nth lst2 3) (List.nth lst2 2))

(* test capture *)
let white_capture_test (name : string) (st : State.state)
    (expected_output : Board.piece list) : test =
  name >:: fun _ -> assert_equal expected_output (State.get_white_captured st)

let black_capture_test (name : string) (st : State.state)
    (expected_output : Board.piece list) : test =
  name >:: fun _ -> assert_equal expected_output (State.get_black_captured st)

(* undo/redo *)
let undo_test (name : string) (st : State.state)
    (expected_output : State.undo_result) : test =
  name >:: fun _ -> assert_equal expected_output (State.undo st)

let redo_test (name : string) (st : State.state)
    (expected_output : State.redo_result) : test =
  name >:: fun _ -> assert_equal expected_output (State.redo st)

(* -------------------Pieces Tests---------------------- *)

(* Knight *)
let knight_valid_test (name : string) (board : Board.board)
    (plr : Player.player) (lst : int list) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Knight.validate_knight_move board plr lst)

(* Pawn *)
let pawn_valid_test (name : string) (board : Board.board) (plr : Player.player)
    (lst : int list) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Pawn.validate_pawn_move board plr lst)

(* Rook *)
let rook_valid_test (name : string) (board : Board.board) (plr : Player.player)
    (lst : int list) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Rook.validate_rook_move board plr lst)

(* bishop *)
let bishop_valid_test (name : string) (board : Board.board)
    (plr : Player.player) (lst : int list) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Bishop.validate_bishop_move board plr lst)

(* king *)
let king_valid_test (name : string) (board : Board.board) (plr : Player.player)
    (lst : int list) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (King.validate_king_move board plr lst)

(* -------------------Commands Tests---------------------- *)
let parse_input (c : Commands.command) : string =
  match c with
  | Move ph -> String.concat "; " (List.map string_of_int ph)
  | Redo -> "redo"
  | Undo -> "undo"
  | Quit -> "quit"

let parse_test (name : string) (s : string) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_input (Commands.parse s))

(* -----------------------------Specific Tests------------------------------- *)
(* boards *)
let start_board = Board.starting_board

(* new board legal move 1 *)
let rank_1_1 =
  [
    Board.Piece (Rook White);
    Piece (Knight White);
    Piece (Bishop White);
    Piece (Queen White);
    Piece (King White);
    Piece (Bishop White);
    Piece (Knight White);
    Piece (Rook White);
  ]

let rank_2_1 =
  [
    Board.Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
    Board.Empty;
    Piece (Pawn White);
    Piece (Pawn White);
    Piece (Pawn White);
  ]

let rank_4_1 =
  [
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Piece (Pawn White);
    Board.Empty;
    Board.Empty;
    Board.Empty;
  ]

let rank_7_1 =
  [
    Board.Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
  ]

let rank_8_1 =
  [
    Board.Piece (Rook Black);
    Piece (Knight Black);
    Piece (Bishop Black);
    Piece (Queen Black);
    Piece (King Black);
    Piece (Bishop Black);
    Piece (Knight Black);
    Piece (Rook Black);
  ]

let empty_ranks =
  [ Board.Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]

let legal_board_1 =
  [
    rank_8_1;
    rank_7_1;
    empty_ranks;
    empty_ranks;
    rank_4_1;
    empty_ranks;
    rank_2_1;
    rank_1_1;
  ]

(* states *)
let start_state = State.init_state ()
let state_1 = get_state (State.make_move start_state [ 4; 6; 4; 5 ])

let get_undone un =
  match un with
  | State.Undo_Fail -> failwith "can't undo"
  | Undone st -> st

let state_tests =
  [
    compare_board "compare board e2->e4" [ 4; 6; 4; 4 ] start_state
      legal_board_1;
    make_move_illegalmove_test "illegal move from e1 to h6" [ 4; 7; 5; 2 ]
      start_state;
    make_move_illegalpiece_test "illegal piece from e8 to e3" [ 4; 0; 4; 5 ]
      start_state;
    move_one_legal_compare "make one legal move: e2 to e4" [ 4; 6; 4; 4 ]
      start_state;
    move_one_legal_compare "make a second legal move from e2->e4 then e7->e5"
      [ 4; 1; 4; 3 ] state_1;
    move_two_legal_compare "make two legal moves: e2->e4 then e7->e5"
      [ 4; 6; 4; 4 ] [ 4; 1; 4; 3 ] start_state;
    white_capture_test "white capture none" start_state [];
    black_capture_test "black capture none" start_state [];
    undo_test "undo start board" start_state Undo_Fail;
    redo_test "redo start board" start_state Redo_Fail;
  ]

let parse_tests =
  [
    parse_test "test move" "move e2 e4" "4; 6; 4; 4";
    parse_test "test undo" "undo" "undo";
    parse_test "test redo" "redo" "redo";
    parse_test "test quit" "quit" "quit";
  ]

let rank_7_2 =
  [
    Board.Empty;
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
    Piece (Pawn Black);
  ]

let rank_6_2 =
  [
    Board.Piece (Pawn Black);
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
  ]

let rank_3_2 =
  [
    Board.Piece (Pawn White);
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
  ]

let rank_2_2 =
  [
    Board.Empty;
    Board.Piece (Pawn White);
    Board.Piece (Pawn White);
    Board.Piece (Pawn White);
    Board.Piece (Pawn White);
    Board.Piece (Pawn White);
    Board.Piece (Pawn White);
    Board.Piece (Pawn White);
  ]

let legal_board_2 =
  [
    rank_8_1;
    rank_7_2;
    rank_6_2;
    empty_ranks;
    empty_ranks;
    rank_3_2;
    rank_2_2;
    rank_1_1;
  ]

let legal_board_3 =
  [
    rank_8_1;
    rank_7_2;
    rank_6_2;
    empty_ranks;
    rank_4_1;
    empty_ranks;
    rank_2_1;
    rank_1_1;
  ]

let legal_board_4 =
  [
    rank_8_1;
    rank_7_1;
    empty_ranks;
    empty_ranks;
    empty_ranks;
    rank_3_2;
    rank_2_2;
    rank_1_1;
  ]

let rank_3_knight =
  [
    Board.Empty;
    Board.Empty;
    Piece (Knight White);
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
    Board.Empty;
  ]

let rank_1_no_knight =
  [
    Board.Piece (Rook White);
    Empty;
    Piece (Bishop White);
    Piece (Queen White);
    Piece (King White);
    Piece (Bishop White);
    Piece (Knight White);
    Piece (Rook White);
  ]

let legal_board_knight =
  [
    rank_8_1;
    rank_7_2;
    rank_6_2;
    empty_ranks;
    empty_ranks;
    rank_3_knight;
    rank_2_1;
    rank_1_no_knight;
  ]

let legal_board_rook =
  [
    rank_8_1;
    rank_7_2;
    rank_6_2;
    empty_ranks;
    rank_3_2;
    empty_ranks;
    rank_2_2;
    rank_1_1;
  ]

let board_tests =
  [
    piece_to_string_test "test white pawn" (Pawn White) "♙";
    piece_to_string_test "test black pawn" (Pawn Black) "♙";
    piece_to_string_test "test white bishop" (Bishop White) "♗";
    piece_to_string_test "test black bishop" (Bishop Black) "♗";
    piece_to_string_test "test white knight" (Knight White) "♘";
    piece_to_string_test "test black knight" (Knight Black) "♘";
    piece_to_string_test "test white Rook" (Rook White) "♖";
    piece_to_string_test "test black Rook" (Rook Black) "♖";
    piece_to_string_test "test white Queen" (Queen White) "♕";
    piece_to_string_test "test black Queen" (Queen Black) "♕";
    piece_to_string_test "test white King" (King White) "♔";
    piece_to_string_test "test black King" (King Black) "♔";
    get_square_test "get a1" 7 0 start_board (Piece (Rook White));
    get_square_test "get b1" 7 1 start_board (Piece (Knight White));
    get_square_test "get c1" 7 2 start_board (Piece (Bishop White));
    get_square_test "get d1" 7 3 start_board (Piece (Queen White));
    get_square_test "get e1" 7 4 start_board (Piece (King White));
    get_square_test "get h8" 0 7 start_board (Piece (Rook Black));
    get_square_test "get g8" 0 6 start_board (Piece (Knight Black));
    get_square_test "get f8" 0 5 start_board (Piece (Bishop Black));
    get_square_test "get e8" 0 4 start_board (Piece (King Black));
    get_square_test "get d8" 0 3 start_board (Piece (Queen Black));
    validate_owner_test "owner should be white" Player.White (Rook White) true;
    validate_owner_test "owner should be white" Player.White (Pawn White) true;
    validate_owner_test "owner should be black" Player.Black (Pawn Black) true;
    validate_owner_test "owner should be black" Player.Black (Queen Black) true;
    board_equal_test "two boards are not equal" legal_board_1 start_board false;
    board_equal_test "compare the same board" legal_board_knight
      legal_board_knight true;
  ]

let movement_tests =
  [
    move_test "move e2 to e4" start_board Player.White [ 4; 6; 4; 4 ]
      legal_board_1;
    move_test "move a7 to a6" legal_board_1 Player.Black [ 0; 1; 0; 2 ]
      legal_board_3;
    move_test "move a7 to a6" legal_board_4 Player.Black [ 0; 1; 0; 2 ]
      legal_board_2;
  ]

let player_tests =
  [
    switch_player_test "after white should be black" Player.White Player.Black;
    switch_player_test "after black should be white" Player.Black Player.White;
    string_of_player_test "test white" Player.White "White";
    string_of_player_test "test black" Player.Black "Black";
  ]

let piece_tests =
  [
    knight_valid_test "move white knight from b1 to c3" start_board Player.White
      [ 1; 7; 2; 5 ] true;
    knight_valid_test "move white knight from b1 to a3" start_board Player.White
      [ 1; 7; 0; 5 ] true;
    knight_valid_test "move white knight from c3 to b1" legal_board_knight
      Player.White [ 2; 5; 1; 7 ] true;
    knight_valid_test "move white knight from c3 to a4" legal_board_knight
      Player.White [ 2; 5; 0; 4 ] true;
    knight_valid_test "move white knight from c3 to b5" legal_board_knight
      Player.White [ 2; 5; 1; 3 ] true;
    knight_valid_test "move white knight from c3 to d5" legal_board_knight
      Player.White [ 2; 5; 3; 3 ] true;
    knight_valid_test "move white knight from c3 to e4" legal_board_knight
      Player.White [ 2; 5; 4; 4 ] true;
    knight_valid_test "invalid knight move" start_board Player.White
      [ 1; 7; 4; 4 ] false;
    knight_valid_test "invalid knight move" start_board Player.White
      [ 1; 7; 5; 5 ] false;
    pawn_valid_test "move pawn from h2 to h3" start_board Player.White
      [ 5; 6; 5; 5 ] true;
    pawn_valid_test "move pawn from h2 to h4" start_board Player.White
      [ 5; 6; 5; 4 ] true;
    pawn_valid_test "move pawn from e2 to e3" start_board Player.White
      [ 4; 6; 4; 5 ] true;
    pawn_valid_test "move pawn from a2 to a3" start_board Player.White
      [ 0; 6; 0; 5 ] true;
    pawn_valid_test "move pawn from a4 to a5" legal_board_rook Player.White
      [ 0; 4; 0; 3 ] true;
    pawn_valid_test "invalid pawn move" start_board Player.White [ 5; 6; 5; 3 ]
      false;
    pawn_valid_test "invalid pawn move" legal_board_1 Player.Black
      [ 5; 0; 5; 3 ] false;
    pawn_valid_test "invalid pawn move" start_board Player.White [ 5; 6; 5; 2 ]
      false;
    pawn_valid_test "invalid pawn move" start_board Player.White [ 5; 6; 4; 5 ]
      false;
    rook_valid_test "move rook from a1 to a2" legal_board_2 Player.White
      [ 0; 7; 0; 6 ] true;
    rook_valid_test "move rook from a1 to a3" legal_board_rook Player.White
      [ 0; 7; 0; 5 ] true;
    rook_valid_test "invalid rook move" start_board Player.White [ 0; 7; 0; 5 ]
      false;
    bishop_valid_test "move bishop from f1 to d3" legal_board_3 Player.White
      [ 5; 7; 3; 5 ] true;
    bishop_valid_test "move bishop from f1 to a6" legal_board_3 Player.White
      [ 5; 7; 0; 2 ] true;
    bishop_valid_test "move bishop from f1 to a6" legal_board_3 Player.White
      [ 5; 7; 1; 3 ] true;
    bishop_valid_test "move bishop from f1 to a6" legal_board_3 Player.White
      [ 5; 7; 2; 4 ] true;
    bishop_valid_test "invalid bishop move" start_board Player.White
      [ 5; 7; 0; 5 ] false;
    king_valid_test "move king from e1 to e2" legal_board_3 Player.White
      [ 4; 7; 4; 6 ] true;
    king_valid_test "invalid king move" start_board Player.White [ 4; 7; 0; 5 ]
      false;
    king_valid_test "invalid king move" start_board Player.White [ 4; 7; 3; 5 ]
      false;
  ]

let suite =
  "test suite for project"
  >::: List.flatten
         [
           state_tests;
           parse_tests;
           board_tests;
           piece_tests;
           movement_tests;
           player_tests;
         ]

let _ = run_test_tt_main suite
