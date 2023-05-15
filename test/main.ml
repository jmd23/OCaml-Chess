(* testing plan *)
(* - Automatically tested: we will automatically test (through OUnit) whether
   pieces only move in valid ways, and if they are able to capture any piece as
   a result of the move. This is primarily done through black-box testing:
   examing the specified movements of each piece, setting up boards, and entring
   in both squares that align and do not align with these movements. We also
   compare the board before and after the move to ensure any captures occur
   correctly. We will also test to ensure the board is initialized correctly and
   if each piece is in the right position, again using black-box testing. This
   is part of the tests we craft to the state module. In addition, we write
   OUnit tests for the other major functions in that module, including
   [has_legal_moves], and [in_check]. We also add tests for parsing the inputs
   (the commands module), ensuring that move commands, quit, undo/redo, etc...
   are being interpreted correctly. We have some tests for the board and player
   modules, ensuring that our basic functionality (storing the board, switching
   the player, etc...) is met. Lastly, the test suite will check if the game is
   started and ended in a correct way and if turn-taking is happening. In
   summary, we use automatic testing for the basic functionality of the system
   and simple move sequences, as these can be encoded relatively quickly and
   give us some assurance of correctness. *)
(*- Manually tested: the terminal interface is manually tested to make sure that
  the game is easy to play with while engaging for players in terminal. We also
  made sure that the messages printed out from different entry inputs are
  understandable and engaging for players. In this manually testing, we test out
  longer move sequences (castling, sequences of captures, etc...) to determine
  if all the rules of the game are adhered to and that the behavior of the
  system is as expected. We aksi enter bad inputs into the game, to make sure
  the system does not crash. *)
(*- Taken together, we use these to establish the correctness of our system. If
  the basic piece functionality, and simplie move sequences work (automatic
  testing), and longer games and move sequences also work (manual testing), we
  can be fairly confident our chess system is robust and will behaive as
  expected, no matter the input. There may still be bugs, but they do not
  strongly affect gameplay. *)

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
let undo_test (name : string) (st : State.state) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (State.undo st)

let redo_test (name : string) (st : State.state)
    (expected_output : State.redo_result) : test =
  name >:: fun _ -> assert_equal expected_output (State.redo st)

let has_legal_moves_test (name : string) (st : State.state)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (State.has_legal_moves st)

let is_in_check_test (name : string) (st : State.state) (expected_output : bool)
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (State.in_check st
       (State.get_current_player st)
       (State.get_current_board st))

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

let rank_8_king =
  [
    Board.Empty;
    Empty;
    Empty;
    Empty;
    Board.Piece (King Black);
    Empty;
    Empty;
    Empty;
  ]

let rank_6_all =
  [
    Board.Empty;
    Empty;
    Empty;
    Board.Piece (Queen White);
    Piece (King White);
    Piece (Rook White);
    Empty;
    Empty;
  ]

let empty_ranks =
  [ Board.Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]

(* e2 to e4 *)
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

let stalemate_board =
  [
    rank_8_king;
    empty_ranks;
    rank_6_all;
    empty_ranks;
    empty_ranks;
    empty_ranks;
    empty_ranks;
    empty_ranks;
  ]

(* states *)
let start_state = State.init_state ()
let state_1 = get_state (State.make_move start_state [ 4; 6; 4; 5 ])

let state_black_capture : State.state =
  get_state
    (State.make_move
       (get_state
          (State.make_move
             (get_state
                (State.make_move
                   (get_state (State.make_move start_state [ 4; 6; 4; 4 ]))
                   [ 4; 1; 4; 3 ]))
             [ 5; 6; 5; 4 ]))
       [ 4; 3; 5; 4 ])

let state_white_capture : State.state =
  get_state
    (State.make_move
       (get_state
          (State.make_move
             (get_state
                (State.make_move
                   (get_state
                      (State.make_move
                         (get_state
                            (State.make_move state_black_capture [ 5; 7; 2; 4 ]))
                         [ 3; 0; 7; 4 ]))
                   [ 4; 7; 5; 7 ]))
             [ 1; 1; 1; 3 ]))
       [ 2; 4; 1; 3 ])

let get_undone un =
  match un with
  | State.Undo_Fail -> failwith "can't undo"
  | Undone st -> st

let get_redone re =
  match re with
  | State.Redo_Fail -> failwith "can't redo"
  | Redone st -> st

let state_no_legals =
  get_state
    (State.make_move
       (get_state
          (State.make_move
             (get_state
                (State.make_move
                   (get_state (State.make_move start_state [ 5; 6; 5; 5 ]))
                   [ 4; 1; 4; 3 ]))
             [ 6; 6; 6; 4 ]))
       [ 3; 0; 7; 4 ])

let state_black_in_check =
  get_state
    (State.make_move
       (get_state
          (State.make_move
             (get_state (State.make_move start_state [ 4; 6; 4; 4 ]))
             [ 3; 1; 3; 3 ]))
       [ 5; 7; 1; 3 ])

let state_tests =
  [
    compare_board "compare board e2->e4" [ 4; 6; 4; 4 ] start_state
      legal_board_1;
    make_move_illegalmove_test "illegal move from e1 to h6" [ 4; 7; 5; 2 ]
      start_state;
    make_move_illegalmove_test "black is in check, must move out of it"
      [ 0; 1; 0; 2 ] state_black_in_check;
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
    black_capture_test "black captured one" state_black_capture [ Pawn White ];
    white_capture_test "white captured one" state_white_capture [ Pawn Black ];
    undo_test "undo start board" start_state Undo_Fail;
    (* undo_test "undo one move (only took one move)" state_1 (Undone
       start_state); *)
    redo_test "redo start board" start_state Redo_Fail;
    has_legal_moves_test "start game has legal moves " start_state true;
    has_legal_moves_test "after one move the game still has legal moves "
      state_1 true;
    has_legal_moves_test "white is in checkmate, has no legal moves "
      state_no_legals false;
    is_in_check_test "white is in check" state_no_legals true;
    is_in_check_test "black is in check" state_black_in_check true;
    is_in_check_test "after one move, neither player is in check" state_1 false;
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

(* 上下的左边 *)
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

(* 上面的左边 e2 to e4*)
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

(* 下面的左边 *)
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

(* 上面的左边 e2e4 knight *)
let legal_board_knight =
  [
    rank_8_1;
    rank_7_2;
    rank_6_2;
    empty_ranks;
    rank_4_1;
    rank_3_knight;
    rank_2_1;
    rank_1_no_knight;
  ]

(* 上下的左边 无rook *)
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
    move_test "move rook " legal_board_3 Player.White [ 1; 7; 2; 5 ]
      legal_board_knight;
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
    king_valid_test
      "should not be a valid move in state (result in a check), but should\n\
      \    pass the king movement check" stalemate_board Player.Black
      [ 4; 0; 4; 1 ] true;
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
