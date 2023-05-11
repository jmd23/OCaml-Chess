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

let set_square_test (name : string) i j (board : Board.board) (p : Board.square)
    (expected_output : Board.square list list) : test =
  name >:: fun _ -> assert_equal expected_output (Board.set_square i j board p)

let validate_owner_test (name : string) (plr : Player.player) (p : Board.piece)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (Board.validate_owner plr p)

(* -------------------Movement Tests---------------------- *)
let move_test (name : string) (board : Board.board) (plr : Player.player)
    (lst : int list) (expected_output : Movement.result) : test =
  name >:: fun _ -> assert_equal expected_output (Movement.move board plr lst)

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

let make_move_illegalmove_test (name : string) lst (st : State.state)
    (expected_output : State.move_result) : test =
  name >:: fun _ -> assert_equal State.Illegal_Move (State.make_move st lst)

let make_move_illegalpiece_test (name : string) lst (st : State.state)
    (expected_output : State.move_result) : test =
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
let white_capture_test (name : string) st (st : State.state)
    (expected_output : Board.piece list) : test =
  name >:: fun _ -> assert_equal expected_output (State.get_white_captured st)

let black_capture_test (name : string) st (st : State.state)
    (expected_output : Board.piece list) : test =
  name >:: fun _ -> assert_equal expected_output (State.get_black_captured st)

(* undo/redo *)
let undo_test (name : string) st (st : State.state)
    (expected_output : State.undo_result) : test =
  name >:: fun _ -> assert_equal expected_output (State.undo st)

let redo_test (name : string) st (st : State.state)
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

let parse_test (name : string) (s : string) (expected_output : Commands.command)
    : test =
  name >:: fun _ -> assert_equal expected_output (Commands.parse s)

(* -----------------------------Specific Tests------------------------------- *)
(* boards *)
let start_board = Board.starting_board

(* new board legal move 1 *)
let rank_1 =
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

let rank_2 =
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

let rank_4 =
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

let rank_7 =
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

let rank_8 =
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
    rank_8;
    rank_7;
    empty_ranks;
    empty_ranks;
    rank_4;
    empty_ranks;
    rank_2;
    rank_1;
  ]

(* states *)
let start_state = State.init_state ()
let state_1 = get_state (State.make_move start_state [ 1; 4; 3; 4 ])

let state_tests =
  [
    make_move_illegalmove_test "illegal move from e1 to h6" [ 0; 4; 5; 7 ]
      start_state State.Illegal_Move;
    make_move_illegalmove_test "illegal piece from e8 to e3" [ 7; 4; 2; 4 ]
      start_state State.Illegal_Piece;
    move_one_legal_compare "make one legal move: e2 to e4" [ 1; 4; 3; 4 ]
      start_state;
    move_one_legal_compare "make a second legal move from e2->e4 then e7->e5"
      [ 6; 4; 4; 4 ] state_1;
    move_two_legal_compare "make two legal moves: e2->e4 then e7->e5"
      [ 1; 4; 3; 4 ] [ 6; 4; 4; 4 ] start_state;
  ]

let suite = "test suite for project" >::: List.flatten []
let _ = run_test_tt_main suite
