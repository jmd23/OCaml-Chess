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

(* need to test string in terminal // note to fix later *)
let print_board_test (name : string) (board : Board.board)
    (expected_output : unit) : test =
  name >:: fun _ -> assert_equal expected_output (Board.print_board board)

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

let suite = "test suite for project" >::: List.flatten []
let _ = run_test_tt_main suite
