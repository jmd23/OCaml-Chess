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

let board_to_string_test (name : string) (board : Board.board)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Board.board_to_string board)

let suite = "test suite for project" >::: List.flatten []
let _ = run_test_tt_main suite
