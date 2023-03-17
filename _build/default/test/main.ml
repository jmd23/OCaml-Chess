open OUnit2

let suite = "test suite for project" >::: List.flatten []
let _ = run_test_tt_main suite
