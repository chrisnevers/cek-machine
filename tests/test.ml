
open OUnit
open Main

let example_test = fun () ->
    assert_equal true true

let suite =
  "Tests" >:::
  [
    "example test" >:: example_test;
  ]

let _ = run_test_tt_main suite

