open OUnit2
open Braincurry

let assert_parse parser input expected =
  let actual = Parsley.run_all parser input in
  assert_equal actual (Parsley.Success expected)

let tests = "test suite for braincurry" >::: [
  "parseBinder 0" >::
    (fun _ ->
      assert_parse parseBinder "_" (Bind 0));

  "parseBinder 1" >::
    (fun _ ->
      assert_parse parseBinder "_+" (Bind 1));

  "parseBinder 3" >::
    (fun _ ->
      assert_parse parseBinder "_+++" (Bind 3));

  "parse io write" >::
    (fun _ ->
      assert_parse parseIO "." IOWrite);

  "parse io read" >::
    (fun _ ->
      assert_parse parseIO "," IORead);
]

let _ = run_test_tt_main tests
