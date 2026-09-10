open Assign2

let testing = true

let run cases b = if b then cases () else []

let taxicab_tests () =
  [
    assert (taxicab 2 = 1);
    assert (taxicab 5 = 0);
    assert (taxicab 1729 = 2);
    assert (taxicab 4104 = 2);
  ]

let drop_trailing_tests () =
  [
    assert (drop_trailing 3 [1;2;3;3] = [1;2]);
    assert (drop_trailing 5 [1;2;3] = [1;2;3]);
    assert (drop_trailing 2 [2;2;2] = []);
  ]

let every_k_tests () =
  [
    assert (every_k 1 [1;2;3;4;5] = [1;2;3;4;5]);
    assert (every_k 2 [1;2;3;4;5] = [1;3;5]);
    assert (every_k 3 [1;2;3;4;5] = [1;4]);
    assert (every_k 4 [1;2;3;4;5] = [1;5]);
    assert (every_k 5 [1;2;3;4;5] = [1]);
  ]

let is_bitonic_tests () =
  [
    assert (is_bitonic [1;2;3;2;1] = true);
    assert (is_bitonic [1;2;3] = true);
    assert (is_bitonic [3;2;1;2] = true);
    assert (is_bitonic [] = true);
    assert (is_bitonic [1;1] = false);
    assert (is_bitonic [1;2;1;2] = false);
  ]

let factor_tests () =
  [
    assert (factor 1 = []);
    assert (factor 19 = [(19,1)]);
    assert (factor 16 = [(2,4)]);
    assert (factor 360 = [(2,3);(3,2);(5,1)]);
  ]

let is_valid_tests () =

  let p1 = (0, [(true, 3); (false, 5)]) in
  let p2 = (5, [(false, 3); (false, 7)]) in

  let p3 = (0, [(true, 6); (false, 12)]) in
  let p4 = (5, [(true, 2); (false, 12)]) in

  let p5 = (0, [(true, 6)]) in

  [
    assert (not (is_valid (10, []) (10, [])));
    assert (not (is_valid p2 p4));
    assert (is_valid (0, []) (10, []));
    assert (not (is_valid p1 p2));
    assert (is_valid p3 p4);
    assert (not (is_valid p5 p4));
  ]

let _run_tests =
  if not testing then [] else
    [
      run taxicab_tests true;
      run drop_trailing_tests true;
      run every_k_tests true;
      run is_bitonic_tests true;
      run factor_tests true;
      run is_valid_tests true;
    ]
