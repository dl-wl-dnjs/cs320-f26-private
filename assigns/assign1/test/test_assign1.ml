open Assign1


let num_digits n =
  let rec count n =
    if n < 10 then 1
    else 1 + count (n / 10)
  in
  count (abs n)



let rec num_factors (k:int) : int = 
  let rec aux (res : int) (counter: int) (k:int) = 
    if k=1 then res else 
      if k mod counter =0 then aux (res + 1) counter (k / counter)
    else aux res (counter + 1) k

    in aux 0 2 k


let is_hypotenuse (n : int) : bool =
  let rec go a b =
    if a > b then false
    else
      let s = a * a + b * b in
      if s = n * n then true
      else if s < n * n then go (a + 1) b
      else go a (b - 1)
  in
  go 1 (n - 1)


let rec drop_leading (k:int) (l:int list) : int list = 
  match l with 
  | [] -> []
  | h::t -> if h = k then drop_leading k t else h::t





let testing = true

let run cases b = if b then cases () else []

let num_digits_tests () =
  [
    assert (num_digits 0 = 1);
    assert (num_digits 7 = 1);
    assert (num_digits 12345 = 5);
    assert (num_digits (-121) = 3);
  ]

(*
let is_perfect_pow_tests () =
  [
    assert (is_perfect_pow 1 16);
    assert (is_perfect_pow 2 16);
    assert (not (is_perfect_pow 3 16));
    assert (is_perfect_pow 4 16);
    assert (is_perfect_pow 3 (-8));
  ]
*)

let num_factors_tests () =
  [
    assert (num_factors 16 = 4);
    assert (num_factors 18 = 3);
    assert (num_factors 19 = 1);
  ]


let is_hypotenuse_tests () =
  [
    assert (is_hypotenuse 5);
    assert (is_hypotenuse 13);
    assert (is_hypotenuse 17);
    assert (is_hypotenuse 29);
    assert (not (is_hypotenuse 28));
    assert (not (is_hypotenuse 6));
  ]


let drop_leading_tests () =
  [
    assert (drop_leading 1 [1;1;2;1;3] = [2;1;3]);
    assert (drop_leading 5 [1;2;3] = [1;2;3]);
    assert (drop_leading 2 [2;2;2] = []);
  ]

let _run_tests =
  if not testing then [] else
    [
      run num_digits_tests true;
      (* run is_perfect_pow_tests true; *)
      run num_factors_tests true;
      (* run is_hypotenuse_tests true; *)
      run drop_leading_tests true;
    ]
