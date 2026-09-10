
let taxicab (n : int) : int =
  let _ = ignore n in
  assert false

let drop_trailing (k : 'a) (l : 'a list) : 'a list =
  let _ = ignore (k, l) in
  assert false

let every_k (k : 'a) (l : 'a list) : 'a list =
  let _ = ignore (k, l) in
  assert false

let is_bitonic (l : int list) : bool =
  let _ = ignore l in
  assert false

let factor (n : int) : (int * int) list =
  let _ = ignore n in
  assert false

type path = int * (bool * int) list

let is_valid (p1 : path) (p2 : path) : bool =
  let _ = ignore (p1, p2) in
  assert false
