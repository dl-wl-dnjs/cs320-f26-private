let taxicab (n : int) : int =
  let rec aux n a b count = 
    if (a*a*a) > n then count 
    else if (a*a*a) + (b*b*b) > n then aux n (a+1) (a+1) count 
    else if (a*a*a) + (b*b*b) = n then aux n a (b+1) (count + 1) 
    else aux n a (b+1) count
      
  in aux n 1 1 0

let rec drop_trailing (k : 'a) (l : 'a list) : 'a list =
  
  match l with 
  | [] -> []
  | h :: t -> 
    let rest = drop_trailing k t in 
    if rest = [] && h = k then []
    else h :: rest


    

let every_k (k : 'a) (l : 'a list) : 'a list =
  let rec go n l =
    match l with
    | [] -> []
    | x :: xs ->
      if n = 0 then x :: go (k - 1) xs
      else go (n - 1) xs
  in
  go 0 l


  let rec strict_inc = function
  | x :: (y :: _ as rest) -> x < y && strict_inc rest
  | _ -> true

let rec strict_dec = function
  | x :: (y :: _ as rest) -> x > y && strict_dec rest
  | _ -> true

let rec after_inc = function
  | x :: (y :: _ as rest) when x < y -> after_inc rest
  | l -> l

let rec after_dec = function
  | x :: (y :: _ as rest) when x > y -> after_dec rest
  | l -> l
let is_bitonic (l : int list) : bool =
  strict_dec (after_inc l) || strict_inc (after_dec l)

let factor (n : int) : (int * int) list =
  let rec strip m d e =
    if m mod d = 0 then strip (m / d) d (e + 1) else (m, e)
  in
  let rec go m d =
    if m = 1 then []
    else if d * d > m then [(m, 1)]
    else if m mod d = 0 then
      let (m', e) = strip m d 0 in
      (d, e) :: go m' (d + 1)
    else go m (d + 1)
  in
  go n 2

type path = int * (bool * int) list

let is_valid (p1 : path) (p2 : path) : bool =
  let (s1, l1) = p1 in
  let (s2, l2) = p2 in
  let delta = function
    | [] -> (0, [])
    | (dir, dist) :: t -> ((if dir then dist else (0-dist)), t)
  in
  let rec go right a b l1 l2 =
    (if right then a - b > 0 else a - b < 0)
    && match l1, l2 with
       | [], [] -> true
       | _ ->
         let (da, t1) = delta l1 in
         let (db, t2) = delta l2 in
         go right (a + da) (b + db) t1 t2
  in
  go true s1 s2 l1 l2 || go false s1 s2 l1 l2

