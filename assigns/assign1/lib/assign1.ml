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