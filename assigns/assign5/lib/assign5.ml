let rev (l : 'a list) : 'a list =
  let rec go acc l =
    match l with
    | [] -> acc
    | x :: xs -> go (x :: acc) xs
  in
  go [] l

let group (l : int list) : int list list option =
  let same_sign x y = (x > 0) = (y > 0) in
  let rec go cur acc l =
    match l with
    | [] ->
      (match cur with
       | [] -> Some (rev acc)
       | _ -> Some (rev (rev cur :: acc)))
    | 0 :: rest ->
      (match cur, rest with
       | c :: _, y :: _ when y <> 0 && not (same_sign c y) ->
         go [] (rev cur :: acc) rest
       | _ -> None) (* leading/trailing/double zero, or same sign across zero *)
    | x :: rest ->
      (match cur with
       | c :: _ when not (same_sign c x) -> None (* opposite signs touching *)
       | _ -> go (x :: cur) acc rest)
  in
  go [] [] l

type 'a rtree = Node of 'a * 'a rtree list

let rec split (t : ('a * 'b) rtree) : 'a rtree * 'b rtree =
  let rec unzip l =
    match l with
    | [] -> ([], [])
    | (a, b) :: rest ->
      let (as_, bs) = unzip rest in
      (a :: as_, b :: bs)
  in
  match t with
  | Node ((a, b), children) ->
    let (ls, rs) = unzip (List.map split children) in
    (Node (a, ls), Node (b, rs))

let prefix_map (f : 'a list -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let rec go pre_rev rest =
    match f (rev pre_rev) with
    | Some b -> Some (b, rest)
    | None ->
      (match rest with
       | [] -> None
       | x :: xs -> go (x :: pre_rev) xs)
  in
  go [] l

let apply_cycle (f : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
  let rec go fs n x =
    if n = 0 then x
    else
      match fs with
      | [] -> go f n x
      | g :: rest -> go rest (n - 1) (g x)
  in
  go f n x

let walks (f : 'a -> 'a -> bool) (n : int) (ps : (('a -> 'a) * 'a) list) : 'a list =
  let rec valid p x k = k = 0 || (f x (p x) && valid p (p x) (k - 1)) in
  let rec endpoint p x k = if k = 0 then x else endpoint p (p x) (k - 1) in
  ps
  |> List.filter (fun (p, start) -> valid p start n)
  |> List.map (fun (p, start) -> endpoint p start n)
