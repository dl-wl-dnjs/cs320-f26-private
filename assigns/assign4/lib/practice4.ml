 let rec negatives l = 
    match l with 
    | [] -> []
    | h :: t -> if h < 0 
      then h :: negatives t 
      else negatives t

let rec fold_right op l base = 
  match l with
  |[] -> base
  |x :: xs -> op x (fold_right op xs base)

let filter (p : 'a -> bool) (l : 'a list) = 
 List.fold_right (fun x thing -> (if p x then [x] else []) @ thing) l []
let append l r = List.fold_right ( fun x xs -> x :: xs) r l

type 'a tree = 
| Leaf
| Node of 'a * 'a tree * 'a tree

let rec reverse (t : 'a tree) : 'a tree = 
  match t with
  | Leaf -> Leaf
  | Node ( x, left, right) -> Node (x, reverse right, reverse left)
  