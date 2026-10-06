
let rec remove_key k l = 
    match l with
    | [] -> []
    | (x,y) :: t ->
      if k=x 
        then remove_key k t 
      else  (x, y) :: remove_key k t


let rec nub (l : ('a * 'b) list) : ('a * 'b) list =
  match l with
  | [] -> []
  | (k, v) :: t -> (k, v) :: nub (remove_key k t)
  
let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then acc
    else loop (s.[i] :: acc) (i + 1)
  in List.rev (loop [] 0)

let implode (l : char list) : string =
  String.init (List.length l) (List.nth l)

let split_by_ws' (s : string) : string list =
  let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let rec loop chars =
    match chars with
    | [] -> ([], [])
    | c :: cs ->
      let (word, res) = loop cs in
      if is_ws c then
        if word = [] then ([], res)
        else ([], implode word :: res)
      else (c :: word, res)
  in
  let (word, res) = loop (explode s) in
  if word = [] then res else implode word :: res


  let smth l i j = 
    if j < 0 then []
    else if i < 0 then [] 
    else if j < i then [] else
    let rec aux acc idx lst =
    match lst with
    | [] -> []
    | h :: t ->
      if idx < i then aux acc (idx + 1) t
      else if idx > j then acc 
      else aux (h :: acc) (idx + 1) t
    in
    aux [] 0 l
