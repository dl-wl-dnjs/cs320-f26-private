
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
