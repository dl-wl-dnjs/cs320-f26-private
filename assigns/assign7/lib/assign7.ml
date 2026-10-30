open Utils

module Sexpr = Sexpr

module Expr = struct
  type t =
    | Fun of string * t
    | App of t * t
    | Var of string

  let to_string e =
    let rec go = function
      | Fun (x, e) -> Format.sprintf "(lambda %s %s)" x (go e)
      | App (e1, e2) -> Format.sprintf "(%s %s)" (go e1) (go e2)
      | Var x -> x
    in go e

  let ( let* ) = Option.bind

  let is_var s =
    s <> "" && String.for_all (fun c -> 'a' <= c && c <= 'z') s

  let rec of_sexpr_opt se = 
    match se with
    | Sexpr.Atom x when is_var x -> Some (Var x)
    | Sexpr.List [Sexpr.Atom "lambda"; Sexpr.Atom x; body] when is_var x ->
       let* e = of_sexpr_opt body in
       Some (Fun (x, e))
    | Sexpr.List [se1; se2] ->
       let* e1 = of_sexpr_opt se1 in
       let* e2 = of_sexpr_opt se2 in
       Some (App (e1, e2))
    | _ -> None
  let of_string_opt s =
    let* se = Sexpr.of_string_opt s in
    of_sexpr_opt se

  let of_string s =
    match of_string_opt s with
    | Some e -> e
    | None -> raise (Failure "Expr.of_string")

  let rec fv : t -> VarSet.t = function
    | Fun (x, e) -> VarSet.remove x (fv e)
    | App (e1, e2) -> VarSet.union (fv e1) (fv e2)
    | Var x -> VarSet.singleton x

  let rec subst e' x e =
    match e with
    | Var y -> if y = x then e' else e
    | App (e1, e2) -> App (subst e' x e1, subst e' x e2)
    | Fun (y, body) ->
       if y = x then e
       else if VarSet.mem y (fv e') then
         let z = gensym () in
         Fun (z, subst e' x (subst (Var z) y body))
       else Fun (y, subst e' x body)
  let equal e1 e2 = 
    let rec index x i = function
      | [] -> None
      | y :: ys -> if x = y then Some i else index x (i + 1) ys
    in
    let rec go env1 env2 e1 e2 =
      match e1, e2 with
      | Var x, Var y ->
         (match index x 0 env1, index y 0 env2 with
          | None, None -> x = y
          | Some i, Some j -> i = j
          | _ -> false)
      | App (a1, b1), App (a2, b2) ->
         go env1 env2 a1 a2 && go env1 env2 b1 b2
      | Fun (x, b1), Fun (y, b2) -> go (x :: env1) (y :: env2) b1 b2
      | _ -> false
    in go [] [] e1 e2
end

let rec step (e : Expr.t) : Expr.t option =
  match e with
  | Var _ -> None
  | Fun (x, body) ->                                   (* fun *)
     Option.map (fun body' -> Expr.Fun (x, body')) (step body)
  | App (Fun (x, e1), e2) -> Some (Expr.subst e2 x e1) (* beta *)
  | App (e1, e2) ->
     match step e1 with
     | Some e1' -> Some (Expr.App (e1', e2))           (* appLeft *)
     | None ->                                         (* appRight *)
        Option.map (fun e2' -> Expr.App (e1, e2')) (step e2)

let multistep =
  let rec go e =
    match step e with
    | Some e -> go e
    | None -> e
  in go

let interp s =
  match Expr.of_string_opt s with
  | None -> print_endline "parse error"
  | Some e ->
     match multistep e with
     | e -> e |> Expr.to_string |> print_endline

let expr_of_int n =
  let rec body n =
    if n <= 0 then Expr.Var "x"
    else Expr.App (Expr.Var "f", body (n - 1))
  in Expr.Fun ("f", Expr.Fun ("x", body n))

let int_of_expr_opt (e : Expr.t) =
  match e with
  | Fun (f, Fun (x, body)) ->
     (* if f = x, the inner binder shadows f, so only λx.λx.x (= 0) is a numeral *)
     let rec count = function
       | Expr.Var y when y = x -> Some 0
       | Expr.App (Expr.Var g, rest) when g = f && f <> x ->
          Option.map (fun n -> n + 1) (count rest)
       | _ -> None
     in count body
  | _ -> None

let int_of_expr e =
  match int_of_expr_opt e with
  | Some n -> n
  | None -> raise (Failure "int_of_expr")


let ( @@@ ) e1 e2 = Expr.App (e1, e2)
let v x = Expr.Var x
let lam x e = Expr.Fun (x, e)

let suc : Expr.t = lam "n" (lam "f" (lam "x" (v "f" @@@ ((v "n" @@@ v "f") @@@ v "x"))))
let add : Expr.t = lam "m" (lam "n" (lam "f" (lam "x"
    ((v "m" @@@ v "f") @@@ ((v "n" @@@ v "f") @@@ v "x")))))

