open Utils

module Type = struct
  type t =
    | Nat
    | Arr of t * t

  (* <ty> ::= Nat | ( -> <ty> <ty> ) *)
  let rec of_sexpr_opt (e : Sexpr.t) : t option =
    match e with
    | Sexpr.Atom "Nat" -> Some Nat
    | Sexpr.List [Sexpr.Atom "->"; t1; t2] ->
       begin
         match of_sexpr_opt t1, of_sexpr_opt t2 with
         | Some t1, Some t2 -> Some (Arr (t1, t2))
         | _ -> None
       end
    | _ -> None

  let rec to_string = function
    | Nat -> "Nat"
    | Arr (t1, t2) -> "(-> " ^ to_string t1 ^ " " ^ to_string t2 ^ ")"
end

module Expr = struct
  type t =
    | Var of string
    | Fun of string * Type.t * t
    | App of t * t
    | Let of string * t * t
    | Zero
    | Succ of t
    | Rec_nat of t * t * t

  let ( let* ) = Option.bind

  let keywords = ["lambda"; "let"; "add1"; "rec-Nat"]

  (* <var> : nonempty sequence of lowercase letters or underscores *)
  let is_var s =
    s <> ""
    && String.for_all (fun c -> is_lower c || c = '_') s
    && not (List.mem s keywords)

  (* <nat> : nonempty sequence of digits *)
  let is_nat s =
    s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

  let rec nat_of_int n =
    if n <= 0 then Zero else Succ (nat_of_int (n - 1))

  let rec of_sexpr_opt (se : Sexpr.t) : t option =
    match se with
    | Sexpr.Atom s when is_nat s ->
       let* n = int_of_string_opt s in
       Some (nat_of_int n)
    | Sexpr.Atom x when is_var x -> Some (Var x)
    | Sexpr.List [Sexpr.Atom "lambda"; Sexpr.Atom x; ty; body] when is_var x ->
       let* ty = Type.of_sexpr_opt ty in
       let* body = of_sexpr_opt body in
       Some (Fun (x, ty, body))
    | Sexpr.List [Sexpr.Atom "let"; Sexpr.Atom x; e1; e2] when is_var x ->
       let* e1 = of_sexpr_opt e1 in
       let* e2 = of_sexpr_opt e2 in
       Some (Let (x, e1, e2))
    | Sexpr.List [Sexpr.Atom "add1"; e] ->
       let* e = of_sexpr_opt e in
       Some (Succ e)
    | Sexpr.List [Sexpr.Atom "rec-Nat"; e1; e2; e3] ->
       let* e1 = of_sexpr_opt e1 in
       let* e2 = of_sexpr_opt e2 in
       let* e3 = of_sexpr_opt e3 in
       Some (Rec_nat (e1, e2, e3))
    | Sexpr.List [e1; e2] ->
       let* e1 = of_sexpr_opt e1 in
       let* e2 = of_sexpr_opt e2 in
       Some (App (e1, e2))
    | _ -> None

  let of_string_res s =
    match Sexpr.of_string_opt s with
    | Some se -> Option.to_result ~none:"syntax error" (of_sexpr_opt se)
    | None -> Error "syntax error"

  let of_string s =
    match of_string_res s with
    | Ok e -> e
    | Error _ -> raise (Failure "Expr.of_string")

  (* [e'/x]e -- not capture-avoiding, since we only substitute closed
     values and never evaluate under binders *)
  let subst (e' : t) (x : string) (e : t) : t =
    let rec go e =
      match e with
      | Var y -> if x = y then e' else e
      | Fun (y, ty, body) ->
         if x = y then e else Fun (y, ty, go body)
      | App (e1, e2) -> App (go e1, go e2)
      | Let (y, e1, e2) ->
         if x = y then Let (y, go e1, e2) else Let (y, go e1, go e2)
      | Zero -> Zero
      | Succ e -> Succ (go e)
      | Rec_nat (e1, e2, e3) -> Rec_nat (go e1, go e2, go e3)
    in go e
end

module Ctxt = struct
  module M = Map.Make(String)
  type t = Type.t M.t

  (* empty context

     Ctxt.empty ≡ ∅ ≡ {}
   *)
  let empty = M.empty

  (* Determine declared type for a variable in the context

     Ctxt.find_opt x Γ = { Some τ    if (x : τ) ∈ Γ
                         { None      otherwise
   *)
  let find_opt : string -> t -> Type.t option = M.find_opt

  (* Add a declartion to the context

     Ctxt.add x τ Γ ≡ Γ, x : τ
   *)
  let add : string -> Type.t -> t -> t = M.add
end

let ( let* ) = Result.bind

let rec type_of (gamma: Ctxt.t) (e : Expr.t) : (Type.t, string) result =
  let mismatch what expected actual =
    Error (Format.sprintf "%s: expected type %s but got %s"
             what (Type.to_string expected) (Type.to_string actual))
  in
  match e with
  (* var *)
  | Var x ->
     begin
       match Ctxt.find_opt x gamma with
       | Some ty -> Ok ty
       | None -> Error ("unknown variable " ^ x)
     end
  (* fun *)
  | Fun (x, ty1, body) ->
     let* ty2 = type_of (Ctxt.add x ty1 gamma) body in
     Ok (Type.Arr (ty1, ty2))
  (* app *)
  | App (e1, e2) ->
     let* ty1 = type_of gamma e1 in
     let* ty2 = type_of gamma e2 in
     begin
       match ty1 with
       | Type.Arr (ty_arg, ty_ret) ->
          if ty_arg = ty2
          then Ok ty_ret
          else mismatch "function argument" ty_arg ty2
       | Type.Nat -> Error "applying a non-function (type Nat)"
     end
  (* let *)
  | Let (x, e1, e2) ->
     let* ty1 = type_of gamma e1 in
     type_of (Ctxt.add x ty1 gamma) e2
  (* zero *)
  | Zero -> Ok Type.Nat
  (* succ *)
  | Succ e ->
     let* ty = type_of gamma e in
     if ty = Type.Nat
     then Ok Type.Nat
     else mismatch "argument of add1" Type.Nat ty
  (* recNat *)
  | Rec_nat (e1, e2, e3) ->
     let* ty1 = type_of gamma e1 in
     let* ty = type_of gamma e2 in
     let* ty3 = type_of gamma e3 in
     let expected_step = Type.(Arr (Nat, Arr (ty, ty))) in
     if ty1 <> Type.Nat
     then mismatch "first argument of rec-Nat" Type.Nat ty1
     else if ty3 <> expected_step
     then mismatch "step function of rec-Nat" expected_step ty3
     else Ok ty

let rec eval (e : Expr.t) : Expr.t =
  match e with
  (* funE *)
  | Fun _ -> e
  (* appE *)
  | App (e1, e2) ->
     begin
       match eval e1 with
       | Fun (x, _, body) ->
          let v2 = eval e2 in
          eval (Expr.subst v2 x body)
       | _ -> failwith "eval: applying a non-function"
     end
  (* letE *)
  | Let (x, e1, e2) ->
     let v1 = eval e1 in
     eval (Expr.subst v1 x e2)
  (* zeroE *)
  | Zero -> Zero
  (* succE *)
  | Succ e -> Succ (eval e)
  (* recZero / recSucc *)
  | Rec_nat (e1, e2, e3) ->
     begin
       match eval e1 with
       | Zero -> eval e2
       | Succ v1 -> eval (App (App (e3, v1), Rec_nat (v1, e2, e3)))
       | _ -> failwith "eval: rec-Nat on a non-number"
     end
  | Var x -> failwith ("eval: unbound variable " ^ x)

(* multiply m n = add n to 0, m times *)
let multiply : Expr.t =
  Expr.of_string
    "(lambda m Nat
       (lambda n Nat
         (let add
           (lambda a Nat
             (lambda b Nat
               (rec-Nat a b (lambda p Nat (lambda x Nat (add1 x))))))
         (rec-Nat m 0 (lambda p Nat (lambda acc Nat ((add n) acc)))))))"

(* the spec's signature spells it this way *)
let multipy = multiply

let string_of_value e =
  let rec int_of_value_opt : Expr.t -> int option = function
    | Zero -> Some 0
    | Succ n ->
       begin
         match int_of_value_opt n with
         | Some n -> Some (n + 1)
         | None -> None
       end
    | _ -> None
  in
  match int_of_value_opt e with
  | Some n -> string_of_int n
  | None -> "<fun>"

let interp (s : string) : unit =
  match Expr.of_string_res s with
  | Ok e ->
     begin
       match type_of Ctxt.empty e with
       | Ok _ -> Format.printf "Success: %s" (string_of_value (eval e))
       | Error e -> Format.eprintf "Error: %s" e
     end
  | Error e -> Format.eprintf "Error: %s" e
