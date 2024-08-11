type expr =
  | Const of bool
  | Var of char
  | Not of expr
  | Or of expr * expr
  | And of expr * expr

let rec free_variable = function
  | Const _ -> None
  | Var v -> Some v
  | Not e -> free_variable e
  | Or (x, y) | And (x, y) ->
      match free_variable x with
      | Some _ as result -> result
      | None -> free_variable y

let rec guess_variable var var_bool expr = 
  match expr with 
  | Var v -> if v = var then Const val_bool else Var v 
  | Not e -> Not (guess e)
  | Or (x,y) -> Or(guess x, guess y)
  | And (x,y) -> And (guess x , guess y)
  | Const b -> Const b 
  where guess = guess_variable var val_bool

let () = print_endline "Hello, World!"
