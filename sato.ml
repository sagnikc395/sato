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

      
let () = print_endline "Hello, World!"
