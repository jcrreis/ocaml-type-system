
(* type ops =
 *     | Pop
 *     | Merge *)

type value =
  | Empty

type expr =
  | Let of string * value
  | Push of int * string
  | Pop of string
  | Merge of string * string

type program = expr list


let eval_expr mem expr =
  match expr with
  | Let (s, Empty) ->
      Hashtbl.add mem s []
  | Push (x, s) ->
      let current = Hashtbl.find mem s in
      let value = x :: current in
      Hashtbl.replace mem s value
  | Pop s ->
      let current = Hashtbl.find mem s in
      let new_val = match current with
        | [] -> []
        | _ :: r -> r in
      Hashtbl.replace mem s new_val
  | Merge (s1, s2) ->
      let current_s1 = Hashtbl.find mem s1 in
      let current_s2 = Hashtbl.find mem s2 in
      let new_s1 = current_s1 @ current_s2 in
      let new_s2 = [] in
      Hashtbl.replace mem s1 new_s1;
      Hashtbl.replace mem s2 new_s2

let rec eval_program mem program =
  match program with
  | [] -> ()
  | e :: r ->
      eval_expr mem e;
      eval_program mem r
