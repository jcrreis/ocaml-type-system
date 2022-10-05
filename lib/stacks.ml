
(* type ops =
 *     | Pop
 *     | Merge *)

(* type value =
  | Empty *)

type expr =
  (* | Int of int
  | String of string *)
  | Let of string * expr * expr
  | Plus of int * int
  | Times of int * int
  | Cat of string * string
  | Len of string

type program = expr list


let eval_expr mem_int mem_string expr =
  match expr with
  | Plus (v1, v2) -> Hashtbl.add mem_int "Plus" (v1 + v2) 
  | Times (v1, v2) -> Hashtbl.add mem_int "Times" (v1 * v2) 
  | Cat (s1, s2) -> Hashtbl.add mem_string "Cat" (s1 ^ s2) 
  | Len (s) -> Hashtbl.add mem_int "Len" (String.length s)
  | Let (_, _, _) -> ()




  (* | Let (s, Empty) ->
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
      Hashtbl.replace mem s2 new_s2 *)

let rec eval_program mem_int mem_string program =
  match program with
  | [] -> ()
  | e :: r ->
      eval_expr mem_int mem_string e;
      eval_program mem_int mem_string r
