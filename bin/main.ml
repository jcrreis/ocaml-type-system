(* open Calc
 *
 * (\* let () =
 *   let x = Main.parse "42" in
 *   print_string (Main.string_of_val x);
 *   Format.printf "@." *\)
 *
 * (\* let () =
 *   Format.printf "Hello\n" *\)
 *
 *
 * (\* let () =
 *    let y = Main.parse Sys.argv.(1) in
 *     let x = Main.eval y in
 *     print_string (Main.string_of_val x);
 *     Format.printf "@." *\)
 *   (\* let () =
 *   ignore (Main.step (Obj.magic None)) *\)
 *
 * let () =
 *   let lst = Stacks.push 10 [1;3;4]  in
 *   (\* let a = Stacks.merge lst [3;2;4] in
 *   List.iter (Printf.printf "%d ") a *\)
 *   let a = Stacks.merge lst [3;2;4] in
 *   List.iter (Printf.printf "%d ") a
 *
 *   (\* Printf.printf "%d" (Stacks.pop lst) *\)
 *   (\* let s = Stacks.empty
 *   s.push 1 s  *\) *)

open Calc

let fname = Sys.argv.(1)

let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Format.eprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let pp_stack s v =
  Format.eprintf "%s ---> " s;
  List.iter (fun e -> Format.eprintf "%d; " e) v;
  Format.eprintf "@."

let () =
  let cin = open_in fname in
  let lexbuf = Lexing.from_channel cin in
  try
    let program = Parser.prog Lexer.read lexbuf in
    let mem = Hashtbl.create 64 in
    Stacks.eval_program mem program;
    Hashtbl.iter pp_stack mem
  with Parser.Error ->
    Format.eprintf "Syntax error@.";
    print_position lexbuf;
    Format.eprintf "@."
