{
  open Parser
  open Lexing

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let id = ['a'-'z']+
let string = []

rule read =
  parse
  | '\n' { newline lexbuf; read lexbuf }
    | white { read lexbuf }
    | "Empty" { EMPTY }
    | "Push"  { PUSH }
    | "Pop"   { POP }
    | "Merge" { MERGE }
    | ";"     { SEMICOLON }
    | ","     { COMMA }
    | "("     { LPAREN }
    | ")"     { RPAREN }
    | "let"   { LET }
    | "="     { EQUALS }
    | "."     { DOT }
    | (id as s) { ID s }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }