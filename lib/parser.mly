// %{
//     open Printf
//     open Easy_logging
//     open Util
//     open Lexing

//     let (|@|) node loc = { node = node; loc = loc }
//     let logger = Logging.make_logger "Parser" Info [Cli Debug]
// %}


//values
  %token  <int> INT
 %token <string> ID

//OPS
%token EMPTY
%token PUSH
%token POP
%token MERGE

//Syntax
%token COMMA
%token SEMICOLON
//%token COLON
%token LET
%token LPAREN
%token RPAREN
%token DOT
%token EQUALS
%token EOF


%start <Stacks.program> prog

%%

prog :
    | e = expr* EOF { e }
    ;


expr:
    | LET s = ID EQUALS EMPTY SEMICOLON { Let (s, Empty) }   //1st case
    | s = ID DOT PUSH LPAREN v = INT RPAREN SEMICOLON { Push(v, s) } //2nd case
    | s = ID DOT POP LPAREN RPAREN SEMICOLON { Pop(s) } //3rd case
    | MERGE LPAREN s1 = ID COMMA s2 = ID RPAREN SEMICOLON { Merge(s1, s2) } //4th case
;
