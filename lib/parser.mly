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
  %token <string> STRING
//OPS
%token ADD
%token TIMES
%token CAT
%token LEN
// %token PUSH
// %token POP
// %token MERGE

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
    | ADD LPAREN v1 = INT COMMA v2 = INT RPAREN SEMICOLON { Plus (v1, v2) }   //1st case
    | TIMES LPAREN v1 = INT COMMA v2 = INT RPAREN SEMICOLON { Times (v1, v2) }  
    | CAT LPAREN s1 = STRING COMMA s2 = STRING RPAREN SEMICOLON { Cat (s1, s2) }
    | LEN LPAREN s = STRING RPAREN SEMICOLON { Len (s) }
    
;
