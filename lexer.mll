{

open Parser

let keyword_table = Hashtbl.create 53
let _ = 
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [
      ("number", T_NUMBER);
      ("string", T_STRING);
      ("boolean", T_BOOLEAN);
      ("if", T_IF);
      ("else", T_ELSE);
      ("while", T_WHILE);
      ("return", T_RETURN);
      ("let", T_LET);
      ("var", T_VAR);
      ("function", T_FUNCTION);
    ]

let token_of_word = fun w ->
  try (Hashtbl.find keyword_table w) w
  with Not_found -> T_IDENTIFIER (w)
}

let word    = "[A-Za-z_][A-Za-z0-9_]*"

let hex_const   = Str.regexp "0[xX][0-9a-fA-F]+"
let bin_const   = Str.regexp "0[bB][01]+"
let dec_const   = Str.regexo "[0-9]+"
let str_lit     = Str.regexp "\".+\""

let sl_comment  = Str.regexp "//[^$]*"
let ml_comment  = Str.regexp "\/*.+*\/"

let whitespace  = Str.regexp "[ \t\r\n]+"

rule token = parse
  | sl_comment          { token lexbuf }
  | ml_comment          { token lexbuf }
  | whitespace          { token lexbuf }
  | ','                 { T_COMMA }
  | '.'                 { T_DOT }
  | ':'                 { T_COLON }
  | ';'                 { T_SEMICOLON }
  | '('                 { T_OPEN_PAR }
  | ')'                 { T_CLOSE_PAR }
  | '{'                 { T_OPEN_BRA }
  | '}'                 { T_CLOSE_BRA }
  | '['                 { T_OPEN_SQR }
  | ']'                 { T_CLOSE_SQR }
  | '+'                 { T_PLUS }
  | '-'                 { T_MINUS }
  | '*'                 { T_MUL }
  | '/'                 { T_DIV }
  | '='                 { T_ASSIGN }
  | '<'                 { T_LESS }
  | '>'                 { T_MORE }
  | '!'                 { T_NOT }
  | word      as lexeme { token_of_word lexeme }
  | hex_const as lexeme { T_INT_LIT (int_of_hex lexeme) }
  | bin_const as lexeme { T_INT_LIT (int_of_bin lexeme) }
  | int_const as lexeme { T_INT_LIT (int_of_string lexeme) }
  


