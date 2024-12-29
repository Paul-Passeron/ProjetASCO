{

open Parser

let keyword_table = Hashtbl.create 53
let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [
      "number",   T_NUMBER;
      "string",   T_STRING;
      "boolean",  T_BOOLEAN;
      "if",       T_IF;
      "else",     T_ELSE;
      "while",    T_WHILE;
      "return",   T_RETURN;
      "let",      T_LET;
      "var",      T_VAR;
      "const",    T_CONST;
      "function", T_FUNCTION;
      "typeof",   T_TYPEOF;
      "type",     T_TYPE;
      "true",     T_BOOL_LIT true;
      "false",    T_BOOL_LIT false;
    ]

let token_of_word = fun w ->
  try Hashtbl.find keyword_table w
  with Not_found -> T_IDENTIFIER w
}

let word        = ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']*
let hex_const   = '0'['x''X'](['0'-'9''a'-'f''A'-'F']'_'?)+
let oct_const   = '0'['o''O'](['0'-'9''a'-'f''A'-'F']'_'?)+
let bin_const   = '0'['b''B'](['0''1']'_'?)+
let dec_const   = (['0'-'9']'_'?)+
let float_const = ((((['0'-'9']'_'?)*)'.'(['0'-'9']'_'?)*) | (['0'-'9']'_'?)+)(['e''E']['+''-']?(['0'-'9']'_'?)+)?
let sl_comment  = "//" [^'\n']* ('\n' | eof)
let ml_comment  = "/*" [^'*']* '*' [^'/']* "*/"
let whitespace  = [' ''\t''\r''\n']+

rule token = parse
  | sl_comment            { token lexbuf }
  | ml_comment            { token lexbuf }
  | whitespace            { token lexbuf }
  | ','                   { T_COMMA }
  | '.'                   { T_DOT }
  | ':'                   { T_COLON }
  | ';'                   { T_SEMICOLON }
  | '|'                   { T_BAR }
  | '('                   { T_OPEN_PAR }
  | ')'                   { T_CLOSE_PAR }
  | '{'                   { T_OPEN_BRA }
  | '}'                   { T_CLOSE_BRA }
  | '['                   { T_OPEN_SQR }
  | ']'                   { T_CLOSE_SQR }
  | '+'                   { T_PLUS }
  | '-'                   { T_MINUS }
  | '*'                   { T_MUL }
  | '/'                   { T_DIV }
  | '<'                   { T_LT }
  | "<="                  { T_LEQ }
  | '>'                   { T_GT }
  | ">="                  { T_GEQ }
  | "=="                  { T_EQ }
  | "!="                  { T_DIFF }
  | "==="                 { T_EQQ }
  | "!=="                 { T_NEQQ }
  | "&&"                  { T_AND }
  | "||"                  { T_OR }
  | '='                   { T_ASSIGN }
  | '!'                   { T_NOT }
  | "**"                  { T_POW }
  | word      as lexeme   { token_of_word lexeme }
  | hex_const as lexeme
  | oct_const as lexeme
  | bin_const as lexeme
  | dec_const as lexeme   { T_INT_LIT (int_of_string lexeme) }
  | float_const as lexeme { T_FLOAT_LIT (float_of_string lexeme) }
  |'\'' ( '\\' ['\''] | '\\' ['\\'] | ['^' '\n'] | [^'\''])* '\'' as lexeme    { let l = String.length lexeme in T_STR_LIT (String.sub lexeme 1 (l-2)) }
  |'\"' ( '\\' ['"'] | '\\' ['\\'] | ['^' '\n'] | [^'"'] )* '\"' as lexeme    { let l = String.length lexeme in T_STR_LIT (String.sub lexeme 1 (l-2)) }
  | "\"\"" { T_STR_LIT "" }
  (* | "''" { T_STR_LIT "" } *)
  | eof                   { EOF }
