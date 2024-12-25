{

open Parsertmp

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
      ("typeof", T_TYPEOF);
    ]

let token_of_word = fun w ->
  try (Hashtbl.find keyword_table w)
  with Not_found -> T_IDENTIFIER (w)
}

let word        = ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']*

let hex_const   = '0'['x''X']['0'-'9''a'-'f''A'-'F']+
let bin_const   = '0'['b''B']['0''1']+
let dec_const   = ['0'-'9']+

let sl_comment  = "//" [^'\n']* ('\n' | eof)
let ml_comment  = "/*" [^'*']* '*' [^'/']* "*/"

let whitespace  = [' ''\t''\r''\n']+

let str_lit     = "\"" * "\""

let backslash_escapes =
    ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']


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
  | '<'                 { T_LT }
  | "<="                { T_LEQ }
  | '>'                 { T_GT }
  | ">="                { T_GEQ }
  | "=="                { T_EQUAL }
  | "!="                { T_DIFF }
  | "==="               { T_EQQ }
  | "!=="               { T_NEQQ }
  | "&&"                { T_AND }
  | "||"                { T_OR }
  | '='                 { T_ASSIGN }
  | '!'                 { T_NOT }
  | "**"                { T_POW }
  | word      as lexeme { token_of_word lexeme }
  | hex_const as lexeme { T_INT_LIT (int_of_hex lexeme) }
  | bin_const as lexeme { T_INT_LIT (int_of_bin lexeme) }
  | dec_const as lexeme { T_INT_LIT (int_of_string lexeme) }
  | str_lit   as lexeme { T_STR_LIT lexeme }

{
let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
}
