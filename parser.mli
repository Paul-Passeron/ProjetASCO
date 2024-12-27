type token =
    T_INT_LIT of int
  | T_STR_LIT of string
  | T_IDENTIFIER of string
  | T_NUMBER
  | T_STRING
  | T_BOOLEAN
  | T_ANY
  | T_IF
  | T_ELSE
  | T_WHILE
  | T_RETURN
  | T_LET
  | T_VAR
  | T_FUNCTION
  | T_TYPEOF
  | T_OPEN_PAR
  | T_CLOSE_PAR
  | T_OPEN_BRA
  | T_CLOSE_BRA
  | T_OPEN_SQR
  | T_CLOSE_SQR
  | T_COMMA
  | T_DOT
  | T_COLON
  | T_SEMICOLON
  | T_BAR
  | T_PLUS
  | T_MINUS
  | T_MUL
  | T_DIV
  | T_POW
  | T_AND
  | T_OR
  | T_NOT
  | T_LT
  | T_LEQ
  | T_GT
  | T_GEQ
  | T_EQUAL
  | T_DIFF
  | T_EQQ
  | T_NEQQ
  | T_ASSIGN
  | EOL
  | EOF
val yytransl_const : int array
val yytransl_block : int array
val yylhs : string
val yylen : string
val yydefred : string
val yydgoto : string
val yysindex : string
val yyrindex : string
val yygindex : string
val yytablesize : int
val yytable : string
val yycheck : string
val yynames_const : string
val yynames_block : string
val yyact : (Parsing.parser_env -> Obj.t) array
val yytables : Parsing.parse_tables
val program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Tpscrpt.program
