type token =
  | T_INT_LIT of (
# 3 "parser.mly"
        int
# 6 "parser.mli"
)
  | T_FLOAT_LIT of (
# 4 "parser.mly"
        float
# 11 "parser.mli"
)
  | T_STR_LIT of (
# 5 "parser.mly"
        string
# 16 "parser.mli"
)
  | T_IDENTIFIER of (
# 6 "parser.mly"
        string
# 21 "parser.mli"
)
  | T_BOOL_LIT of (
# 7 "parser.mly"
        bool
# 26 "parser.mli"
)
  | T_NUMBER
  | T_STRING
  | T_BOOLEAN
  | T_ANY
  | T_TYPE
  | T_IF
  | T_ELSE
  | T_WHILE
  | T_RETURN
  | T_LET
  | T_VAR
  | T_FUNCTION
  | T_TYPEOF
  | T_CONST
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
  | T_EQ
  | T_DIFF
  | T_EQQ
  | T_NEQQ
  | T_ASSIGN
  | EOL
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tpscrpt.program
