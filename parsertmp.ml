type token =
  | T_INT_LIT of int
  | T_STR_LIT of string
  | T_IDENTIFIER of string
  | T_NUMBER
  | T_STRING
  | T_BOOLEAN
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
  | T_PLUS
  | T_MINUS
  | T_MUL
  | T_DIV
  | T_LT
  | T_LEQ
  | T_GT
  | T_GEQ
  | T_EQUAL
  | T_DIFF
  | T_EQQ
  | T_NEQQ
  | T_AND
  | T_OR
  | T_ASSIGN
  | T_NOT
  | T_POW
  | EOL
  | EOF

let int_of_hex = int_of_string
let int_of_bin = int_of_string
