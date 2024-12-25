%{
    open Tpscrpt
%}


%token <int> T_INT_LIT
%token <string> T_STR_LIT
%token <string> T_IDENTIFIER
%token T_NUMBER T_STRING T_BOOLEAN T_IF T_ELSE T_WHILE T_RETURN T_LET T_VAR T_FUNCTION T_TYPEOF
%token T_OPEN_PAR T_CLOSE_PAR T_OPEN_BRA T_CLOSE_BRA T_OPEN_SQR T_CLOSE_SQR T_COMMA T_DOT T_COLON T_SEMICOLON
%token T_PLUS T_MINUS T_MUL T_DIV T_LT T_LEQ T_GT T_GEQ T_EQUAL T_DIFF T_EQQ T_NEQQ T_AND T_OR T_ASSIGN T_NOT T_POW
%token EOL EOF
