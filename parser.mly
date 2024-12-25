

%token <int> T_INT_LIT
%token <string> T_STR_LIT
%token <string> T_IDENTIFIER
%token T_NUMBER T_STRING T_BOOLEAN T_ANY T_IF T_ELSE T_WHILE T_RETURN T_LET T_VAR T_FUNCTION T_TYPEOF
%token T_OPEN_PAR T_CLOSE_PAR T_OPEN_BRA T_CLOSE_BRA T_OPEN_SQR T_CLOSE_SQR T_COMMA T_DOT T_COLON T_SEMICOLON T_BAR
%token T_PLUS T_MINUS T_MUL T_DIV T_LT T_LEQ T_GT T_GEQ T_EQUAL T_DIFF T_EQQ T_NEQQ T_AND T_OR T_ASSIGN T_NOT T_POW
%token EOL EOF

%type <Tpscrpt.program>     program
%type <Tpscrpt.instruction> instr
%type <Tpscrpt.declaration> decl
%type <Tpscrpt.expression>  expr
%type <Tpscrpt.instruction list> instr_list
%type <Tpscrpt.binding> binding
%type <Tpscrpt.binding list> binding_list

%start program

%%

program:
  | instr program { (Stmt $1) :: $2 }
  | decl program { (Decl $1) :: $2 }
  | EOF { [] }

instr:
  | T_SEMICOLON                                         { Empty }
  | expr T_SEMICOLON                                    { Expr $1 }
  | T_OPEN_BRA instr_list T_CLOSE_BRA                   { Compound $2 }
  | T_VAR binding_list T_SEMICOLON                      { VarDecl $2 }
  | T_IF T_OPEN_PAR expr T_CLOSE_PAR instr T_ELSE instr { If ($3, $5, Some $7) }
  | T_IF T_OPEN_PAR expr T_CLOSE_PAR instr              { If ($3, $5, None) }
  | T_WHILE T_OPEN_PAR expr T_CLOSE_PAR instr           { While ($3, $5) }
  | T_RETURN expr T_SEMICOLON                           { Return (Some $2) }

decl:
| T_FUNCTION T_IDENTIFIER T_OPEN_PAR T_CLOSE_PAR T_OPEN_BRA instr_list T_CLOSE_BRA { Func ($2, [], None, $6) }

expr:
  | T_INT_LIT { Intlit $1 }
  | T_STR_LIT { Strlit $1 }
  | T_IDENTIFIER { Identifier $1 }

instr_list:
  | instr instr_list { $1 :: $2 }
  | instr { [$1] }

binding:
  | T_IDENTIFIER T_ASSIGN expr { $1, None, Some $3 }

binding_list:
  | binding { [$1] }
  | binding T_COMMA binding_list { $1 :: $3 }
