

%token <int> T_INT_LIT
%token <string> T_STR_LIT
%token <string> T_IDENTIFIER
%token T_NUMBER T_STRING T_BOOLEAN T_ANY 
%token T_IF T_ELSE T_WHILE T_RETURN T_LET T_VAR T_FUNCTION T_TYPEOF
%token T_OPEN_PAR T_CLOSE_PAR T_OPEN_BRA T_CLOSE_BRA T_OPEN_SQR T_CLOSE_SQR 
%token T_COMMA T_DOT T_COLON T_SEMICOLON T_BAR
%token T_PLUS T_MINUS T_MUL T_DIV T_POW
%token T_AND T_OR T_NOT
%token T_LT T_LEQ T_GT T_GEQ T_EQUAL T_DIFF T_EQQ T_NEQQ
%token T_ASSIGN
%token EOL EOF

%left T_BAR
%nonassoc T_OPEN_SQR

%type <Tpscrpt.program>             program
%type <Tpscrpt.instruction>         instr
%type <Tpscrpt.instruction>         compound
%type <Tpscrpt.declaration>         decl
%type <Tpscrpt.expression>          expr
%type <Tpscrpt.instruction list>    instr_list
%type <Tpscrpt.instruction list>    instr_list_empty
%type <Tpscrpt.binding>             binding
%type <Tpscrpt.binding list>        binding_list
%type <Tpscrpt.binding list>        binding_list_empty
%type <Tpscrpt.instruction>         iff
%type <Tpscrpt.type_>               type_
%type <Tpscrpt.type_>               basic_type
%type <Tpscrpt.type_ list>          union_type
%type <Tpscrpt.type_ option>        type_opt
%type <Tpscrpt.object_member>       object_member
%type <Tpscrpt.object_member list>  object_member_list
%type <Tpscrpt.declaration>         function

%start program

%%

program:
  | instr program { (Stmt $1) :: $2 }
  | decl program  { (Decl $1) :: $2 }
  | EOF           { [] }

compound:
  | T_OPEN_BRA T_CLOSE_BRA                              { Compound [] }
  | T_OPEN_BRA instr_list T_CLOSE_BRA                   { Compound $2 }

instr:
  | T_SEMICOLON                                         { Empty }
  | expr T_SEMICOLON                                    { Expr $1 }
  | compound                                            { $1 }
  | T_VAR binding_list T_SEMICOLON                      { VarDecl $2 }
  | T_WHILE T_OPEN_PAR expr T_CLOSE_PAR instr           { While ($3, $5) }
  | T_RETURN expr T_SEMICOLON                           { Return (Some $2) }
  | iff T_SEMICOLON                                     { $1 } 

iff:
  | T_IF T_OPEN_PAR expr T_CLOSE_PAR instr T_ELSE instr { If ($3, $5, Some $7) }
  | T_IF T_OPEN_PAR expr T_CLOSE_PAR instr              { If ($3, $5, None) }

decl:
  | function { $1 }

function:
  | T_FUNCTION T_IDENTIFIER T_OPEN_PAR binding_list_empty T_CLOSE_PAR type_opt T_OPEN_BRA instr_list_empty T_CLOSE_BRA { Func ($2, $4, $6, $8) }

expr:
  | T_INT_LIT     { Intlit $1 }
  | T_STR_LIT     { Strlit $1 }
  | T_IDENTIFIER  { Identifier $1 }

instr_list:
  | instr instr_list { $1 :: $2 }
  | instr            { [$1] }

instr_list_empty:
  | instr_list  { $1 }
  |             { [] }

binding:
  | T_IDENTIFIER type_opt T_ASSIGN expr { $1, $2, Some $4 }
  | T_IDENTIFIER type_opt               { $1, $2,  None }

binding_list:
  | binding                       { [$1] }
  | binding T_COMMA binding_list  { $1 :: $3 }

binding_list_empty:
  | binding_list  { $1 }
  |               { [] }

union_type:
  | union_type T_BAR basic_type { $3 :: $1 }
  | basic_type T_BAR basic_type { [$1; $3] }

type_:
  | basic_type                    { $1 }
  | union_type                    { TypeUnion $1 }
  | type_ T_OPEN_SQR T_CLOSE_SQR  { TypeTab $1 }

basic_type:
  | T_IDENTIFIER                              { TypeIdentifier $1 }
  | T_NUMBER                                  { TypeNumber }
  | T_BOOLEAN                                 { TypeBoolean }
  | T_STRING                                  { TypeString }
  | T_ANY                                     { TypeAny }
  | T_OPEN_BRA object_member_list T_CLOSE_BRA { TypeObject $2 }

type_opt:
  | T_COLON type_ { Some $2 }
  |               { None }

object_member:
  | T_IDENTIFIER T_COLON type_ { $1, $3 }

object_member_list:
  | object_member T_COMMA object_member_list      { $1 :: $3 }
  | object_member T_SEMICOLON object_member_list  { $1 :: $3 }
  | object_member                                 { [$1] }