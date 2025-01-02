%token <int> T_INT_LIT
%token <float> T_FLOAT_LIT
%token <string> T_STR_LIT
%token <string> T_IDENTIFIER
%token <bool> T_BOOL_LIT
%token T_NUMBER T_STRING T_BOOLEAN T_ANY T_TYPE
%token T_IF T_ELSE T_WHILE T_RETURN T_LET T_VAR T_FUNCTION T_TYPEOF T_CONST
%token T_OPEN_PAR T_CLOSE_PAR T_OPEN_BRA T_CLOSE_BRA T_OPEN_SQR T_CLOSE_SQR 
%token T_COMMA T_DOT T_COLON T_SEMICOLON T_BAR
%token T_PLUS T_MINUS T_MUL T_DIV T_POW
%token T_AND T_OR T_NOT
%token T_LT T_LEQ T_GT T_GEQ T_EQ T_DIFF T_EQQ T_NEQQ
%token T_ASSIGN
%token EOL EOF

%nonassoc dummy3
%nonassoc dummy2
%nonassoc dummy
%nonassoc T_ELSE
%nonassoc T_OPEN_SQR
%nonassoc T_BAR

%left T_OPEN_PAR
%left T_DOT
%left T_ASSIGN
%left T_OR
%left T_AND
%left T_EQ T_DIFF T_EQQ T_NEQQ
%left T_LT T_LEQ T_GT T_GEQ
%left T_PLUS T_MINUS
%left T_MUL T_DIV
%right T_POW
%right T_NOT T_TYPEOF

%type <Tpscrpt.program>                     program
%type <Tpscrpt.instruction>                 instr
%type <Tpscrpt.instruction>                 compound
%type <Tpscrpt.instruction>                 iff
%type <Tpscrpt.declaration>                 decl
%type <Tpscrpt.declaration>                 func
%type <Tpscrpt.expression>                  expr
%type <Tpscrpt.left_member>                 left_member
%type <Tpscrpt.binding>                     binding
%type <Tpscrpt.binding list>                binding_list
%type <Tpscrpt.binding list>                binding_list_empty
%type <Tpscrpt.type_>                       type_
%type <Tpscrpt.type_>                       basic_type
%type <Tpscrpt.type_ list>                  union_type
%type <Tpscrpt.type_ option>                type_opt
%type <Tpscrpt.object_member>               object_member
%type <Tpscrpt.object_member list>          object_member_list
%type <Tpscrpt.expression>                  object_expr
%type <string * Tpscrpt.expression>         object_expr_member
%type <(string * Tpscrpt.expression) list>  object_expr_member_list
%type <Tpscrpt.expression list>             tab_expr_list

%start program

%%

program:
  | instr program                   { (Stmt $1) :: $2 }
  | decl program                    { (Decl $1) :: $2 }
  | EOF                             { [] }
  | T_OPEN_BRA T_CLOSE_BRA program  { Stmt (Compound []) :: $3 }


compound:
  | T_OPEN_BRA instr_list T_CLOSE_BRA                   { Compound $2 }

iff: 
  | T_OPEN_PAR expr T_CLOSE_PAR instr T_ELSE instr { If ($2, $4, Some $6) }
  | T_OPEN_PAR expr T_CLOSE_PAR instr %prec dummy  { If ($2, $4, None) }
  | T_OPEN_PAR expr T_CLOSE_PAR error                                          { Printf.printf "Syntax Error: Invalid if statement. Maybe there is an empty compound statement. If you want the if to not have any effect, use the empty statement ';'.\n"; exit 1 }

instr:
  | T_SEMICOLON                                         { Empty }
  | compound                                            { $1 }
  | T_VAR binding_list T_SEMICOLON                      { VarDecl $2 }
  | T_WHILE T_OPEN_PAR expr T_CLOSE_PAR instr           { While ($3, $5) }
  | T_RETURN expr T_SEMICOLON                           { Return (Some $2) }
  | T_RETURN T_SEMICOLON                                { Return None }
  | expr_aux T_SEMICOLON                                { Expr $1 }
  | T_IF iff                                            { $2 }



func: 
  | T_IDENTIFIER T_OPEN_PAR binding_list_empty T_CLOSE_PAR type_opt T_OPEN_BRA instr_list_empty T_CLOSE_BRA
    { Func ($1, $3, $5, $7) }
  | T_IDENTIFIER T_OPEN_PAR binding_list_empty T_CLOSE_PAR error
    { Printf.printf "Syntax Error: Expected type or function body after function declaration.\n"; exit 1 }
  | T_IDENTIFIER T_OPEN_PAR error
    { Printf.printf "Syntax Error: Unmatched '(' after function arguments.\n"; exit 1 }
  | T_IDENTIFIER error
    { Printf.printf "Syntax Error: Expected '(' after function name.\n"; exit 1 }
  | error
    { Printf.printf "Syntax Error: Expected function name after 'function' keyword.\n" ; exit 1 }

decl:
  | T_FUNCTION func                     { $2 }
  | T_TYPE T_IDENTIFIER T_ASSIGN type_  { Alias ($2, $4) }
  | T_LET binding_list T_SEMICOLON      { Let $2 }
  | T_CONST binding_list T_SEMICOLON    { Const $2 }

tab_expr_list:
  | expr T_COMMA tab_expr_list  { $1 :: $3 }
  | expr                        { [$1] }
  |                             { [] }

object_expr_member:
  T_IDENTIFIER T_COLON expr { $1, $3 }

object_expr_member_list:
  | object_expr_member T_COMMA object_expr_member_list  { $1 :: $3 }
  | object_expr_member T_COMMA                          { [$1] }
  | object_expr_member                                  { [$1] }
  
object_expr:
  | T_OPEN_BRA object_expr_member_list T_CLOSE_BRA  { Obj $2 }

expr:
  | expr_aux                           { $1 }
  | T_OPEN_BRA T_CLOSE_BRA %prec dummy { Obj [] }

expr_aux:
  | T_INT_LIT                                       { IntConst $1 }
  | T_FLOAT_LIT                                     { FloatConst $1 }
  | T_STR_LIT                                       { StringConst $1 }
  | T_BOOL_LIT {BoolConst $1}
  | object_expr                                     { $1 }
  | unop                                            { $1 }
  | binop                                           { $1 }
  | left_member                                     { LeftMember $1 }
  | T_OPEN_SQR tab_expr_list T_CLOSE_SQR            { Tab $2 }
  | left_member T_ASSIGN expr                       { Assign ($1, $3) } 
  | expr T_OPEN_PAR tab_expr_list T_CLOSE_PAR       { Funcall ($1, $3) }

left_member:
  | T_IDENTIFIER                      { Identifier $1 }
  | expr T_OPEN_SQR expr T_CLOSE_SQR  { Subscript ($1, $3) }
  | expr T_DOT T_IDENTIFIER           { Access ($1, $3) }
  | T_OPEN_PAR expr T_CLOSE_PAR       { Expr $2 }


instr_list:
  | instr instr_list { (Stmt $1) :: $2 }
  | decl instr_list  { (Decl $1) :: $2 }
  | instr            { [Stmt $1] }
  | decl             { [Decl $1] }

instr_list_empty:
  | instr_list  { $1 }
  |             { [] }

binding:
  | T_IDENTIFIER type_opt T_ASSIGN expr { $1, $2, Some $4 }
  | T_IDENTIFIER type_opt               { $1, $2, None }

binding_list:
  | binding                       { [$1] }
  | binding T_COMMA               { [$1] }
  | binding T_COMMA binding_list  { $1 :: $3 }

binding_list_empty:
  | binding_list  { $1 }
  |               { [] }

union_type:
  | union_type T_BAR basic_type { $3 :: $1 }
  | basic_type T_BAR basic_type { [$1; $3] }

type_:
  | basic_type                        { $1 }
  | union_type                        { TypeUnion $1 }
  | type_ T_OPEN_SQR T_CLOSE_SQR      { TypeTab $1 }

basic_type:
  | T_IDENTIFIER                              { TypeIdentifier $1 }
  | T_NUMBER                                  { TypeNumber }
  | T_BOOLEAN                                 { TypeBoolean }
  | T_STRING                                  { TypeString }
  | T_ANY                                     { TypeAny }
  | T_OPEN_BRA object_member_list T_CLOSE_BRA { TypeObject $2 }
  | T_OPEN_PAR type_ T_CLOSE_PAR              { $2 }
  | T_INT_LIT                                 { TypeCte (IntConst $1) }
  | T_FLOAT_LIT                               { TypeCte (FloatConst $1) }
  | T_BOOL_LIT                                { TypeCte (BoolConst $1) }
  | T_STR_LIT                                 { TypeCte (StringConst $1) }

type_opt:
  | T_COLON type_ { Some $2 }
  |               { None }

object_member:
  | T_IDENTIFIER T_COLON type_  { $1, $3 }
  | T_IDENTIFIER                { $1, TypeAny }

object_member_list:
  | object_member T_COMMA object_member_list      { $1 :: $3 }
  | object_member T_SEMICOLON object_member_list  { $1 :: $3 }
  | object_member T_COMMA                         { [$1] }
  | object_member T_SEMICOLON                     { [$1] }
  | object_member                                 { [$1] }

unop: 
  | T_NOT expr     {Unary (UnOpNot, $2) }
  | T_PLUS expr    {Unary (UnOpPlus, $2) }
  | T_MINUS expr   {Unary (UnOpMinus, $2) }
  | T_TYPEOF expr  {Unary (UnOpTypeof, $2) }

binop:
  | expr T_PLUS expr  { Binary ($1, BinOpPlus, $3) }
  | expr T_MINUS expr { Binary ($1, BinOpMinus, $3) }
  | expr T_MUL expr   { Binary ($1, BinOpMul, $3) }
  | expr T_DIV expr   { Binary ($1, BinOpDiv, $3) }
  | expr T_LT expr    { Binary ($1, BinOpLt, $3) }
  | expr T_LEQ expr   { Binary ($1, BinOpLeq, $3) }
  | expr T_GT expr    { Binary ($1, BinOpGt, $3) }
  | expr T_GEQ expr   { Binary ($1, BinOpGeq, $3) }
  | expr T_EQ expr    { Binary ($1, BinOpEq, $3) }
  | expr T_DIFF expr  { Binary ($1, BinOpDif, $3) }
  | expr T_EQQ expr   { Binary ($1, BinOpEqq, $3) }
  | expr T_NEQQ expr  { Binary ($1, BinOpNeqq, $3) }
  | expr T_AND expr   { Binary ($1, BinOpAnd, $3) }
  | expr T_OR expr    { Binary ($1, BinOpOr, $3) }
  | expr T_NOT expr   { Binary ($1, BinOpNot, $3) }
  | expr T_POW expr   { Binary ($1, BinOpPow, $3) }