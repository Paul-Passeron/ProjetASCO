type binopkind =
    BinOpPlus
  | BinOpMinus
  | BinOpMul
  | BinOpDiv
  | BinOpLt
  | BinOpLeq
  | BinOpGt
  | BinOpGeq
  | BinOpEq
  | BinOpDif
  | BinOpEqq
  | BinOpNeqq
  | BinOpAnd
  | BinOpOr
  | BinOpNot
  | BinOpPow
type unopkind = UnOpPlus | UnOpMinus | UnOpTypeof
type expression =
    Binop of expression * binopkind * expression
  | Unary of unopkind * expression
  | Intlit of int
  | Strlit of string
  | Identifier of string
type type_ =
    TypeIdentifier of string
  | TypeString
  | TypeBoolean
  | TypeNumber
  | TypeAny
  | TypeTab of type_
  | TypeUnion of type_ list
  | TypeObject of object_member list
and object_member = string * type_
type binding = string * type_ option * expression option
type instruction =
    Empty
  | Expr of expression
  | Compound of instruction list
  | VarDecl of binding list
  | If of expression * instruction * instruction option
  | While of expression * instruction
  | Return of expression option
type declaration =
    Alias of string * type_
  | Let of binding list
  | Const of binding list
  | Func of string * binding list * type_ option * instruction list
type program_element = Stmt of instruction | Decl of declaration
type program = program_element list
val prt_aux : ('a -> 'b) -> 'a list -> 'b
val print_binopkind : binopkind -> unit
val print_unopkind : unopkind -> unit
val print_expression : expression -> unit
val print_object_member : object_member -> unit
val print_type_ : type_ -> unit
val print_binding : string * type_ option * expression option -> unit
val print_instruction : instruction -> unit
val print_declaration : declaration -> unit
val print_prog_elem : program_element -> unit
val print_program : program_element list -> unit
val read_whole_file : string -> string
