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
type unopkind = UnOpPlus | UnOpMinus | UnOpTypeof | UnOpNot
type expression =
    IntConst of int
  | FloatConst of float
  | StringConst of string
  | BoolConst of bool
  | Obj of (string * expression) list
  | Tab of expression list
  | Funcall of expression * expression list
  | Unary of unopkind * expression
  | Binary of expression * binopkind * expression
  | LeftMember of left_member
  | Assign of left_member * expression
and left_member =
    Identifier of string
  | Subscript of expression * expression
  | Access of expression * string
  | Expr of expression
type type_ =
    TypeIdentifier of string
  | TypeString
  | TypeBoolean
  | TypeNumber
  | TypeAny
  | TypeTab of type_
  | TypeUnion of type_ list
  | TypeObject of object_member list
  | TypeCte of expression
and object_member = string * type_
type binding = string * type_ option * expression option
type instruction =
    Empty
  | Expr of expression
  | Compound of program_element list
  | VarDecl of binding list
  | If of expression * instruction * instruction option
  | While of expression * instruction
  | Return of expression option
and declaration =
    Alias of string * type_
  | Let of binding list
  | Const of binding list
  | Func of string * binding list * type_ option * program_element list
and program_element = Stmt of instruction | Decl of declaration
type program = program_element list
val prt_aux : ('a -> unit) -> 'a list -> unit
val print_binopkind : binopkind -> unit
val print_unopkind : unopkind -> unit
val print_left_member : left_member -> unit
val print_expression : expression -> unit
val print_object_member : object_member -> unit
val print_type_ : type_ -> unit
val print_binding : string * type_ option * expression option -> unit
val print_instruction : instruction -> unit
val print_declaration : declaration -> unit
val print_prog_elem : program_element -> unit
val print_program : program_element list -> unit
val read_whole_file : string -> string
