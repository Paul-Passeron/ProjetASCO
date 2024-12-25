type binopkind =
  | BinOpPlus
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

type unopkind =
  | UnOpPlus
  | UnOpMinus
  | UnOpTypeof

type expression =
  | Binop       of expression * binopkind * expression
  | Unary       of unopkind * expression
  | Intlit      of int
  | Strlit      of string
  | Identifier of string

type type_ =
  | TypeIdentifier
  | TypeString
  | TypeBoolean
  | TypeNumber
  | TypeAny
  | TypeTab     of type_
  | TypeUnion   of type_ list
  | TypeObject  of object_member list

and object_member = string * type_

type binding = string * type_ option * expression option

type instruction =
  | Empty
  | Expr      of expression
  | Compound  of instruction list
  | VarDecl   of binding list
  | If        of expression * instruction * instruction option
  | While     of expression * instruction
  | Return    of expression option

type declaration =
  | Alias of string * type_
  | Let   of binding list
  | Const of binding list
  | Func  of string * binding list * type_ option * instruction list

type program_element =
  | Stmt of instruction
  | Decl of declaration

type program = program_element list

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let _ = print_endline "Hello, World !"
