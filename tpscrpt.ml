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
  | TypeIdentifier  of string
  | TypeString
  | TypeBoolean
  | TypeNumber
  | TypeAny
  | TypeTab         of type_
  | TypeUnion       of type_ list
  | TypeObject      of object_member list

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

let print_binopkind = function
  | BinOpPlus -> Printf.printf "BinOpPlus\n"
  | BinOpMinus -> Printf.printf "BinOpMinus\n"
  | BinOpMul -> Printf.printf "BinOpMul\n"
  | BinOpDiv -> Printf.printf "BinOpDiv\n"
  | BinOpLt -> Printf.printf "BinOpLt\n"
  | BinOpLeq -> Printf.printf "BinOpLeq\n"
  | BinOpGt -> Printf.printf "BinOpGt\n"
  | BinOpGeq -> Printf.printf "BinOpGeq\n"
  | BinOpEq -> Printf.printf "BinOpEq\n"
  | BinOpDif -> Printf.printf "BinOpDif\n"
  | BinOpEqq -> Printf.printf "BinOpEqq\n"
  | BinOpNeqq -> Printf.printf "BinOpNeqq\n"
  | BinOpAnd -> Printf.printf "BinOpAnd\n"
  | BinOpOr -> Printf.printf "BinOpOr\n"
  | BinOpNot -> Printf.printf "BinOpNot\n"
  | BinOpPow -> Printf.printf "BinOpPow\n"

let print_unopkind = function
  | UnOpPlus -> Printf.printf "UnOpPlus\n"
  | UnOpMinus -> Printf.printf "UnOpMinus\n"
  | UnOpTypeof -> Printf.printf "UnOpTypeof\n"

let rec print_expression = function
  | Binop (lhs, op, rhs) -> Printf.printf "Binop("; print_expression lhs; Printf.printf ", "; print_binopkind op; Printf.printf ", "; print_expression rhs; Printf.printf ")"
  | Unary (uop, operand) -> Printf.printf "Unary("; print_unopkind uop; Printf.printf ", "; print_expression operand;  Printf.printf ")"
  | Intlit d -> Printf.printf "Intlit(%d)" d
  | Strlit s -> Printf.printf "Strlit(%s)" s
  | Identifier s -> Printf.printf "Identifier(%s)" s

let rec print_object_member = function
| (name, value) -> Printf.printf "Field(%s, "  name ; print_type_ value; Printf.printf ")"

and print_type_ = function
  | TypeIdentifier s -> Printf.printf "TypeIdentifier(%s)" s
  | TypeString -> Printf.printf "TypeString"
  | TypeBoolean -> Printf.printf "TypeBoolean"
  | TypeNumber -> Printf.printf "TypeNumber"
  | TypeAny  -> Printf.printf "TypeAny"
  | TypeTab t -> Printf.printf "TypeTab("; print_type_ t; Printf.printf ")"
  | TypeUnion   li -> Printf.printf "TypeUnion(["; List.iter (fun x -> print_type_ x; Printf.printf ", ") li; Printf.printf "])"
  | TypeObject  li -> Printf.printf "TypeUnion(["; List.iter (fun x -> print_object_member x; Printf.printf ", ") li; Printf.printf "])"

let print_binding = 
  function
  (name, t, e) -> 
    (
      Printf.printf "Binding(%s" name;
      let () = if t <> None then (
        Printf.printf ", Type=";
        print_type_ (Option.get t);
      ) in ();
      let () = if e <> None then (
        Printf.printf ", Expression=";
        print_expression (Option.get e);
      ) in ();
      Printf.printf ")"
    )

  let rec print_instruction = function
  | Empty -> Printf.printf "Empty"
  | Expr e -> Printf.printf "Expr("; print_expression (e); Printf.printf ")"
  | Compound li -> Printf.printf "Compound(["; List.iter (fun x -> print_instruction x; Printf.printf ",") li ; Printf.printf "])"
  | VarDecl li -> Printf.printf "VarDecl(["; List.iter (fun x -> print_binding x; Printf.printf ",") li; Printf.printf "])"
  | If (e, iff, elze) -> 
    Printf.printf "If("; print_expression e; Printf.printf ", "; print_instruction iff; if elze <> None then ( Printf.printf ", "; print_instruction (Option.get elze)) else ()

  | While (e, i) -> Printf.printf "While("; print_expression e; Printf.printf ", "; print_instruction i; Printf.printf ")"
  | Return expr -> Printf.printf "Return("; if expr <> None then (print_expression (Option.get expr)) else (); Printf.printf ")"

let print_declaration = function
| Alias (s, t) -> Printf.printf "Alias(%s, " s; print_type_ t; Printf.printf ")"
| Let   li -> Printf.printf "Let(["; List.iter (fun x -> print_binding x; Printf.printf ", "; ) li; Printf.printf "])"
| Const li -> Printf.printf "Const(["; List.iter (fun x -> print_binding x; Printf.printf ", "; ) li; Printf.printf "])"
| Func (name, bli, topt, ili) -> (
  Printf.printf "Func(%s, [" name;
  List.iter (fun x -> print_binding x; Printf.printf ", ") bli;
  if topt <> None then print_type_ (Option.get topt);
  Printf.printf "], [";
  List.iter (fun x -> print_instruction x; Printf.printf ", ") ili;
  Printf.printf "])"
)

let print_prog_elem = function
    | Stmt i -> Printf.printf "Stmt("; print_instruction i; Printf.printf ")"
    | Decl d -> Printf.printf "Decl("; print_declaration (d); Printf.printf ")"

let print_program = List.iter (fun x -> print_prog_elem x; Printf.printf "\n")

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
