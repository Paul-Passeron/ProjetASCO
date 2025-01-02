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

type unopkind = UnOpPlus | UnOpMinus | UnOpTypeof | UnOpNot

type expression =
  | IntConst of int
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
  | Identifier of string
  | Subscript of expression * expression
  | Access of expression * string
  | Expr of expression

type type_ =
  | TypeIdentifier of string
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
  | Empty
  | Expr of expression
  | Compound of program_element list
  | VarDecl of binding list
  | If of expression * instruction * instruction option
  | While of expression * instruction
  | Return of expression option

and declaration =
  | Alias of string * type_
  | Let of binding list
  | Const of binding list
  | Func of string * binding list * type_ option * program_element list

and program_element = Stmt of instruction | Decl of declaration

type program = program_element list

let prt_aux f l =
  if l = [] then ()
  else
    let l2 = List.rev l in
    let h = List.hd l2 in
    let t = List.rev (List.tl l2) in
    List.iter
      (fun x ->
        f x;
        Printf.printf ", ")
      t;
    f h

let print_binopkind = function
  | BinOpPlus -> Printf.printf "BinOpPlus"
  | BinOpMinus -> Printf.printf "BinOpMinus"
  | BinOpMul -> Printf.printf "BinOpMul"
  | BinOpDiv -> Printf.printf "BinOpDiv"
  | BinOpLt -> Printf.printf "BinOpLt"
  | BinOpLeq -> Printf.printf "BinOpLeq"
  | BinOpGt -> Printf.printf "BinOpGt"
  | BinOpGeq -> Printf.printf "BinOpGeq"
  | BinOpEq -> Printf.printf "BinOpEq"
  | BinOpDif -> Printf.printf "BinOpDif"
  | BinOpEqq -> Printf.printf "BinOpEqq"
  | BinOpNeqq -> Printf.printf "BinOpNeqq"
  | BinOpAnd -> Printf.printf "BinOpAnd"
  | BinOpOr -> Printf.printf "BinOpOr"
  | BinOpNot -> Printf.printf "BinOpNot"
  | BinOpPow -> Printf.printf "BinOpPow"

let print_unopkind = function
  | UnOpPlus -> Printf.printf "UnOpPlus"
  | UnOpMinus -> Printf.printf "UnOpMinus"
  | UnOpTypeof -> Printf.printf "UnOpTypeof"
  | UnOpNot -> Printf.printf "UnOpNot"

let rec print_left_member = function
  | Subscript (tab, index) ->
      Printf.printf "(";
      print_expression tab;
      Printf.printf ")[";
      print_expression index;
      Printf.printf "]"
  | Identifier s -> Printf.printf "Identifier(%s)" s
  | Access (e, s) ->
      Printf.printf "(";
      print_expression e;
      Printf.printf ").%s" s
  | Expr e -> print_expression e

and print_expression = function
  | IntConst i -> Printf.printf "IntConst(%d)" i
  | StringConst s -> Printf.printf "StringConst(%s)" s
  | BoolConst b -> Printf.printf "BoolConst(%s)" (if b then "true" else "false")
  | Obj li ->
      Printf.printf "[";
      prt_aux
        (fun (a, b) ->
          Printf.printf "%s ->" a;
          print_expression b)
        li;
      Printf.printf "]"
  | Tab li ->
      Printf.printf "[";
      prt_aux print_expression li;
      Printf.printf "]"
  | Funcall (e, li) ->
      Printf.printf "(";
      print_expression e;
      Printf.printf ")(";
      prt_aux print_expression li;
      Printf.printf ")"
  | Unary (u, e) ->
      print_unopkind u;
      Printf.printf "(";
      print_expression e;
      Printf.printf ")"
  | Binary (l, o, r) ->
      print_binopkind o;
      Printf.printf "(";
      print_expression l;
      Printf.printf ", ";
      print_expression r;
      Printf.printf ")"
  | LeftMember lm -> print_left_member lm
  | Assign (lm, e) ->
      Printf.printf "(";
      print_left_member lm;
      Printf.printf ") = ";
      print_expression e
  | FloatConst f -> Printf.printf "FloatConst(%f)" f

let rec print_object_member = function
  | name, value ->
      Printf.printf "Field(%s, " name;
      print_type_ value;
      Printf.printf ")"

and print_type_ = function
  | TypeIdentifier s -> Printf.printf "TypeIdentifier(%s)" s
  | TypeString -> Printf.printf "TypeString"
  | TypeBoolean -> Printf.printf "TypeBoolean"
  | TypeNumber -> Printf.printf "TypeNumber"
  | TypeAny -> Printf.printf "TypeAny"
  | TypeTab t ->
      Printf.printf "TypeTab(";
      print_type_ t;
      Printf.printf ")"
  | TypeUnion li ->
      Printf.printf "TypeUnion([";
      prt_aux print_type_ li;
      Printf.printf "])"
  | TypeObject li ->
      Printf.printf "TypeUnion([";
      prt_aux print_object_member li;
      Printf.printf "])"
  | TypeCte cte ->
      Printf.printf "TypeCte(";
      print_expression cte;
      Printf.printf ")"

let print_binding = function
  | name, t, e ->
      Printf.printf "Binding(%s" name;
      let () =
        if t <> None then (
          Printf.printf ", Type=";
          print_type_ (Option.get t))
      in
      ();
      let () =
        if e <> None then (
          Printf.printf ", Expression=";
          print_expression (Option.get e))
      in
      ();
      Printf.printf ")"

let rec print_instruction = function
  | Empty -> Printf.printf "Empty"
  | Expr e ->
      Printf.printf "Expr(";
      print_expression e;
      Printf.printf ")"
  | Compound li ->
      Printf.printf "Compound([";
      prt_aux print_prog_elem li;
      Printf.printf "])"
  | VarDecl li ->
      Printf.printf "VarDecl([";
      prt_aux print_binding li;
      Printf.printf "])"
  | If (e, iff, elze) ->
      Printf.printf "If(";
      print_expression e;
      Printf.printf ", ";
      print_instruction iff;
      if elze <> None then (
        Printf.printf ", ";
        print_instruction (Option.get elze))
      else ()
  | While (e, i) ->
      Printf.printf "While(";
      print_expression e;
      Printf.printf ", ";
      print_instruction i;
      Printf.printf ")"
  | Return expr ->
      Printf.printf "Return(";
      if expr <> None then print_expression (Option.get expr) else ();
      Printf.printf ")"

and print_declaration = function
  | Alias (s, t) ->
      Printf.printf "Alias(%s, " s;
      print_type_ t;
      Printf.printf ")"
  | Let li ->
      Printf.printf "Let([";
      prt_aux print_binding li;
      Printf.printf "])"
  | Const li ->
      Printf.printf "Const([";
      prt_aux print_binding li;
      Printf.printf "])"
  | Func (name, bli, topt, ili) ->
      Printf.printf "Func(%s, [" name;
      prt_aux print_binding bli;
      Printf.printf "]";
      if topt <> None then (
        Printf.printf ", ";
        print_type_ (Option.get topt));
      Printf.printf ", [";
      prt_aux print_prog_elem ili;
      Printf.printf "])"

and print_prog_elem = function
  | Stmt i ->
      Printf.printf "Stmt(";
      print_instruction i;
      Printf.printf ")"
  | Decl d ->
      Printf.printf "Decl(";
      print_declaration d;
      Printf.printf ")"

let print_program =
  List.iter (fun x ->
      print_prog_elem x;
      Printf.printf "\n")

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
