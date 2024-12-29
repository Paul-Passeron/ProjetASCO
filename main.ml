open Lexer
open Tpscrpt
open Parser
open Tpscrpt



let print_token t = let () = match t with
| T_INT_LIT d -> Printf.printf "T_INT_LIT %d" d
| T_STR_LIT s -> Printf.printf "T_STR_LIT %s" s
| T_BOOL_LIT b -> Printf.printf "T_STR_LIT %s" (if b then "true" else "false")
| T_IDENTIFIER s -> Printf.printf "T_IDENTIFIER %s" s
| T_NUMBER -> Printf.printf "T_NUMBER"
| T_STRING -> Printf.printf "T_STRING"
| T_BOOLEAN -> Printf.printf "T_BOOLEAN"
| T_ANY -> Printf.printf "T_ANY"
| T_IF -> Printf.printf "T_IF"
| T_ELSE -> Printf.printf "T_ELSE"
| T_WHILE -> Printf.printf "T_WHILE"
| T_RETURN -> Printf.printf "T_RETURN"
| T_LET -> Printf.printf "T_LET"
| T_VAR -> Printf.printf "T_VAR"
| T_FUNCTION -> Printf.printf "T_FUNCTION"
| T_TYPEOF -> Printf.printf "T_TYPEOF"
| T_OPEN_PAR -> Printf.printf "T_OPEN_PAR"
| T_CLOSE_PAR -> Printf.printf "T_CLOSE_PAR"
| T_OPEN_BRA -> Printf.printf "T_OPEN_BRA"
| T_CLOSE_BRA -> Printf.printf "T_CLOSE_BRA"
| T_OPEN_SQR -> Printf.printf "T_OPEN_SQR"
| T_CLOSE_SQR -> Printf.printf "T_CLOSE_SQR"
| T_COMMA -> Printf.printf "T_COMMA"
| T_DOT -> Printf.printf "T_DOT"
| T_COLON -> Printf.printf "T_COLON"
| T_SEMICOLON -> Printf.printf "T_SEMICOLON"
| T_BAR -> Printf.printf "T_BAR"
| T_PLUS -> Printf.printf "T_PLUS"
| T_MINUS -> Printf.printf "T_MINUS"
| T_MUL -> Printf.printf "T_MUL"
| T_DIV -> Printf.printf "T_DIV"
| T_LT -> Printf.printf "T_LT"
| T_LEQ -> Printf.printf "T_LEQ"
| T_GT -> Printf.printf "T_GT"
| T_GEQ -> Printf.printf "T_GEQ"
| T_EQ -> Printf.printf "T_EQUAL"
| T_DIFF -> Printf.printf "T_DIFF"
| T_EQQ -> Printf.printf "T_EQQ"
| T_NEQQ -> Printf.printf "T_NEQQ"
| T_AND -> Printf.printf "T_AND"
| T_OR -> Printf.printf "T_OR"
| T_ASSIGN -> Printf.printf "T_ASSIGN"
| T_NOT -> Printf.printf "T_NOT"
| T_POW -> Printf.printf "T_POW"
| EOL -> Printf.printf "EOL"
| EOF -> Printf.printf "EOF"
| T_FLOAT_LIT f -> Printf.printf "T_FLOAT_LIT %f" f
| T_TYPE -> Printf.printf "T_TYPE"
| T_CONST -> Printf.printf "T_CONST"
in Printf.printf "\n"

let rec read l =
  try
    let res = token l in
    if res = EOF then () else
      let _ = print_token res in
      read l
  with _ -> ()


type cont_elem =
  | Function of string
  | Var      of string
  | Const    of string
  | Type     of string
  
let is_function_in_context e l = List.exists (fun x -> match x with Function s -> s = e | _ -> false) l
let is_var_in_context e l = List.exists (fun x -> match x with Var s -> s = e | _ -> false) l
let is_const_in_context e l = List.exists (fun x -> match x with Const s -> s = e | _ -> false) l
let is_type_in_context e l = List.exists (fun x -> match x with Type s -> s = e | _ -> false) l
let is_decl_in_context e l = List.exists (fun x -> match x with Var s | Const s -> s = e | _ -> false) l

type kind = 
  | K_VAR
  | K_ASSIGN
  | K_FUN
let rec check_scope ast = 
  let rec check_left_member l context kind = match l with 
    | Identifier s -> (
      match kind with 
      | K_VAR -> is_decl_in_context s context
      | K_ASSIGN -> is_var_in_context s context
      | K_FUN -> is_function_in_context s context
    )
    | Subscript (e, i) -> check_scope_expr e context kind && check_scope_expr i context K_VAR
    | Access (e, _) -> check_scope_expr e context kind
    | Expr e -> check_scope_expr e context kind

    and check_scope_expr e context kind = match e with
    | IntConst _ | FloatConst _ | StringConst _ | BoolConst _ -> true
    | Obj li -> List.for_all (fun (_, e) -> check_scope_expr e (context) kind) li 
    | Tab li -> List.for_all (fun x-> check_scope_expr x context kind) li 
    | Funcall (e, li) -> check_scope_expr e context K_FUN && List.for_all (fun e -> check_scope_expr e context K_VAR) li
    | Unary (_, e) -> check_scope_expr e context kind
    | Binary (a, _, b) -> check_scope_expr a context kind && check_scope_expr b context kind
    | LeftMember l -> check_left_member l context kind
    | Assign (l, e) -> check_scope_expr e context kind && check_left_member l context K_ASSIGN
in
  let rec check_scope_rec ast context = match ast with
    | (Stmt s) :: q -> (
      match s with
      | Empty -> check_scope_rec q context
      | Expr e -> check_scope_expr e context K_VAR && check_scope_rec q context
      | Compound li -> check_scope_rec li context && check_scope_rec q context
      | VarDecl bl -> 
        let new_context = List.fold_right (fun (s, _, _) acc -> (Var s)::acc) bl context in
        List.for_all(fun (_, _, e_opt) -> match e_opt with None -> true | Some e -> check_scope_expr e context K_VAR) bl && check_scope_rec q new_context
      | If (e, i, e_opt) -> check_scope_expr e context K_VAR && check_scope_rec [Stmt i] context && (if e_opt <> None then check_scope_rec [Stmt (Option.get e_opt)] context else true) && check_scope_rec q context
      | While (e, i) -> check_scope_expr e context K_VAR && check_scope_rec [Stmt i] context && check_scope_rec q context
      | Return e_opt -> (if e_opt <> None then check_scope_expr (Option.get e_opt) context K_VAR else true) && check_scope_rec q context
    ) 
    | (Decl d) :: q -> ( 
      match d with
      | Alias (s, _) -> check_scope_rec q ((Type s) :: context)
      | Let li ->
        let new_context = List.fold_right (fun (s, _, _) acc -> (Var s)::acc) li context in
        List.for_all(fun (_, _, e_opt) -> match e_opt with None -> true | Some e -> check_scope_expr e context K_VAR) li && check_scope_rec q new_context
      | Const li -> 
        let new_context = List.fold_right (fun (s, _, _) acc -> (Const s)::acc) li context in
        List.for_all(fun (_, _, e_opt) -> match e_opt with None -> true | Some e -> check_scope_expr e context K_VAR) li && check_scope_rec q new_context
      | Func (id, pli, t_opt, ili) -> 
        let added = (Function id)::context in
        let new_context = (List.fold_right (fun (s, _, _) acc -> (Function s)::acc) pli added) in
        check_scope_rec ili new_context && check_scope_rec q added
    )
    | [] -> true

   in
  check_scope_rec ast []



let () = Printf.printf "\nTokens:\n"
let contents = read_whole_file "test.ts"
let lexbuf = Lexing.from_string contents
let _ = read lexbuf
let () = Printf.printf "\nAst:\n"
let lexbuf = Lexing.from_string contents 
let ast = program token lexbuf
let () = print_program ast
let () = Printf.printf "\nScope:\n"
let () = Printf.printf "%s\n" (if check_scope ast then "true" else "false")
