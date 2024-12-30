open Lexer
open Tpscrpt
open Parser
open Tpscrpt

let print_token t =
  let () =
    match t with
    | T_INT_LIT d -> Printf.printf "T_INT_LIT %d" d
    | T_STR_LIT s -> Printf.printf "T_STR_LIT %s" s
    | T_BOOL_LIT b ->
        Printf.printf "T_STR_LIT %s" (if b then "true" else "false")
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
  in
  Printf.printf "\n"

let rec read l =
  try
    let res = token l in
    if res = EOF then ()
    else
      let _ = print_token res in
      read l
  with _ -> ()

type cont_elem =
  | Function of string
  | Var of string
  | Const of string
  | Type of string

let is_function_in_context e l =
  let res =
    List.exists (fun x -> match x with Function s -> s = e | _ -> false) l
  in
  if not res then
    Printf.printf "Error: %s is not a function in this context\n" e;
  res

let is_var_in_context e l =
  let res =
    List.exists (fun x -> match x with Var s -> s = e | _ -> false) l
  in
  if not res then Printf.printf "Error: mutable variable %s not in context\n" e;
  res

let is_const_in_context e l =
  List.exists (fun x -> match x with Const s -> s = e | _ -> false) l

let is_type_in_context e l =
  let res =
    List.exists (fun x -> match x with Type s -> s = e | _ -> false) l
  in
  if not res then Printf.printf "Error: type %s not in context\n" e;
  res

let is_decl_in_context e l =
  let res =
    List.exists
      (fun x -> match x with Var s | Const s -> s = e | _ -> false)
      l
  in
  if not res then Printf.printf "Error: %s is not defined in this context\n" e;
  res

type kind = K_VAR | K_ASSIGN | K_FUN

let rec is_cte exp =
  match exp with
  | FloatConst _ | StringConst _ | BoolConst _ | IntConst _ -> true
  | Assign _ | LeftMember _ | Funcall _ | Tab _ -> false
  | Obj l -> List.for_all (fun (_, x) -> is_cte x) l
  | Unary (_, e) -> is_cte e
  | Binary (a, _, b) -> is_cte a && is_cte b

let rec check_scope ast =
  let rec check_type_scope ast context =
    match ast with
    | TypeIdentifier s -> is_type_in_context s context
    | TypeBoolean | TypeAny | TypeNumber | TypeObject _ | TypeString -> true
    | TypeTab t -> check_type_scope t context
    | TypeUnion l -> List.for_all (fun x -> check_type_scope x context) l
    | TypeCte e -> is_cte e
  and check_left_member l context kind =
    match l with
    | Identifier s -> (
        match kind with
        | K_VAR -> is_decl_in_context s context
        | K_ASSIGN -> is_var_in_context s context
        | K_FUN -> is_function_in_context s context)
    | Subscript (e, i) ->
        check_scope_expr e context kind && check_scope_expr i context K_VAR
    | Access (e, _) -> check_scope_expr e context kind
    | Expr e -> check_scope_expr e context kind
  and check_scope_expr e context kind =
    match e with
    | IntConst _ | FloatConst _ | StringConst _ | BoolConst _ -> true
    | Obj li -> List.for_all (fun (_, e) -> check_scope_expr e context kind) li
    | Tab li -> List.for_all (fun x -> check_scope_expr x context kind) li
    | Funcall (e, li) ->
        check_scope_expr e context K_FUN
        && List.for_all (fun e -> check_scope_expr e context K_VAR) li
    | Unary (_, e) -> check_scope_expr e context kind
    | Binary (a, _, b) ->
        check_scope_expr a context kind && check_scope_expr b context kind
    | LeftMember l -> check_left_member l context kind
    | Assign (l, e) ->
        check_scope_expr e context kind && check_left_member l context K_ASSIGN
  in
  let rec check_scope_rec ast context =
    match ast with
    | Stmt s :: q -> (
        match s with
        | Empty -> check_scope_rec q context
        | Expr e ->
            check_scope_expr e context K_VAR && check_scope_rec q context
        | Compound li -> check_scope_rec li context && check_scope_rec q context
        | VarDecl bl ->
            let new_context =
              List.fold_right (fun (s, _, _) acc -> Var s :: acc) bl context
            in
            List.for_all
              (fun (_, t_opt, e_opt) ->
                (match e_opt with
                | None -> true
                | Some e -> check_scope_expr e context K_VAR)
                &&
                match t_opt with
                | None -> true
                | Some t -> check_type_scope t context)
              bl
            && check_scope_rec q new_context
        | If (e, i, e_opt) ->
            check_scope_expr e context K_VAR
            && check_scope_rec [ Stmt i ] context
            && (if e_opt <> None then
                  check_scope_rec [ Stmt (Option.get e_opt) ] context
                else true)
            && check_scope_rec q context
        | While (e, i) ->
            check_scope_expr e context K_VAR
            && check_scope_rec [ Stmt i ] context
            && check_scope_rec q context
        | Return e_opt ->
            (if e_opt <> None then
               check_scope_expr (Option.get e_opt) context K_VAR
             else true)
            && check_scope_rec q context)
    | Decl d :: q -> (
        match d with
        | Alias (s, _) -> check_scope_rec q (Type s :: context)
        | Let li ->
            let new_context =
              List.fold_right (fun (s, _, _) acc -> Var s :: acc) li context
            in
            List.for_all
              (fun (_, t_opt, e_opt) ->
                (match e_opt with
                | None -> true
                | Some e -> check_scope_expr e context K_VAR)
                &&
                match t_opt with
                | None -> true
                | Some t -> check_type_scope t context)
              li
            && check_scope_rec q new_context
        | Const li ->
            let new_context =
              List.fold_right (fun (s, _, _) acc -> Const s :: acc) li context
            in
            List.for_all
              (fun (_, t_opt, e_opt) ->
                (match e_opt with
                | None -> true
                | Some e -> check_scope_expr e context K_VAR)
                &&
                match t_opt with
                | None -> true
                | Some t -> check_type_scope t context)
              li
            && check_scope_rec q new_context
        | Func (id, pli, t_opt, ili) ->
            let added = Function id :: context in
            let new_context =
              List.fold_right (fun (s, _, _) acc -> Var s :: acc) pli added
            in
            check_scope_rec ili new_context && check_scope_rec q added)
    | [] -> true
  in
  check_scope_rec ast []

type typeCtx =
  | Alias of string * type_
  | Fun of string * type_
  | Var of string * type_

let rec has_field s t context =
  match t with
  | TypeBoolean | TypeAny | TypeTab _ | TypeNumber | TypeString -> false
  | TypeCte exp -> has_field s (get_typeof exp context) context
  | TypeIdentifier name -> has_field s (get_type_of_var name context) context
  | TypeUnion li -> List.for_all (fun t -> has_field s t context) li
  | TypeObject li -> List.exists (fun (name, _) -> name = s) li

and uniq li =
  match li with
  | [] -> []
  | h :: t -> if List.mem h t then uniq t else h :: uniq t

and get_typeof exp context =
  match exp with
  | FloatConst _ | IntConst _ -> TypeNumber
  | StringConst _ -> TypeString
  | BoolConst _ -> TypeBoolean
  | Obj li -> TypeObject (List.map (fun (s, e) -> (s, get_typeof e context)) li)
  | Tab li ->
      let types = uniq (List.map (fun e -> get_typeof e context) li) in
      TypeTab
        (match types with [] -> TypeAny | [ t ] -> t | _ -> TypeUnion types)
  | Funcall (f, _) -> get_return_type (get_function_name f) context
  | Unary (_, e) -> get_typeof e context
  | Binary (a, _, b) -> get_typeof a context
  | LeftMember lm -> get_typeof_left lm context
  | Assign (lm, e) -> get_typeof e context

and get_type_of_var name context =
  match
    List.find_opt
      (fun x -> match x with Var (s, _) -> name = s | _ -> false)
      context
  with
  | Some (Var (_, t)) -> t
  | _ -> failwith ("No alias type for " ^ name)

and is_subscriptable t context =
  match t with
  | TypeString | TypeBoolean | TypeNumber | TypeObject _ -> false
  | TypeTab _ -> true
  | TypeAny -> true
  | TypeIdentifier s -> is_subscriptable (get_type_from_name s context) context
  | TypeUnion li -> List.for_all (fun t -> is_subscriptable t context) li
  | TypeCte exp -> is_subscriptable (get_typeof exp context) context

and tab_lower t context =
  match t with
  | TypeAny -> TypeAny
  | TypeTab t -> t
  | TypeIdentifier s -> tab_lower (get_type_from_name s context) context
  | TypeUnion li when List.for_all (fun x -> is_subscriptable x context) li ->
      TypeUnion (List.map (fun x -> tab_lower x context) li)
  | _ -> failwith "Unreachable: Type cannot be subscripted"

and get_type_of_field t s context =
  match t with
  | TypeAny -> TypeAny
  | TypeObject li -> (
      match List.find_opt (fun (member, _) -> s = member) li with
      | Some (_, t) -> t
      | None -> failwith "No such field in object")
  | TypeIdentifier s ->
      get_type_of_field (get_type_from_name s context) s context
  | _ -> failwith ("Type does not contain field " ^ s)

and get_typeof_left exp context =
  match exp with
  | Identifier s -> get_type_from_name s context
  | Subscript (e, _) -> tab_lower (get_typeof e context) context
  | Access (e, s) -> get_type_of_field (get_typeof e context) s context
  | Expr e -> get_typeof e context

and get_function_name e =
  match e with
  | LeftMember (Identifier s) -> s
  | _ -> failwith "Not a valid function"

and get_type_from_name s context =
  match
    List.find_opt
      (fun x -> match x with Alias (name, _) -> name = s | _ -> false)
      context
  with
  | Some (Alias (_, t)) -> t
  | _ -> failwith ("No alias type for " ^ s)

and get_return_type f context =
  match
    List.find_opt
      (fun x -> match x with Fun (name, _) -> name = f | _ -> false)
      context
  with
  | Some (Fun (_, t)) -> t
  | _ -> failwith ("No function " ^ f)

let rec print_type t ctx =
  match t with
  | TypeNumber -> Printf.printf "number"
  | TypeString -> Printf.printf "string"
  | TypeBoolean -> Printf.printf "bool"
  | TypeAny -> Printf.printf "any"
  | TypeTab t' ->
      print_type t' ctx;
      Printf.printf "[]"
  | TypeObject li ->
      Printf.printf "{";
      prt_aux
        (fun (s, t') ->
          Printf.printf " %s: " s;
          print_type t' ctx)
        li;
      Printf.printf "}"
  | TypeUnion l ->
      let hd = List.hd l in
      let tl = List.tl l in
      print_type hd ctx;
      List.iter
        (fun x ->
          Printf.printf " | ";
          print_type x ctx)
        tl
  | TypeCte e ->
      let t = get_typeof e ctx in
      print_type t ctx
  | TypeIdentifier s -> print_type (get_type_from_name s ctx) ctx

let check_type ast =
  let current_return_type = ref None in

  let rec check_type_rec ast ctx =
    match ast with
    | [] -> true
    | Stmt stmt :: q -> (
        match stmt with
        | Empty -> check_type_rec q ctx
        | Expr e -> is_sub_type e TypeAny ctx
        | Compound li -> check_type_rec li ctx && check_type_rec q ctx
        | VarDecl bli ->
            let new_context =
              List.fold_right
                (fun (s, t, _) acc ->
                  (if t <> None then Var (s, Option.get t) else Var (s, TypeAny))
                  :: acc)
                bli ctx
            in
            List.for_all
              (fun (s, t, eopt) ->
                let ty = if t <> None then Option.get t else TypeAny in
                if eopt <> None then
                  is_sub_type (Option.get eopt) ty new_context
                else true)
              bli
            && check_type_rec q new_context
        | If (e, s, e2) ->
            check_type_rec [ Stmt s ] ctx
            &&
            if e2 <> None then check_type_rec [ Stmt (Option.get e2) ] ctx
            else true
        | Return e ->
            if e = None then
              if !current_return_type = None then (
                Printf.printf "Type Error: returning outside of function.\n";
                false)
              else !current_return_type = Some TypeAny
            else if !current_return_type <> None then
              if
                is_sub_type (Option.get e) (Option.get !current_return_type) ctx
              then true
              else (
                Printf.printf "Type Error: expected return type ";
                print_type (Option.get !current_return_type) ctx;
                Printf.printf " but got ";
                print_type (get_typeof (Option.get e) ctx) ctx;
                Printf.printf "\n";
                false)
            else (
              Printf.printf "Type Error: returning outside of function.\n";
              false)
        | _ -> true)
    | Decl decl :: q -> (
        match decl with
        | Alias (s, t) -> check_type_rec q (Alias (s, t) :: ctx)
        | Const bli | Let bli ->
            let new_context =
              List.fold_right
                (fun (s, t, _) acc ->
                  (if t <> None then Var (s, Option.get t) else Var (s, TypeAny))
                  :: acc)
                bli ctx
            in
            List.for_all
              (fun (s, t, eopt) ->
                let ty = if t <> None then Option.get t else TypeAny in
                if eopt <> None then
                  is_sub_type (Option.get eopt) ty new_context
                else true)
              bli
            && check_type_rec q new_context
        | Func (name, bli, topt, li) ->
            let old_ret_type = !current_return_type in
            if topt <> None then current_return_type := Some (Option.get topt)
            else current_return_type := Some TypeAny;
            let new_context1 =
              Fun (name, if topt = None then TypeAny else Option.get topt)
              :: ctx
            in
            let new_context2 =
              List.fold_right
                (fun (s, t, _) acc ->
                  (if t <> None then Var (s, Option.get t) else Var (s, TypeAny))
                  :: acc)
                bli new_context1
            in
            let res1 = check_type_rec li new_context2 in
            current_return_type := old_ret_type;
            res1 && check_type_rec q new_context1)
  and is_sub_type e t context =
    if t = TypeAny then true
    else
      let t2 = get_typeof e context in
      is_type_sub_type t2 t context
  and is_type_sub_type t1 t2 context =
    match t2 with
    | TypeAny -> true
    | TypeCte e ->
        let t = get_typeof e context in
        is_type_sub_type t1 t context
    | TypeIdentifier s ->
        let t = get_type_from_name s context in
        is_type_sub_type t1 t context
    | _ -> (
        match t1 with
        | TypeIdentifier s ->
            is_type_sub_type (get_type_from_name s context) t2 context
        | TypeAny -> t1 = t2
        | TypeCte e ->
            let t = get_typeof e context in
            is_type_sub_type t t2 context
        | TypeString | TypeBoolean | TypeNumber -> (
            match t2 with
            | t2 when t1 = t2 -> true
            | TypeUnion li ->
                List.exists (fun x -> is_type_sub_type t1 x context) li
            | _ -> false)
        | TypeUnion li ->
            List.for_all (fun x -> is_type_sub_type x t2 context) li
        | TypeTab t ->
            if is_subscriptable t2 context then
              is_type_sub_type t (tab_lower t2 context) context
            else false
        | TypeObject li -> (
            match t2 with
            | TypeUnion l ->
                List.exists (fun t -> is_type_sub_type t1 t context) l
            | TypeObject l2 ->
                List.for_all
                  (fun (s1, t21) ->
                    List.exists
                      (fun (s2, t22) ->
                        s1 = s2 && is_type_sub_type t22 t21 context)
                      li)
                  l2
                && List.for_all
                     (fun (s1, t21) ->
                       List.exists
                         (fun (s2, t22) ->
                           s1 = s2 && is_type_sub_type t21 t22 context)
                         l2)
                     li
            | _ -> false))
  in
  check_type_rec ast []

let rec transpile ast =
  match ast with
  | Stmt Empty :: q -> ";\n" ^ transpile q
  | Stmt (VarDecl li) :: q ->
      let l =
        List.map
          (fun (s, _, eopt) ->
            "var " ^ s
            ^ (if eopt = None then ""
               else
                 let e = Option.get eopt in
                 " = " ^ trans_expr e)
            ^ ";\n")
          li
      in
      List.fold_right ( ^ ) l "" ^ transpile q
  | Stmt (Expr e) :: q -> trans_expr e ^ ";\n" ^ transpile q
  | Stmt (If (e, s1, s2)) :: q ->
      "if (" ^ trans_expr e ^ ")\n" ^ transpile [ Stmt s1 ]
      ^ (if s2 <> None then "else\n" ^ transpile [ Stmt (Option.get s2) ]
         else "")
      ^ transpile q
  | Stmt (Return e) :: q ->
      "return "
      ^ (if e <> None then trans_expr (Option.get e) else "")
      ^ ";\n" ^ transpile q
  | Stmt (Compound li) :: q -> "{" ^ transpile li ^ "}" ^ transpile q
  | [] -> ""
  | _ -> "TODO"

and trans_expr e =
  match e with
  | IntConst i -> string_of_int i
  | FloatConst f -> string_of_float f
  | StringConst s -> "\"" ^ s ^ "\""
  | BoolConst b -> string_of_bool b
  | Obj li ->
      "{"
      ^ List.fold_right ( ^ )
          (List.map (fun (s, e) -> s ^ ": " ^ trans_expr e ^ ", ") li)
          ""
      ^ "}"
  | Tab t ->
      "["
      ^ List.fold_right ( ^ ) (List.map (fun e -> trans_expr e ^ ", ") t) ""
      ^ "]"
  | LeftMember lm -> trans_lm lm
  | Funcall (e, li) ->
      trans_expr e ^ "("
      ^ List.fold_right ( ^ ) (List.map (fun e -> trans_expr e ^ ", ") li) ""
      ^ ")"
  | _ -> "TODO "

and trans_lm lm =
  match lm with
  | Identifier s -> s
  | Subscript (e, i) -> trans_expr e ^ "[" ^ trans_expr i ^ "]"
  | Access (e, id) -> trans_expr e ^ "." ^ id
  | Expr e -> trans_expr e

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
let () = Printf.printf "\nType:\n"
let () = Printf.printf "%s\n" (if check_type ast then "true" else "false")
let () = Printf.printf "\nJavaScript:\n\n"
let () = Printf.printf "%s\n" (transpile ast)
