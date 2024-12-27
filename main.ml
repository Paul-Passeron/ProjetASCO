open Lexer
open Tpscrpt
open Parser
open Tpscrpt



let print_token t = let () = match t with
| T_INT_LIT d -> Printf.printf "T_INT_LIT %d" d
| T_STR_LIT s -> Printf.printf "T_STR_LIT %s" s
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
| T_EQUAL -> Printf.printf "T_EQUAL"
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
in Printf.printf "\n"

let rec read l =
  try
    let res = token l in
    if res = EOF then () else
      let _ = print_token res in
      read l
  with _ -> ()

let () = Printf.printf "\nTokens:\n"
let contents = read_whole_file "test.ts"
let lexbuf = Lexing.from_string contents
let _ = read lexbuf
let () = Printf.printf "\nAst:\n"
let lexbuf = Lexing.from_string contents 
let () = program token lexbuf |> print_program