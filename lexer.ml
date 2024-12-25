# 1 "lexer.mll"


(* open Parsertmp *)

let keyword_table = Hashtbl.create 53
let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [
      ("number", T_NUMBER);
      ("string", T_STRING);
      ("boolean", T_BOOLEAN);
      ("if", T_IF);
      ("else", T_ELSE);
      ("while", T_WHILE);
      ("return", T_RETURN);
      ("let", T_LET);
      ("var", T_VAR);
      ("function", T_FUNCTION);
      ("typeof", T_TYPEOF);
    ]

let token_of_word = fun w ->
  try (Hashtbl.find keyword_table w)
  with Not_found -> T_IDENTIFIER (w)

# 28 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\001\000\078\000\088\000\144\000\022\000\001\000\002\000\
    \003\000\031\000\033\000\054\000\241\255\242\255\243\255\244\255\
    \245\255\246\255\247\255\248\255\249\255\250\255\251\255\252\255\
    \005\000\105\000\106\000\011\000\255\255\108\000\109\000\254\255\
    \226\255\237\255\235\255\088\000\232\255\092\000\231\255\230\255\
    \229\255\109\000\220\000\243\000\111\000";
  Lexing.lex_backtrk =
   "\255\255\034\000\033\000\033\000\030\000\255\255\255\255\028\000\
    \027\000\019\000\017\000\015\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \002\000\016\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\021\000\255\255\022\000\255\255\255\255\
    \255\255\255\255\255\255\031\000\032\000";
  Lexing.lex_default =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\026\000\027\000\000\000\029\000\029\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\255\255\255\255\255\255\255\255";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\024\000\024\000\000\000\000\000\024\000\024\000\024\000\
    \000\000\000\000\024\000\000\000\000\000\028\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\007\000\001\000\001\000\000\000\024\000\006\000\039\000\
    \019\000\018\000\011\000\013\000\023\000\012\000\022\000\025\000\
    \003\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\021\000\020\000\010\000\008\000\009\000\037\000\
    \035\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\015\000\034\000\014\000\033\000\004\000\
    \032\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\017\000\005\000\016\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\040\000\026\000\029\000\036\000\030\000\030\000\
    \027\000\038\000\041\000\255\255\031\000\044\000\044\000\044\000\
    \044\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \042\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\041\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \042\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\000\000\000\000\000\000\000\000\004\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\028\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\000\000\000\000\000\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\000\000\024\000\024\000\
    \255\255\255\255\024\000\255\255\255\255\027\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\001\000\255\255\024\000\000\000\006\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
    \008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\009\000\000\000\010\000\000\000\
    \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\005\000\025\000\026\000\035\000\029\000\030\000\
    \025\000\037\000\003\000\029\000\030\000\041\000\041\000\044\000\
    \044\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\003\000\255\255\255\255\255\255\255\255\255\255\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\255\255\255\255\255\255\255\255\004\000\
    \255\255\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\027\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\042\000\042\000\042\000\
    \042\000\042\000\042\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\043\000\043\000\043\000\043\000\
    \043\000\043\000\255\255\255\255\255\255\042\000\042\000\042\000\
    \042\000\042\000\042\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\043\000\043\000\043\000\043\000\
    \043\000\043\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\026\000\255\255\029\000\030\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 45 "lexer.mll"
                        ( token lexbuf )
# 200 "lexer.ml"

  | 1 ->
# 46 "lexer.mll"
                        ( token lexbuf )
# 205 "lexer.ml"

  | 2 ->
# 47 "lexer.mll"
                        ( token lexbuf )
# 210 "lexer.ml"

  | 3 ->
# 48 "lexer.mll"
                        ( T_COMMA )
# 215 "lexer.ml"

  | 4 ->
# 49 "lexer.mll"
                        ( T_DOT )
# 220 "lexer.ml"

  | 5 ->
# 50 "lexer.mll"
                        ( T_COLON )
# 225 "lexer.ml"

  | 6 ->
# 51 "lexer.mll"
                        ( T_SEMICOLON )
# 230 "lexer.ml"

  | 7 ->
# 52 "lexer.mll"
                        ( T_OPEN_PAR )
# 235 "lexer.ml"

  | 8 ->
# 53 "lexer.mll"
                        ( T_CLOSE_PAR )
# 240 "lexer.ml"

  | 9 ->
# 54 "lexer.mll"
                        ( T_OPEN_BRA )
# 245 "lexer.ml"

  | 10 ->
# 55 "lexer.mll"
                        ( T_CLOSE_BRA )
# 250 "lexer.ml"

  | 11 ->
# 56 "lexer.mll"
                        ( T_OPEN_SQR )
# 255 "lexer.ml"

  | 12 ->
# 57 "lexer.mll"
                        ( T_CLOSE_SQR )
# 260 "lexer.ml"

  | 13 ->
# 58 "lexer.mll"
                        ( T_PLUS )
# 265 "lexer.ml"

  | 14 ->
# 59 "lexer.mll"
                        ( T_MINUS )
# 270 "lexer.ml"

  | 15 ->
# 60 "lexer.mll"
                        ( T_MUL )
# 275 "lexer.ml"

  | 16 ->
# 61 "lexer.mll"
                        ( T_DIV )
# 280 "lexer.ml"

  | 17 ->
# 62 "lexer.mll"
                        ( T_LT )
# 285 "lexer.ml"

  | 18 ->
# 63 "lexer.mll"
                        ( T_LEQ )
# 290 "lexer.ml"

  | 19 ->
# 64 "lexer.mll"
                        ( T_GT )
# 295 "lexer.ml"

  | 20 ->
# 65 "lexer.mll"
                        ( T_GEQ )
# 300 "lexer.ml"

  | 21 ->
# 66 "lexer.mll"
                        ( T_EQUAL )
# 305 "lexer.ml"

  | 22 ->
# 67 "lexer.mll"
                        ( T_DIFF )
# 310 "lexer.ml"

  | 23 ->
# 68 "lexer.mll"
                        ( T_EQQ )
# 315 "lexer.ml"

  | 24 ->
# 69 "lexer.mll"
                        ( T_NEQQ )
# 320 "lexer.ml"

  | 25 ->
# 70 "lexer.mll"
                        ( T_AND )
# 325 "lexer.ml"

  | 26 ->
# 71 "lexer.mll"
                        ( T_OR )
# 330 "lexer.ml"

  | 27 ->
# 72 "lexer.mll"
                        ( T_ASSIGN )
# 335 "lexer.ml"

  | 28 ->
# 73 "lexer.mll"
                        ( T_NOT )
# 340 "lexer.ml"

  | 29 ->
# 74 "lexer.mll"
                        ( T_POW )
# 345 "lexer.ml"

  | 30 ->
let
# 75 "lexer.mll"
                 lexeme
# 351 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 75 "lexer.mll"
                        ( token_of_word lexeme )
# 355 "lexer.ml"

  | 31 ->
let
# 76 "lexer.mll"
                 lexeme
# 361 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 76 "lexer.mll"
                        ( T_INT_LIT (int_of_hex lexeme) )
# 365 "lexer.ml"

  | 32 ->
let
# 77 "lexer.mll"
                 lexeme
# 371 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 77 "lexer.mll"
                        ( T_INT_LIT (int_of_bin lexeme) )
# 375 "lexer.ml"

  | 33 ->
let
# 78 "lexer.mll"
                 lexeme
# 381 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 78 "lexer.mll"
                        ( T_INT_LIT (int_of_string lexeme) )
# 385 "lexer.ml"

  | 34 ->
let
# 79 "lexer.mll"
                 lexeme
# 391 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 79 "lexer.mll"
                        ( T_STR_LIT lexeme )
# 395 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

# 81 "lexer.mll"

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

# 411 "lexer.ml"