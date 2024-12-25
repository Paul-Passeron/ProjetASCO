val keyword_table : (string, Parser.token) Hashtbl.t
val token_of_word : string -> Parser.token
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> Parser.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> Parser.token
