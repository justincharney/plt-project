
open Printf
open Scanner (* scanner.ml contains the token type and lexer *)

(* Function to print tokens *)
let print_token = function
  | IDENT s -> printf "IDENT(%s)\n" s
  | INT_LIT i -> printf "INT_LIT(%d)\n" i
  | FLOAT_LIT f -> printf "FLOAT_LIT(%f)\n" f
  | STRING_LIT s -> printf "STRING_LIT(%s)\n" s
  | CHAR_LIT c -> printf "CHAR_LIT(%c)\n" c
  | BOOL_LIT b -> printf "BOOL_LIT(%b)\n" b
  | FUNC -> printf "FUNC\n"
  | PACKAGE -> printf "PACKAGE\n"
  | IMPORT -> printf "IMPORT\n"
  | TYPE -> printf "TYPE\n"
  | STRUCT -> printf "STRUCT\n"
  | RETURN -> printf "RETURN\n"
  | BREAK -> printf "BREAK\n"
  | IF -> printf "IF\n"
  | ELSE -> printf "ELSE\n"
  | CONTINUE -> printf "CONTINUE\n"
  | FOR -> printf "FOR\n"
  | WHILE -> printf "WHILE\n"
  | CONST -> printf "CONST\n"
  | VAR -> printf "VAR\n"
  | MAKE -> printf "MAKE\n"
  | TRUE -> printf "TRUE\n"
  | FALSE -> printf "FALSE\n"
  | FINAL -> printf "FINAL\n"
  | MUT -> printf "MUT\n"
  | LATE -> printf "LATE\n"
  | PRIVATE -> printf "PRIVATE\n"
  | GET -> printf "GET\n"
  | POST -> printf "POST\n"
  | DELETE -> printf "DELETE\n"
  | ERROR -> printf "ERROR\n"
  | NULL -> printf "NULL\n"
  | BOOL -> printf "BOOL\n"
  | STRING -> printf "STRING\n"
  | U8 -> printf "U8\n"
  | U16 -> printf "U16\n"
  | U32 -> printf "U32\n"
  | U64 -> printf "U64\n"
  | I8 -> printf "I8\n"
  | I16 -> printf "I16\n"
  | I32 -> printf "I32\n"
  | I64 -> printf "I64\n"
  | F16 -> printf "F16\n"
  | F32 -> printf "F32\n"
  | PLUS -> printf "PLUS\n"
  | MINUS -> printf "MINUS\n"
  | DIV -> printf "DIV\n"
  | MOD -> printf "MOD\n"
  | LSHIFT -> printf "LSHIFT\n"
  | RSHIFT -> printf "RSHIFT\n"
  | BITXOR -> printf "BITXOR\n"
  | BITOR -> printf "BITOR\n"
  | BITNOT -> printf "BITNOT\n"
  | ASSIGN -> printf "ASSIGN\n"
  | DECL_ASSIGN -> printf "DECL_ASSIGN\n"
  | PLUS_ASSIGN -> printf "PLUS_ASSIGN\n"
  | MINUS_ASSIGN -> printf "MINUS_ASSIGN\n"
  | TIMES_ASSIGN -> printf "TIMES_ASSIGN\n"
  | DIV_ASSIGN -> printf "DIV_ASSIGN\n"
  | MOD_ASSIGN -> printf "MOD_ASSIGN\n"
  | LSHIFT_ASSIGN -> printf "LSHIFT_ASSIGN\n"
  | RSHIFT_ASSIGN -> printf "RSHIFT_ASSIGN\n"
  | BITAND_ASSIGN -> printf "BITAND_ASSIGN\n"
  | BITXOR_ASSIGN -> printf "BITXOR_ASSIGN\n"
  | BITOR_ASSIGN -> printf "BITOR_ASSIGN\n"
  | EQ -> printf "EQ\n"
  | NEQ -> printf "NEQ\n"
  | LT -> printf "LT\n"
  | LE -> printf "LE\n"
  | GT -> printf "GT\n"
  | GE -> printf "GE\n"
  | AND -> printf "AND\n"
  | OR -> printf "OR\n"
  | NOT -> printf "NOT\n"
  | INC -> printf "INC\n"
  | DEC -> printf "DEC\n"
  | AMPERSAND -> printf "AMPERSAND\n"
  | ASTERISK -> printf "ASTERISK\n"
  | LPAREN -> printf "LPAREN\n"
  | RPAREN -> printf "RPAREN\n"
  | LBRACE -> printf "LBRACE\n"
  | RBRACE -> printf "RBRACE\n"
  | LBRACKET -> printf "LBRACKET\n"
  | RBRACKET -> printf "RBRACKET\n"
  | SEMICOLON -> printf "SEMICOLON\n"
  | COLON -> printf "COLON\n"
  | COMMA -> printf "COMMA\n"
  | DOT -> printf "DOT\n"
  | QUESTION -> printf "QUESTION\n"
  | EOF -> printf "EOF\n"

(* tokenize a string and print all tokens *)
(* note that <> checks structual inequality (values are different) *)
let tokenize_string str =
  let lexbuf = Lexing.from_string str in
  let rec print_all () =
    let token = token lexbuf in
    print_token token;
    if token <> EOF then print_all ()
  in
  print_all ()

(* Test cases *)
let run_tests () =
  printf "\n--- Testing empty string ---\n";
  tokenize_string "";

  printf "\n--- Testing keywords ---\n";
  tokenize_string "func package import type struct return break if else continue for while const var make";

  printf "\n--- Testing literals ---\n";
  tokenize_string "123 45.67 \"hello\" 'a' true false";

  printf "\n--- Testing operators ---\n";
  tokenize_string "+ - / % == != < <= > >= && || ! ++ -- & *";

  printf "\n--- Testing identifiers ---\n";
  tokenize_string "foo bar123 baz_qux";

  printf "\n--- Testing comments ---\n";
  tokenize_string "foo // this is a comment\nbar /* multi\nline\ncomment */ baz";

  printf "\n--- Testing code ---\n";
  tokenize_string "func main() {\n  var x := u16(10);\n  if(x > 5) {\n    return true;\n  }\n}"

let () = run_tests ()
