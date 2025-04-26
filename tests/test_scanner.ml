
open OUnit2
open Scanner (* scanner.ml contains the token type and lexer *)

(* Helper function to get all tokens from a string *)
let get_all_tokens str =
  (* reset the ASI state before we start lexing a fresh string *)
  Scanner.need_semi   := false;
  Scanner.paren_depth := 0;

  let lexbuf = Lexing.from_string str in
  let rec loop acc =
    let token = Scanner.token lexbuf in
    if token = EOF then List.rev acc
    else loop(token :: acc)
  in
  loop []

(* Test cases *)
let scanner_tests = "test suite for scanner" >::: [
  "empty" >:: (fun _ ->
    assert_equal [] (get_all_tokens ""));

  "basic keywords" >:: (fun _ ->
    assert_equal [FUNC] (get_all_tokens "func");
    assert_equal [PACKAGE] (get_all_tokens "package"));

  "identifiers" >:: (fun _ ->
    assert_equal [IDENT "foo"] (get_all_tokens "foo");
    assert_equal [IDENT "bar123"] (get_all_tokens "bar123"));

  "operators" >:: (fun _ ->
    assert_equal [PLUS] (get_all_tokens "+");
    assert_equal [MINUS] (get_all_tokens "-");
    assert_equal [IDENT "x"; PLUS; IDENT "y"] (get_all_tokens "x + y"));

  "variable declaration" >:: (fun _ ->
    assert_equal
      [IDENT "x"; DECL_ASSIGN; INT_LIT 10]
      (get_all_tokens "x := 10"));

  "function declaration" >:: (fun _ ->
    assert_equal
      [FUNC; IDENT "main"; LPAREN; RPAREN; LBRACE; RETURN; INT_LIT 42; SEMICOLON; RBRACE]
      (get_all_tokens "func main() { return 42; }"));

  "type casting" >:: (fun _ ->
    assert_equal
      [IDENT "x"; DECL_ASSIGN; U16; LPAREN; INT_LIT 10; RPAREN; SEMICOLON]
      (get_all_tokens "x := u16(10);"));

  "escape sequeneces" >:: (fun _ ->
    assert_equal
      [STRING_LIT "Hello\nWorld"]
      (get_all_tokens "\"Hello\\nWorld\"");

    assert_equal
      [CHAR_LIT '\t']
      (get_all_tokens "'\\t'");
  );

  "automatic semicolon insertion - basic" >:: (fun _ ->
    assert_equal
      [IDENT "x"; ASSIGN; INT_LIT 5; SEMICOLON; IDENT "y"; ASSIGN; INT_LIT 6;]
      (get_all_tokens "x = 5 \n y = 6"));

  (* "automatic semicolon insertion - after statement-ending tokens" >:: (fun _ ->
    assert_equal
      [IDENT "x"; INC; SEMICOLON; IDENT "y"; ASSIGN; INT_LIT 10; SEMICOLON; RETURN; SEMICOLON; BREAK; SEMICOLON]
      (get_all_tokens "x++\ny = 10\nreturn\nbreak"));

  "automatic semicolon insertion - not inside parentheses" >:: (fun _ ->
    assert_equal
      [FUNC; IDENT "foo"; LPAREN; IDENT "x"; COMMA; IDENT "y"; RPAREN]
      (get_all_tokens "func foo(\nx,\ny)"));

  "automatic semicolon insertion - not inside brackets" >:: (fun _ ->
    assert_equal
      [IDENT "arr"; DECL_ASSIGN; LBRACKET; INT_LIT 1; COMMA; INT_LIT 2; COMMA; INT_LIT 3; RBRACKET]
      (get_all_tokens "arr := [\n1,\n2,\n3\n]"));

  "automatic semicolon insertion - after closing delimiters" >:: (fun _ ->
    assert_equal
      [FUNC; IDENT "foo"; LPAREN; RPAREN; LBRACE; RETURN; INT_LIT 5; SEMICOLON; RBRACE; SEMICOLON; IDENT "x"; ASSIGN; INT_LIT 10]
      (get_all_tokens "func foo() {\n  return 5\n}\nx = 10"));

  "automatic semicolon insertion - mixed with explicit semicolons" >:: (fun _ ->
    assert_equal
      [IDENT "x"; ASSIGN; INT_LIT 5; SEMICOLON; IDENT "y"; ASSIGN; INT_LIT 6; SEMICOLON; IDENT "z"; ASSIGN; INT_LIT 7; SEMICOLON]
      (get_all_tokens "x = 5;\ny = 6\nz = 7;"));

  "automatic semicolon insertion - nested blocks" >:: (fun _ ->
    assert_equal
      [IF; IDENT "x"; LBRACE; IDENT "y"; ASSIGN; INT_LIT 5; SEMICOLON;
       IF; IDENT "z"; LBRACE; IDENT "a"; ASSIGN; INT_LIT 10; SEMICOLON;
       RBRACE; SEMICOLON; IDENT "b"; ASSIGN; INT_LIT 15; SEMICOLON; RBRACE; SEMICOLON]
      (get_all_tokens "if x {\n  y = 5\n  if z {\n    a = 10\n  }\n  b = 15\n}"));

  "nested comments" >:: (fun _ ->
    assert_equal
      [IDENT "x"; ASSIGN; INT_LIT 5]
      (get_all_tokens "/* outer /* nested */ comment */ x = 5"));

  "deeply nested comments" >:: (fun _ ->
    assert_equal
      [IDENT "x"; ASSIGN; INT_LIT 5]
      (get_all_tokens "/* L1 /* L2 /* L3 */ L2 cont */ L1 cont */ x = 5")); *)

]

(* Run test *)
let () = run_test_tt_main scanner_tests
