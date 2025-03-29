
open OUnit2
open Scanner (* scanner.ml contains the token type and lexer *)

(* Helper function to get all tokens from a string *)
let get_all_tokens str =
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
      [VAR; IDENT "x"; DECL_ASSIGN; INT_LIT 10]
      (get_all_tokens "var x := 10"));

  "function declaration" >:: (fun _ ->
    assert_equal
      [FUNC; IDENT "main"; LPAREN; RPAREN; LBRACE; RETURN; INT_LIT 42; SEMICOLON; RBRACE]
      (get_all_tokens "func main() { return 42; }"));

  "type casting" >:: (fun _ ->
    assert_equal
      [VAR; IDENT "x"; DECL_ASSIGN; U16; LPAREN; INT_LIT 10; RPAREN; SEMICOLON]
      (get_all_tokens "var x := u16(10);"));

  "escape sequeneces" >:: (fun _ ->
    assert_equal
      [STRING_LIT "Hello\nWorld"]
      (get_all_tokens "\"Hello\\nWorld\"");

    assert_equal
      [CHAR_LIT '\t']
      (get_all_tokens "'\\t'");
  )
]

(* Run test *)
let () = run_test_tt_main scanner_tests
