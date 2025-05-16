(* OCAMLLEX SCANNER FOR P.A.T. *)

{

open Parser (* use parser's tokens *)

exception Lexer_error of string

(* Any OCaml functions defined here will be subsequently available in the remainder of the lexer definition. *)

let interpret_string s =
  let rec interp i acc =
    if i >= String.length s then acc
    else if s.[i] = '\\' then
      let c = match s.[i+1] with
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | '"' -> '"'
        | '\\' -> '\\'
        | _ -> failwith "Invalid escape sequence"
      in interp (i+2) (acc ^ String.make 1 c)
    else interp (i+1) (acc ^ String.make 1 s.[i])
  in interp 0 ""

let interpret_char s =
  if String.length s = 1 then s.[0]
  else if s.[0] = '\\' then
    match s.[1] with
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | '\'' -> '\''
    | '\\' -> '\\'
    | _ -> failwith "Invalid escape sequence"
  else failwith "Invalid character literal"

(* ------------------------------------------------------------------ *)
(*  Automatic-semicolon-insertion (ASI) state                         *)
(* This lexer implements automatic semicolon insertion similar to Go. *)
(* The core idea is that a newline character can implicitly act as a  *)
(* semicolon, terminating a statement, but only under specific        *)
(* conditions.                                                        *)

let need_semi   = ref false
(* Purpose: Tracks whether the immediately preceding token emitted  *)
(*          was one that could legally end a statement (as defined  *)
(*          by the `ends_stmt` function).                           *)
(* Set by: `emit` function sets this to true after emitting a token *)
(*         that matches `ends_stmt`.                                 *)
(* Reset by: Set to `false` when an explicit `;` is processed, OR when ASI *)
(*           successfully inserts a semicolon on a newline.*)



let paren_depth = ref 0
(* Purpose: Tracks the nesting level of `()` and `[]` only. *)
(*          Braces `{}` for expressions are handled by Scanner_state.expr_brace_depth. *)
(* Logic: ASI is disabled (newlines are treated as whitespace) whenever `paren_depth` is > 0. *)
(* Updated by: `update_nesting` (called by `emit`). *)

(* The `ends_stmt` function below lists tokens that can end a statement *)
(* if followed by a newline (outside parentheses/brackets).            *)
let ends_stmt = function
  | IDENT _ | TYPE_NAME _
  | INT_LIT _ | FLOAT_LIT _ | STRING_LIT _ | CHAR_LIT _
  | BOOL_LIT _ | NULL
  | RETURN | BREAK | CONTINUE | INC | DEC
  | RPAREN | RBRACKET | RBRACE                       -> true
  | _                                               -> false

(* The `update_nesting` function updates `paren_depth`. *)
let update_nesting = function
  | LPAREN  -> incr paren_depth | RPAREN  -> decr paren_depth
  | LBRACKET-> incr paren_depth | RBRACKET-> decr paren_depth
  | _ -> ()

(* The `emit` function is a wrapper to update state before returning a token. *)
let emit tok =
  update_nesting tok;
  need_semi := ends_stmt tok;
  tok

}

(* Regular expressions for token components *)
let digit      = ['0'-'9']
let alpha      = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t' '\r']
let identifier = alpha (alpha | digit | '_')*

(* Literals *)
let int_lit   = ['-']? digit+
let float_lit = ['-']? ((digit* '.' digit+) | (digit+ '.' digit*))

(* Boolean literal? Also, define string_lit as 0 or more char_lit? *)
let string_lit = '"' ([^ '"' '\\'] | '\\' ['n' 't' 'r' '"' '\\'])* '"'
let char_lit = '\'' ([^ '\'' '\\'] | '\\' ['n' 't' 'r' '\'' '\\']) '\''

rule token = parse

    (* Whitespace *)
    | whitespace            { token lexbuf }
    | '\n'                  { Lexing.new_line lexbuf;
                            if !paren_depth > 0 || Scanner_state.get_expr_brace_level() > 0 then (
                                token lexbuf (* Supress ASI if inside (), [], or expression {} *)
                            ) else if !need_semi then (
                                (need_semi := false; SEMICOLON)
                            ) else
                                token lexbuf}



    (* Comments *)
    | "//" [^ '\n']*        { token lexbuf }
    | "/*"                  { comment lexbuf }

    (*************** KEYWORDS ***************)

    (* Functions and Packages *)
    | "func"                { emit FUNC }
    | "package"             { emit PACKAGE }
    | "import"              { emit IMPORT }

    (* Types and Structs *)
    | "type"                { emit TYPE }
    | "struct"              { emit STRUCT }

    (* Control Flow *)
    | "return"              { emit RETURN }
    | "break"               { emit BREAK }
    | "if"                  { emit IF }
    | "else"                { emit ELSE }
    | "continue"            { emit CONTINUE }
    | "for"                 { emit FOR }
    | "while"               { emit WHILE }

    (* Constants and Variables *)
    | "const"               { emit CONST }
    (*| "make"                { emit MAKE }*)

    (* Boolean Literals *)
    | "true"                { emit (BOOL_LIT(true)) }
    | "false"               { emit (BOOL_LIT(false)) }

    (* Data Types *)
    | "error"               { emit ERROR }
    | "null"                { emit NULL }

    (* Modifiers *)
    | "final"               { emit FINAL }
    | "mut"                 { emit MUT }
    | "late"                { emit LATE }
    | "private"             { emit PRIVATE }

    (* Built-in types *)
    | "bool"               { emit BOOL }
    | "string"             { emit STRING }
    | "u8"                 { emit U8 }
    | "u16"                { emit U16 }
    | "u32"                { emit U32 }
    | "u64"                { emit U64 }
    | "i8"                 { emit I8 }
    | "i16"                { emit I16 }
    | "i32"                { emit I32 }
    | "i64"                { emit I64 }
    | "f32"                { emit F32 }
    | "f64"                { emit F64 }

    (* Literals *)
    | int_lit               { emit(INT_LIT (int_of_string (Lexing.lexeme lexbuf))) }
    | float_lit             { emit(FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf))) }
    | string_lit            { let s = Lexing.lexeme lexbuf in
                                (* Remove the quotes *)
                                let content = String.sub s 1 (String.length s - 2) in
                                emit(STRING_LIT (interpret_string content)) }
    | char_lit              { let c = Lexing.lexeme lexbuf in
                                let content = String.sub c 1 (String.length c - 2) in
                                emit(CHAR_LIT (interpret_char content))}

    (* Arithmetic *)
    | "+"                   { emit PLUS }
    | "-"                   { emit MINUS }
    | "/"                   { emit DIV }
    | "%"                   { emit MOD }
    | "*"                   { emit MULT }

    (* Bitwise *)
    | "<<"                  { emit LSHIFT }
    | ">>"                  { emit RSHIFT }
    | "^"                   { emit BITXOR }
    | "|"                   { emit BITOR }
    | "~"                   { emit BITNOT }
    | "&"                   { emit BITAND }

    (* Assignment *)
    | "="                   { emit ASSIGN }
    | ":="                  { emit DECL_ASSIGN }
    | "+="                  { emit PLUS_ASSIGN }
    | "-="                  { emit MINUS_ASSIGN }
    | "*="                  { emit TIMES_ASSIGN }
    | "/="                  { emit DIV_ASSIGN }
    | "%="                  { emit MOD_ASSIGN }
    | "<<="                 { emit LSHIFT_ASSIGN }
    | ">>="                 { emit RSHIFT_ASSIGN }
    | "&="                  { emit BITAND_ASSIGN }
    | "^="                  { emit BITXOR_ASSIGN }
    | "|="                  { emit BITOR_ASSIGN }

    (* Equivalence *)
    | "=="                  { emit EQ }
    | "!="                  { emit NEQ }
    | "<"                   { emit LT }
    | "<="                  { emit LE }
    | ">"                   { emit GT }
    | ">="                  { emit GE }

    (* Logical *)
    | "&&"                  { emit AND }
    | "||"                  { emit OR }
    | "!"                   { emit NOT }

    (* Unary *)
    | "++"                  { emit INC }
    | "--"                  { emit DEC }

    (* Separators *)
    | "("                   { emit LPAREN }
    | ")"                   { emit RPAREN }
    | "{"                   { emit LBRACE }
    | "}"                   { emit RBRACE }
    | "["                   { emit LBRACKET }
    | "]"                   { emit RBRACKET }
    | ";"                   { need_semi := false; emit SEMICOLON }
    | ":"                   { emit COLON }
    | ","                   { emit COMMA }
    | "."                   { emit DOT }

    (* Ocamllex checks rules in order, so this is after keywords *)
    | identifier as s           { if Scanner_state.is_type_name s then
                                emit (TYPE_NAME s)
                              else
                                emit (IDENT s)
                            }
    | eof                   { EOF }
    | _ as c                { raise (Failure (Printf.sprintf "Lexing bad char: '%c'" c)) }

and comment = parse
    | "*/"      { token lexbuf }
    | '\n'      { Lexing.new_line lexbuf; comment lexbuf }
    | "/*"      { ignore (comment lexbuf); comment lexbuf }
    | eof       { failwith "unterminated comment" }
    | _         { comment lexbuf }
