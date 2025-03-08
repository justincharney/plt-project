(* Ocamllex scanner *)

{

(* Any OCaml functions defined here will be subsequently available in the remainder of the lexer definition. *)
open Printf
open Lexing

(* Helper to create integer literal with correct type suffix *)
let create_int_lit str suffix =
    let num_str = String.sub str 0 (String.length str - String.length suffix) in
    try
        match suffix with
        | "u8" -> UINT8_LIT (int_of_string num_str)
        | "u16" -> UINT16_LIT (int_of_string num_str)
        | "u32" -> UINT32_LIT (int_of_string num_str)
        | "u64" -> UINT64_LIT (Int64.of_string num_str)
        | "i8" -> INT8_LIT (int_of_string num_str)
        | "i16" -> INT16_LIT (int_of_string num_str)
        | "i32" -> INT32_LIT (int_of_string num_str)
        | "i64" -> INT64_LIT (Int64.of_string num_str)
        | _ -> INT_LIT (int_of_string num_str)
    with Failure _ ->
        error (sprintf "Invalid integer literal: %s" str)

let create_float_lit str suffix =
    let num_str = String.sub str 0 (String.length str - String.length suffix) in
    try
        match suffix with
        | "f32" -> FLOAT32_LIT (float_of_string num_str)
        | "f4" | "f64" -> FLOAT64_LIT (float_of_string num_str)
        | _ -> FLOAT32_LIT (float_of_string num_str) (* Default to FLOAT32_LIT if no suffix *)
    with Failure _ ->
        error (sprintf "Invalid float literal: %s" str)

}

type token =
  (* Keywords *)
  | FUNC | PACKAGE | IMPORT | TYPE | STRUCT | RETURN | BREAK | IF | ELSE
  | CONTINUE | FOR | CONST | VAR | MAKE | WHILE | DO | SWITCH | CASE | DEFAULT
  | TRUE | FALSE | FINAL | MUT | LATE | PRIVATE | GET | POST | DELETE | ERROR

  (* Identifiers *)
  | IDENT of string

  (* Literals *)
  | UINT8_LIT of int    (* uint8 literal *)
  | UINT16_LIT of int   (* uint16 literal *)
  | UINT32_LIT of int   (* uint32 literal *)
  | UINT64_LIT of int64 (* uint64 literal *)
  | INT8_LIT of int     (* int8 literal *)
  | INT16_LIT of int    (* int16 literal *)
  | INT32_LIT of int    (* int32 literal *)
  | INT64_LIT of int64  (* int64 literal *)
  | FLOAT32_LIT of float (* float32 literal *)
  | FLOAT64_LIT of float (* float64 literal *)
  | STRING_LIT of string (* string literal *)

  (* Operators *)
  (* Arithmetic *)
  | PLUS | MINUS | DIV | MOD | EXP
  (* Bitwise *)
  | LSHIFT | RSHIFT | BITXOR | BITOR | BITNOT
  (* Assignment *)
  | ASSIGN | DECL_ASSIGN (* = vs := *)
  | PLUS_ASSIGN | MINUS_ASSIGN | TIMES_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | EXP_ASSIGN | LSHIFT_ASSIGN | RSHIFT_ASSIGN | BITAND_ASSIGN | BITXOR_ASSIGN | BITOR_ASSIGN
  (* Equivalence *)
  | EQ | NEQ | LT | LE | GT | GE
  (* Logical *)
  | AND | OR | NOT
  (* Unary *)
  | INC | DEC | TILDE
  (* Ambiguous - BITAND/ADDR_OF or DEREF/TIMES *)
  | AMPERSAND | ASTERISK

  (* Separators *)
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET
  | SEMICOLON | COLON | COMMA | DOT | QUESTION

  (* Special tokens *)
  | EOF     (* End of file *)
  | EOL     (* End of line, if needed *)


(* Regular expressions for token components *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t']
let newline = '\n' | '\r' | "\r\n"
let identifier = alpha (alpha | digit | '_')*

(* Integer literals *)
let int_lit = digit+
let uint8_lit = int_lit "u8"
let uint16_lit = int_lit "u16"
let uint32_lit = int_lit "u32"
let uint64_lit = int_lit "u64"
let int8_lit = int_lit "i8"
let int16_lit = int_lit "i16"
let int32_lit = int_lit "i32"
let int64_lit = int_lit "i64"

(* Float literals *)
let float_lit = (digit* '.' digit+) | (digit+ '.' digit*)
let float32_lit = float_lit "f32"
let float64_lit = float_lit ("f4" | "f64")

(* Match nicely formatted strings. No multi-line *)
let string_lit = '"' ([^ '"' '\\' '\n'])* '"'

rule token = parse
    (* Whitespace *)
    | whitespace            { token lexbuf }
    | newline               { token lexbuf }

    (* Comments *)
    | "//" [^ '\n']*        { token lexbuf }
    | "/*"                  { comment lexbuf }

    (* Keywords *)
    | "func"                { FUNC }
    | "package"             { PACKAGE }
    | "import"              { IMPORT }
    | "type"                { TYPE }
    | "struct"              { STRUCT }
    | "return"              { RETURN }
    | "break"               { BREAK }
    | "if"                  { IF }
    | "else"                { ELSE }
    | "continue"            { CONTINUE }
    | "for"                 { FOR }
    | "const"               { CONST }
    | "var"                 { VAR }
    | "make"                { MAKE }
    | "while"               { WHILE }
    | "do"                  { DO }
    | "switch"              { SWITCH }
    | "case"                { CASE }
    | "default"             { DEFAULT }
    | "true"                { TRUE }
    | "false"               { FALSE }
    | "final"               { FINAL }
    | "mut"                 { MUT }
    | "late"                { LATE }
    | "private"             { Private }
    | "get"                 { GET }
    | "post"                { POST }
    | "delete"              { DELETE }
    | "error"               { ERROR }

    (* Integer literals *)
    | uint8_lit             { create_int_lit (Lexing.lexeme lexbuf) "u8" }
    | uint16_lit            { create_int_lit (Lexing.lexeme lexbuf) "u16" }
    | uint32_lit            { create_int_lit (Lexing.lexeme lexbuf) "u32" }
    | uint64_lit            { create_int_lit (Lexing.lexeme lexbuf) "u64" }
    | int8_lit              { create_int_lit (Lexing.lexeme lexbuf) "i8" }
    | int16_lit             { create_int_lit (Lexing.lexeme lexbuf) "i16" }
    | int32_lit             { create_int_lit (Lexing.lexeme lexbuf) "i32" }
    | int64_lit             { create_int_lit (Lexing.lexeme lexbuf) "i64" }
    | int_lit               { INT32_LIT (int_of_string (Lexing.lexeme lexbuf)) }

    (* Float literals *)
    | float32_lit           { create_float_lit (Lexing.lexeme lexbuf) "f32" }
    | float64_lit           { create_float_lit (Lexing.lexeme lexbuf) "f64" }
    | float_lit             { FLOAT32_LIT (float_of_string (Lexing.lexeme lexbuf)) }

    | string_lit            { let s = Lexing.lexeme lexbuf in
                                (* Remove the quotes *)
                                STRING_LIT (String.sub s 1 (String.length s - 2))}

    (* Arithmetic *)
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "/"                   { DIV }
    | "%"                   { MOD }
    | "**"                  { EXP }

    (* Bitwise *)
    | "<<"                  { LSHIFT }
    | ">>"                  { RSHIFT }
    | "^"                   { BITXOR }
    | "|"                   { BITOR }
    | "~"                   { BITNOT }

    (* Assignment *)
    | "="                   { ASSIGN }
    | ":="                  { DECL_ASSIGN }
    | "+="                  { PLUS_ASSIGN }
    | "-="                  { MINUS_ASSIGN }
    | "*="                  { TIMES_ASSIGN }
    | "**="                 { EXP_ASSIGN }
    | "/="                  { DIV_ASSIGN }
    | "%="                  { MOD_ASSIGN }
    | "<<="                 { LSHIFT_ASSIGN }
    | ">>="                 { RSHIFT_ASSIGN }
    | "&="                  { BITAND_ASSIGN }
    | "^="                  { BITXOR_ASSIGN }
    | "|="                  { BITOR_ASSIGN }

    (* Equivalence *)
    | "=="                  { EQ }
    | "!="                  { NEQ }
    | "<"                   { LT }
    | "<="                  { LE }
    | ">"                   { GT }
    | ">="                  { GE }

    (* Logical *)
    | "&&"                  { AND }
    | "||"                  { OR }
    | "!"                   { NOT }

    (* Ambiguous - parser needs to disambiguate *)
    | "&"                   { AMPERSAND }
    | "*"                   { ASTERISK }

    (* Unary *)
    | "++"                  { INC }
    | "--"                  { DEC }
    | "~"                   { TILDE }

    (* Separators *)
    | "("                   { LPAREN }
    | ")"                   { RPAREN }
    | "{"                   { LBRACE }
    | "}"                   { RBRACE }
    | "["                   { LBRACKET }
    | "]"                   { RBRACKET }
    | ";"                   { SEMICOLON }
    | ":"                   { COLON }
    | ","                   { COMMA }
    | "."                   { DOT }
    | "?"                   { QUESTION }

    (* Ocamllex checks rules in order, so this is after keywords *)
    | identifier            { IDENT (Lexing.lexeme lexbuf) }
    | eof                   { EOF }
    | _                     { error (sprintf "Unrecognized token: %s" (Lexing.lexeme lexbuf)) }

and comment = parse
    | "*/"      { token lexbuf }
    | newline   { comment lexbuf }
    | _         { comment lexbuf }
