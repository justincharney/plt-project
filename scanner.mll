(* Ocamllex scanner *)

{

(* Any OCaml functions defined here will be subsequently available in the remainder of the lexer definition. *)
open Printf
open Lexing

}

type token =
  (* Keywords *)
  | FUNC | PACKAGE | IMPORT | TYPE | STRUCT | RETURN | BREAK | IF | ELSE
  | CONTINUE | FOR | CONST | VAR | MAKE | WHILE
  | TRUE | FALSE | FINAL | MUT | LATE | PRIVATE | GET | POST | DELETE | ERROR
  | NULL

  (* Built-in type keywords *)
  | BOOL
  | STRING
  | U8 | U16 | U32 | U64
  | I8 | I16 | I32 | I64
  | F16 | F32

  (* Identifiers *)
  | IDENT of string

  (* Literals *)
  | INT_LIT of int
  | FLOAT_LIT of float
  | BOOL_LIT of bool
  | STRING_LIT of string (* string literal *)
  | CHAR_LIT of char

  (* Operators *)
  (* Arithmetic *)
  | PLUS | MINUS | DIV | MOD
  (* Bitwise *)
  | LSHIFT | RSHIFT | BITXOR | BITOR | BITNOT
  (* Assignment *)
  | ASSIGN | DECL_ASSIGN (* = vs := *)
  | PLUS_ASSIGN | MINUS_ASSIGN | TIMES_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | LSHIFT_ASSIGN | RSHIFT_ASSIGN | BITAND_ASSIGN | BITXOR_ASSIGN | BITOR_ASSIGN
  (* Equivalence *)
  | EQ | NEQ | LT | LE | GT | GE
  (* Logical *)
  | AND | OR | NOT
  (* Unary *)
  | INC | DEC
  (* Ambiguous - BITAND/ADDR_OF or DEREF/TIMES *)
  | AMPERSAND | ASTERISK

  (* Separators *)
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET
  | SEMICOLON | COLON | COMMA | DOT | QUESTION

  (* Special tokens *)
  | EOF     (* End of file *)


(* Regular expressions for token components *)
let digit      = ['0'-'9']
let alpha      = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t']
let newline    = '\n' | '\r' | "\r\n"
let identifier = alpha (alpha | digit | '_')*

(* Literals *)
let int_lit = ['-']? digit+
let float_lit = ['-']? ((digit* '.' digit+) | (digit+ '.' digit*))
(* Boolean literal? Also, define string_lit as 0 or more char_lit? *)
let string_lit = '"' ([^ '"' '\\' '\n'])* '"' (* Match nicely formatted strings. No multi-line *)
let char_lit = '\'' ([^ '\'' '\\' '\n']) '\'' (* Match nicely formatted strings. No multi-line *)

rule token = parse
    (* Whitespace *)
    | whitespace            { token lexbuf }
    | newline               { token lexbuf }

    (* Comments *)
    | "//" [^ '\n']*        { token lexbuf }
    | "/*"                  { comment lexbuf }

    (* Keywords *)
    (* Functions and Packages *)
    | "func"                { FUNC }
    | "package"             { PACKAGE }
    | "import"              { IMPORT }

    (* Types and Structs *)
    | "type"                { TYPE }
    | "struct"              { STRUCT }

    (* Control Flow *)
    | "return"              { RETURN }
    | "break"               { BREAK }
    | "if"                  { IF }
    | "else"                { ELSE }
    | "continue"            { CONTINUE }
    | "for"                 { FOR }
    | "while"               { WHILE }

    (* Constants and Variables *)
    | "const"               { CONST }
    | "var"                 { VAR }
    | "make"                { MAKE }

    (* Boolean Literals *)
    | "true"                { BOOL_LIT(true) }
    | "false"               { BOOL_LIT(false) }

    (* Data Types *)
    | "error"               { ERROR }
    | "null"                { NULL }

    (* Modifiers *)
    | "final"               { FINAL }
    | "mut"                 { MUT }
    | "late"                { LATE }
    | "private"             { PRIVATE }

    (* HTTP Functions*)
    | "GET"                 { GET }
    | "POST"                { POST }
    | "DELETE"              { DELETE }

    (* Built-in types *)
    | "bool"               { BOOL }
    | "string"             { STRING }
    | "u8"                 { U8 }
    | "u16"                { U16 }
    | "u32"                { U32 }
    | "u64"                { U64 }
    | "i8"                 { I8 }
    | "i16"                { I16 }
    | "i32"                { I32 }
    | "i64"                { I64 }
    | "f16"                { F16 }
    | "f32"                { F32 }

    (* Literals *)
    | int_lit               { INT_LIT (int_of_string (Lexing.lexeme lexbuf)) }
    | float_lit             { FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf)) }
    | string_lit            { let s = Lexing.lexeme lexbuf in
                                (* Remove the quotes *)
                                STRING_LIT (String.sub s 1 (String.length s - 2))}
    | char_lit              { let c = Lexing.lexeme lexbuf in
                                CHAR_LIT (String.get c 1)}

    (* Arithmetic *)
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "/"                   { DIV }
    | "%"                   { MOD }

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
    | _                     { raise (Failure (Printf.sprintf "Unrecognized token: %s" (Lexing.lexeme lexbuf))) }

and comment = parse
    | "*/"      { token lexbuf }
    | newline   { comment lexbuf }
    | _         { comment lexbuf }
