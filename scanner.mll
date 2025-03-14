(* Ocamllex scanner *)

{

(* Any OCaml functions defined here will be subsequently available in the remainder of the lexer definition. *)
open Printf
open Lexing

}

type token =
  (* Keywords *)
  | FUNC | PACKAGE | IMPORT | TYPE | STRUCT | RETURN | BREAK | IF | ELSE
  | CONTINUE | FOR | CONST | VAR | MAKE | WHILE | DO | SWITCH | CASE | DEFAULT
  | TRUE | FALSE | FINAL | MUT | LATE | PRIVATE | GET | POST | DELETE | ERROR
  | NULL

  (* Identifiers *)
  | IDENT of string

  (* Literals *)
  | INT_LIT of int
  | FLOAT_LIT of float
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
  | EOL     (* End of line, if needed *)


(* Regular expressions for token components *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t']
let newline = '\n' | '\r' | "\r\n"
let identifier = alpha (alpha | digit | '_')*

(* Integer literals *)
let int_lit = digit+

(* Float literals *)
let float_lit = (digit* '.' digit+) | (digit+ '.' digit*)

(* Match nicely formatted strings. No multi-line *)
let string_lit = '"' ([^ '"' '\\' '\n'])* '"'

let char_lit = '\'' ([^ '"' '\\' '\n']) '\''

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
    | "private"             { PRIVATE }
    | "get"                 { GET }
    | "post"                { POST }
    | "delete"              { DELETE }
    | "error"               { ERROR }
    | "null"                { NULL }

    (* Literals *)
    | int_lit               { INT_LIT (int_of_string (Lexing.lexeme lexbuf)) }
    | float_lit             { FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf)) }
    | string_lit            { let s = Lexing.lexeme lexbuf in
                                (* Remove the quotes *)
                                STRING_LIT (String.sub s 1 (String.length s - 2))}
    | char_lit              { let c = Lexing.lexeme lexbuf in
                                CHAR_LIT}

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
    | _                     { error (sprintf "Unrecognized token: %s" (Lexing.lexeme lexbuf)) }

and comment = parse
    | "*/"      { token lexbuf }
    | newline   { comment lexbuf }
    | _         { comment lexbuf }
