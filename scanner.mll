(* OCAMLLEX SCANNER FOR P.A.T. *)

{

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

(* Token type definition exposed to other modules *)

(* Keywords *)
type token =
 | FUNC | PACKAGE | IMPORT | TYPE | STRUCT | RETURN | BREAK | IF | ELSE
 | CONTINUE | FOR | CONST | MAKE | WHILE
 | TRUE | FALSE | FINAL | MUT | LATE | PRIVATE | ERROR
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
 | PLUS | MINUS | DIV | MOD | MULT

 (* Bitwise *)
 | LSHIFT | RSHIFT | BITXOR | BITOR | BITNOT | BITAND

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

 (* Separators *)
 | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET
 | SEMICOLON | COLON | COMMA | DOT

 (* Special tokens *)
 | EOF     (* End of file *)

(* ------------------------------------------------------------------ *)
(*  Automatic-semicolon-insertion (ASI) state                         *)

let need_semi   = ref false   (* true ⇒ a newline can end the stmt   *)
let paren_depth = ref 0       (* nesting of () and []               *)

let ends_stmt = function
  | IDENT _ | INT_LIT _ | FLOAT_LIT _ | STRING_LIT _ | CHAR_LIT _
  | BOOL_LIT _ | NULL
  | RETURN | BREAK | CONTINUE | INC | DEC
  | RPAREN | RBRACKET | RBRACE                       -> true
  | _                                               -> false

let update_nesting = function
  | LPAREN  -> incr paren_depth | RPAREN  -> decr paren_depth
  | LBRACKET-> incr paren_depth | RBRACKET-> decr paren_depth
  | _ -> ()

let emit tok =            (* update flags, then return tok *)
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
                                if !need_semi && !paren_depth = 0 then
                                    (need_semi := false; SEMICOLON)
                                else
                                    token lexbuf }


    (* Comments *)
    | "//" [^ '\n']*        { token lexbuf }
    | "/*"                  { comment lexbuf }

    (*************** KEYWORDS ***************)

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
                                let content = String.sub s 1 (String.length s - 2) in
                                STRING_LIT (interpret_string content) }
    | char_lit              { let c = Lexing.lexeme lexbuf in
                                let content = String.sub c 1 (String.length c - 2) in
                                CHAR_LIT (interpret_char content)}

    (* Arithmetic *)
    | "+"                   { PLUS }
    | "-"                   { MINUS }
    | "/"                   { DIV }
    | "%"                   { MOD }
    | "*"                   { MULT }

    (* Bitwise *)
    | "<<"                  { LSHIFT }
    | ">>"                  { RSHIFT }
    | "^"                   { BITXOR }
    | "|"                   { BITOR }
    | "~"                   { BITNOT }
    | "&"                   { BITAND }

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

    (* Ocamllex checks rules in order, so this is after keywords *)
    | identifier            { IDENT (Lexing.lexeme lexbuf) }
    | eof                   { EOF }
    | _ as c                    { raise (Failure (Printf.sprintf "Lexing bad char: '%c'" c)) }

and comment = parse
    | "*/"      { token lexbuf }
    | '\n'      { Lexing.new_line lexbuf; comment lexbuf }
    | "/*"      { ignore (comment lexbuf); comment lexbuf }
    | eof       { failwith "unterminated comment" }
    | _         { comment lexbuf }
