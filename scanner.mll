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
    | "make"                { emit MAKE }

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
    | "f16"                { emit F16 }
    | "f32"                { emit F32 }

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
    | identifier            { emit (IDENT (Lexing.lexeme lexbuf)) }
    | eof                   { EOF }
    | _ as c                { raise (Failure (Printf.sprintf "Lexing bad char: '%c'" c)) }

and comment = parse
    | "*/"      { token lexbuf }
    | '\n'      { Lexing.new_line lexbuf; comment lexbuf }
    | "/*"      { ignore (comment lexbuf); comment lexbuf }
    | eof       { failwith "unterminated comment" }
    | _         { comment lexbuf }
