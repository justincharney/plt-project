type token =
  | PACKAGE
  | IMPORT
  | FUNC
  | TYPE
  | STRUCT
  | RETURN
  | BREAK
  | IF
  | ELSE
  | CONTINUE
  | FOR
  | WHILE
  | CONST
  | MAKE
  | FINAL
  | MUT
  | LATE
  | PRIVATE
  | ERROR
  | TRUE
  | FALSE
  | NULL
  | BOOL
  | STRING
  | U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | F16
  | F32
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | LSHIFT
  | RSHIFT
  | BITAND
  | BITXOR
  | BITOR
  | BITNOT
  | AND
  | OR
  | NOT
  | ASSIGN
  | DECL_ASSIGN
  | PLUS_ASSIGN
  | MINUS_ASSIGN
  | TIMES_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | LSHIFT_ASSIGN
  | RSHIFT_ASSIGN
  | BITAND_ASSIGN
  | BITXOR_ASSIGN
  | BITOR_ASSIGN
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | INC
  | DEC
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | SEMICOLON
  | COLON
  | COMMA
  | DOT
  | IDENT of (string)
  | TYPE_NAME of (string)
  | INT_LIT of (int)
  | FLOAT_LIT of (float)
  | STRING_LIT of (string)
  | CHAR_LIT of (char)
  | BOOL_LIT of (bool)
  | EOF
  | UPLUS
  | UMINUS
  | IFX

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
