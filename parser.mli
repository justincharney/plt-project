type token =
  | DOUBLECOLON
  | EOF
  | FUNC
  | PACKAGE
  | IMPORT
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
  | ERROR
  | NULL
  | FINAL
  | MUT
  | LATE
  | PRIVATE
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
  | F32
  | F64
  | PLUS
  | MINUS
  | DIV
  | MULT
  | MOD
  | LSHIFT
  | RSHIFT
  | BITXOR
  | BITOR
  | BITNOT
  | BITAND
  | ASSIGN
  | PLUS_ASSIGN
  | MINUS_ASSIGN
  | TIMES_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | DECL_ASSIGN
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
  | AND
  | OR
  | NOT
  | INC
  | DEC
  | NEG
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
  | TRIPLEDOT
  | QUESTION
  | INT_LIT of (int)
  | BOOL_LIT of (bool)
  | CHAR_LIT of (char)
  | FLOAT_LIT of (float)
  | STRING_LIT of (string)
  | IDENT of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
