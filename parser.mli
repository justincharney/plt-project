type token =
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
  | VAR
  | MAKE
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
  | F16
  | F32
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
  | QUESTION
  | EOF
  | INT_LIT of (
# 21 "parser.mly"
        int
# 85 "parser.mli"
)
  | BOOL_LIT of (
# 22 "parser.mly"
        bool
# 90 "parser.mli"
)
  | CHAR_LIT of (
# 23 "parser.mly"
        char
# 95 "parser.mli"
)
  | FLOAT_LIT of (
# 24 "parser.mly"
        float
# 100 "parser.mli"
)
  | STRING_LIT of (
# 25 "parser.mly"
        string
# 105 "parser.mli"
)
  | IDENT of (
# 26 "parser.mly"
        string
# 110 "parser.mli"
)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
