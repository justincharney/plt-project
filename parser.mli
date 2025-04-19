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
  | IDENT of (
# 23 "parser.mly"
        string
# 84 "parser.mli"
)
  | INT_LIT of (
# 24 "parser.mly"
        int
# 89 "parser.mli"
)
  | FLOAT_LIT of (
# 25 "parser.mly"
        float
# 94 "parser.mli"
)
  | STRING_LIT of (
# 26 "parser.mly"
        string
# 99 "parser.mli"
)
  | CHAR_LIT of (
# 27 "parser.mly"
        char
# 104 "parser.mli"
)
  | BOOL_LIT of (
# 28 "parser.mly"
        bool
# 109 "parser.mli"
)
  | EOF
  | UPLUS
  | UMINUS
  | IFX

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
