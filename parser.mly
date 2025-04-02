(* OCAMLYACC PARSER FOR P.A.T. *)

%{ open Ast %}

(* included all necessary tokens *)
%token FUNC PACKAGE IMPORT TYPE STRUCT
%token RETURN BREAK IF ELSE CONTINUE FOR WHILE
%token CONST VAR MAKE ERROR NULL 
%token FINAL MUT LATE PRIVATE GET POST DELETE
%token BOOL STRING U8 U16 U32 U64 I8 I16 I32 I64 F16 F32
%token PLUS MINUS DIV AMPERSAND MOD 
%token LSHIFT RSHIFT BITXOR BITOR BITNOT
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token DECL_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN
%token BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
%token EQ NEQ LT LE GT GE AND OR NOT 
%token AMPERSAND ASTERISK INC DEC 
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COLON COMMA DOT QUESTION EOF

%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <char> CHAR_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <string> IDENT

%left SEMICOLON
%right BITAND_ASSIGN BITOR_ASSIGN BITXOR_ASSIGN
%right LSHIFT_ASSIGN RSHIFT_ASSIGN
%right TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
%right PLUS_ASSIGN MINUS_ASSIGN
%right ASSIGN DECL_ASSIGN
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND
%left EQ NEQ
%left LT LE GT GE
%left LSHIFT RSHIFT
%left PLUS MINUS
%left AMPERSAND DIV MOD
%right NOT BITNOT
%left INC DEC (* x++ *)

%start program
%type <Ast.expr> program

%%

program:
  expr EOF { $1 }

expr:
| expr PLUS      expr         { Binop($1, Plus, $3) }
| expr MINUS     expr         { Binop($1, Minus, $3) }
| expr ASTERISK     expr      { Binop($1, Asterisk, $3) }
| expr DIV       expr         { Binop($1, Div, $3) }
| expr MOD       expr         { Binop($1, Mod, $3) }
| expr LSHIFT    expr         { Binop($1, Lshift, $3) }
| expr RSHIFT    expr         { Binop($1, Rshift, $3) }
| expr BITXOR    expr         { Binop($1, Bitxor, $3) }
| expr BITOR     expr         { Binop($1, Bitor, $3) }
| expr BITAND    expr         { Binop($1, Bitand, $3) }
| expr EQ        expr         { Binop($1, Eq, $3) }
| expr NEQ       expr         { Binop($1, Neq, $3) }
| expr LT        expr         { Binop($1, Lt, $3) }
| expr LE        expr         { Binop($1, Le, $3) }
| expr GT        expr         { Binop($1, Gt, $3) }
| expr GE        expr         { Binop($1, Ge, $3) }
| expr AND       expr         { Binop($1, And, $3) }
| expr OR        expr         { Binop($1, Or, $3) }
| expr SEMICOLON expr         { Sequence($1, $3)} (* sequencing *)
| IDENT ASSIGN   expr         { Assign($1, $3)} (* assignment *)
| INT_LIT                     { IntLit($1) }
| BOOL_LIT                    { BoolLit($1) }
| CHAR_LIT                    { CharLit($1) }
| FLOAT_LIT                   { FloatLit($1) }
| STRING_LIT                  { StringLit($1) }
| IDENT                       { Identifier($1) }

(* NOT DONE *)