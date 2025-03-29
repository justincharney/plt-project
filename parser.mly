/* Ocamlyacc parser */

%{ open Ast %}

(* included all necessary tokens *)
%token FUNC PACKAGE IMPORT TYPE STRUCT
%token RETURN BREAK IF ELSE CONTINUE FOR WHILE
%token CONST VAR MAKE ERROR NULL 
%token FINAL MUT LATE PRIVATE GET POST DELETE
%token BOOL STRING U8 U16 U32 U64 I8 I16 I32 I64 F16 F32
%token PLUS MINUS DIV TIMES MOD 
%token LSHIFT RSHIFT BITXOR BITOR BITNOT
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token DECL_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN
%token BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
%token EQ NEQ LT LE GT GE AND OR NOT 
%token AMPERSAND ASTERISK INC DEC 
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COLON COMMA DOT QUESTION IDENT EOF

%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <char> CHAR_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT

// included vast majority of precedence rules
// not included --> DECL_ASSIGN :=
// are we including a ternary operator? :(

%right BITAND_ASSIGN BITOR_ASSIGN BITXOR_ASSIGN
%right LSHIFT_ASSIGN RSHIFT_ASSIGN
%right TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
%right PLUS_ASSIGN MINUS_ASSIGN
%right ASSIGN
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND
%left EQ NEQ
%left LT LE GE GE
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT BITNOT
%right INC DEC
%left INC DEC

%start program
%type <Ast.tokenseq> program

%%

program:
  tokens EOF { $1}

tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }

one_token:
  | SEMI  {  "SEMI" }
  | LPAREN { "LPAREN" }
  | RPAREN { "RPAREN" }
  | LBRACE { "LBRACE" }
  | RBRACE { "RBRACE" }
  | COMMA { "COMMA" }
  | PLUS { "PLUS" }
  | MINUS { "MINUS" }
  | ASSIGN { "ASSIGN" }
  | EQ { "EQ" }
  | NEQ { "NEQ" }
  | LT { "LT" }
  | AND { "AND" }
  | OR { "OR" }
  | IF { "IF" }
  | ELSE { "ELSE" }
  | WHILE { "WHILE" }
  | RETURN { "RETURN" }
  | INT { "INT" }
  | BOOL { "BOOL" }
  | BLIT { "BOOL: " ^ string_of_bool $1 }
  | LITERAL { "LITERAL: " ^ string_of_int $1 }
  | ID { "ID: " ^ $1 }