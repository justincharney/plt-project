(* OCAMLYACC PARSER FOR P.A.T. *)

%{ open Ast %}

(* included all necessary tokens *)
%token FUNC PACKAGE IMPORT TYPE STRUCT
%token RETURN BREAK IF ELSE CONTINUE FOR WHILE
%token CONST VAR MAKE ERROR NULL 
%token FINAL MUT LATE PRIVATE
%token BOOL STRING U8 U16 U32 U64 I8 I16 I32 I64 F32 F64
%token PLUS MINUS DIV MULT MOD 
%token LSHIFT RSHIFT BITXOR BITOR BITNOT BITAND
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token DECL_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN
%token BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
%token EQ NEQ LT LE GT GE AND OR NOT 
%token INC DEC 
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COLON COMMA DOT TRIPLEDOT QUESTION EOF

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
%left MULT DIV MOD
%right NOT BITNOT
%left INC DEC (* x++ *)

%start program
%type <Ast.program> program

%%

program:
  package_decl import_decls type_decls var_decls func_decls struct_func_decls 
  { { package_name = $1;
      imports = $2;
      type_declarations = $3;
      global_vars = $4;
      functions = $5;
      struct_functions = $6 }
  }

/********** PACKAGE **********/
package_decl:
  | PACKAGE IDENT { $2 }

/********** IMPORTS **********/
import_decls:
  | /* nothing */            { [] }
  | import_decls import_decl { $1 :: [$2] }

import_decl:
  | IMPORT STRING_LIT { $2 }

/********** TYPE DECLARATIONS **********/
type_decls:
  | /* nothing */        { [] }
  | type_decls type_decl { $1 :: [$2] }

/*** STRUCTS AND ALIAS ***/
type_decl:
  | STRUCT IDENT LBRACE field_list RBRACE { TypeStruct ($2, $4) } /* why no ID on LRM? */
  | TYPE IDENT ASSIGN type_expr           { TypeAlias ($2, $4) }

field_list:
  | /* nothing */         { [] }
  | field_list field_decl { $1 :: [$2] }

field_decl:
    opt_type_modifier IDENT type_expr opt_default opt_semicolon
    { { name = $2;
        field_type = $3;
        modifier = $1;
        default_value = $4; }
    }

opt_type_modifier:
  | /* nothing */          { None }
  | modifier               { Some $1 }

modifier:
  | PRIVATE                { Private }
  | MUTABLE                { Mutable }
  | FINAL                  { Final }
  | LATE                   { Late }

opt_default:
  | /* nothing */ { [] }
  | ASSIGN expr   { Some $2 }

opt_semicolon:
  | /* nothing */ { [] }
  | SEMICOLON     { $1 }

/********** VARIABLE DECLARATIONS **********/

var_decls:
  | /* nothing */       { [] }
  | var_decls var_decl  { $1 :: [$2]}

var_decl:
  | opt_const type_expr IDENT ASSIGN expr opt_semicolon /* const i64 = 256 */
  { StrictType { 
    is_const = $1;
    name = $3;
    var_type = $2;
    initializer_expr = $5; } }

  | opt_const opt_type_expr IDENT DECL_ASSIGN expr opt_semicolon /* const x := 256 */
  { InferType { 
    is_const = $1;
    name = $3;
    var_type = $2; (* is this needed if inference? *)
    initializer_expr = $5; } }

opt_const: 
  | /* nothing */ { false }
  | CONST         { true }

opt_type_expr:
  | /* nothing */ { None }
  | type_expr     { Some $1 }

/********** FUNCTION DECLARATIONS **********/
func_decls:
  | /* nothing */        { [] }
  | func_decls func_decl { $1 :: $2 }

func_decl:
  | FUNC IDENT LPAREN params RPAREN return_types LBRACE stmts RBRACE 
  { { name = $2;
      params = $4;
      return_types = $6;
      body = $8; }}

params:
  | /* nothing */ { [] }
  | params_list   { $1 }

params_list:
  | param COMMA params_list { $1 :: $3 }
  | param                   { [$1] }

param:
  | IDENT type_expr { { 
    name = $1;
    param_type = $2;
    is_variadic = false; }}
  
  | IDENT TRIPLEDOT type_expr { {
    name = $1;
    param_type = $3;
    is_variadic = true; }}

return_types:
  | /* nothing */   { [] }
  | type_expr_list  { $1 }

type_expr_list:
  | type_expr                      { [$1] }
  | type_expr COMMA type_expr_list { $1 :: $3}

/********** STRUCT-FUNC DECLARATION **********/
struct_func_decls:
  | /* nothing */                      { [] }
  | struct_func_decls struct_func_decl { $1 :: $2 }

struct_func_decl: (* REVIEW THIS, NOT QUITE SURE ON SYNTAX OF A STRUCT-FUNC *)
  | FUNC LPAREN IDENT type_expr RPAREN IDENT LPAREN params RPAREN return_types LBRACE stmts RBRACE {{
  name: $6;
  struct_name: $3;
  params: $8;
  return_types: $10;
  body: $12 }}

stmts:
  | /* nothing */ { [] }
  | stmts stmt    { $1 :: $2}

stmt:
  | expr opt_semicolon                                                     { Expr ($1) }
  | var_decl                                                               { VarDecl ($1) }
  | IF expr LBRACE stmts RBRACE else_block                                 { IfStmt ($2, $4, $6) }
  | FOR opt_stmt SEMICOLON opt_expr SEMICOLON opt_expr LBRACE stmts RBRACE { ForStmt ($2, $4, $6, $8) }
  | WHILE expr LBRACE stmts RBRACE                                         { WhileStmt ($2, $3) }
  | RETURN expr_list opt_semicolon (* MAKE SURE TO REVIEW *)               { Return ($2)} 

(* MAKE SURE TO REVIEW *)
else_block:
  | /* nothing */            { [] }
  | ELSE LBRACE stmts RBRACE { $3 }
  | ELSE IF expr LBRACE stmts RBRACE else_block { IfStmt $3, $5, $7 }

opt_stmt:
  | /* nothing */            { None }
  | var_decl                 { VarDecl ($1) }

opt_expr: 
  | /* nothing */            { None }
  | expr                     { Some $1 }

expr_list:
  | /* nothing */            { [] }
  | expr_list COMMA expr     { $1 :: [$3]}

expr:
| expr PLUS      expr         { Binop($1, Plus, $3) }
| expr MINUS     expr         { Binop($1, Minus, $3) }
| expr MULT      expr         { Binop($1, Mult, $3) }
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
| expr ASSIGN expr            { Assignment ($1, RegAssign, $3) }
| expr DECL_ASSIGN expr       { Assignment ($1, DeclAssign, $3) }
| expr PLUS_ASSIGN expr       { Assignment ($1, PlusAssign, $3) }
| expr MINUS_ASSIGN expr      { Assignment ($1, MinusAssign, $3) }
| expr TIMES_ASSIGN expr      { Assignment ($1, TimesAssign, $3) }
| expr DIV_ASSIGN expr        { Assignment ($1, DivAssign, $3) }
| expr MOD_ASSIGN expr        { Assignment ($1, ModAssign, $3) }
| expr LSHIFT_ASSIGN expr     { Assignment ($1, LshiftAssign, $3) }
| expr RSHIFT_ASSIGN expr     { Assignment ($1, RshiftAssign, $3) }
| expr BITAND_ASSIGN expr     { Assignment ($1, BitandAssign, $3) }
| expr BITXOR_ASSIGN expr     { Assignment ($1, BitxorAssign, $3) }
| expr BITOR_ASSIGN expr      { Assignment ($1, BitorAssign, $3) }
| unaop expr                  { Unaop ($1, $2) }
| expr DOT IDENT              { FieldAccess ($1, Identifier $3) }

| expr LPAREN expr_list RPAREN           { FunctionCall ($1, $3) }
| expr LBRACKET expr RBRACKET            { IndexAccess ($1, $3) }
| expr LBRACKET expr COLON expr RBRACKET { SliceExpr ($1, $3, Some $5) }
| expr LBRACKET expr COLON RBRACKET      { SliceExpr ($1, $3, None) }
| type_expr LPAREN expr RPAREN           { Cast ($1, $3) }
| LPAREN expr RPAREN                     { SubExpr $2 }
| BREAK                                  { Break }
| CONTINUE                               { Continue }
| INT_LIT                                { IntLit($1) }
| BOOL_LIT                               { BoolLit($1) }
| CHAR_LIT                               { CharLit($1) }
| FLOAT_LIT                              { FloatLit($1) }
| STRING_LIT                             { StringLit($1) }
| IDENT                                  { Identifier($1) }
| TRUE                                   { BoolLit true }
| FALSE                                  { BoolLit false }
| NULL                                   { Null }
(* | LBRACKET expr_list RBRACKET            { ArrayLit (??, $2) } STILL NEED TO FIGURE OUT *)

unaop:
  | BITNOT                 { Bitnot }
  | NOT                    { Not }
  | NEG                    { Neg }
  | INC                    { Inc }
  | DEC                    { Dec }

(* NOT DONE *)
%%