/* ----------  header ----------------------------------------------------- */
%{
open Ast
%}

/* ----------  tokens ----------------------------------------------------- */
%token PACKAGE IMPORT FUNC TYPE STRUCT RETURN BREAK IF ELSE CONTINUE FOR WHILE
%token CONST MAKE
%token FINAL MUT LATE PRIVATE ERROR
%token TRUE FALSE NULL
%token BOOL STRING
%token U8 U16 U32 U64 I8 I16 I32 I64 F16 F32
%token PLUS MINUS MULT DIV MOD
%token LSHIFT RSHIFT BITAND BITXOR BITOR BITNOT
%token AND OR NOT
%token ASSIGN DECL_ASSIGN
%token PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token LSHIFT_ASSIGN RSHIFT_ASSIGN BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
%token EQ NEQ LT LE GT GE
%token INC DEC
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COLON COMMA DOT
%token <string> IDENT
%token <int>    INT_LIT
%token <float>  FLOAT_LIT
%token <string> STRING_LIT
%token <char>   CHAR_LIT
%token <bool>   BOOL_LIT
%token EOF

/* Dummy tokens used only for precedence annotations */
%token UPLUS UMINUS IFX

/* ----------  precedence & associativity --------------------------------- */
%right ASSIGN DECL_ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN
       MOD_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
%left  OR
%left  AND
%nonassoc EQ NEQ LT LE GT GE
%left  BITOR
%left  BITXOR
%left  BITAND
%left  LSHIFT RSHIFT
%left  PLUS MINUS
%left  MULT DIV MOD
%right NOT BITNOT
%nonassoc UPLUS UMINUS          /* unary + and – */
%left  INC DEC
%left  DOT LBRACKET             /* highest‑level postfix operators             */
%nonassoc IFX                   /* “dangling‑else” fix                        */
%nonassoc ELSE

/* ----------  entry point ------------------------------------------------- */
%start program
%type  <Ast.program> program

%%  /* ---------- grammar rules & semantic actions ------------------------ */

/* === Program hierarchy ================================================== */
program:
    PACKAGE IDENT import_list decl_list EOF
      { let (t,g,f,s) = $4 in
        { package_name      = $2;
          imports           = $3;
          type_declarations = List.rev t;
          global_vars       = List.rev g;
          functions         = List.rev f;
          struct_functions  = List.rev s } }

import_list:
      /* ε */                { [] }
    | import_list import_decl { $2 :: $1 }

import_decl:
      IMPORT STRING_LIT      { $2 }

/* === Top‑level declarations ============================================ */
decl_list:
      /* ε */                 { ([],[],[],[]) }
    | decl_list top_decl      {
        let (tl,gl,fl,sl) = $1 in
        match $2 with
        | `T d  -> (d::tl, gl,     fl,     sl)
        | `G g  -> (tl,    g::gl,  fl,     sl)
        | `F f  -> (tl,    gl,     f::fl,  sl)
        | `S sf -> (tl,    gl,     fl,     sf::sl) }

top_decl:
      type_decl               { `T $1 }
    | global_decl             { `G $1 }
    | func_decl               { `F $1 }
    | struct_func_decl        { `S $1 }

/* ---------- Types ------------------------------------------------------- */
primitive_type:
      BOOL   { Primitive Bool  } | STRING { Primitive String }
    | U8     { Primitive U8    } | U16    { Primitive U16   }
    | U32    { Primitive U32   } | U64    { Primitive U64   }
    | I8     { Primitive I8    } | I16    { Primitive I16   }
    | I32    { Primitive I32   } | I64    { Primitive I64   }
    | F16    { Primitive F16   } | F32    { Primitive F32   }
    | ERROR  { Primitive Error }

type_expr:
      primitive_type                       { $1 }
    | IDENT                                { TypeName $1 }
    | LBRACKET INT_LIT RBRACKET type_expr  { Array($4,$2) }
    | LBRACKET RBRACKET type_expr          { Slice $3 }
    | STRUCT IDENT                         { Struct $2 }   /* reference by name */

/* ---------- Fields (inside struct) -------------------------------------- */
modifier_opt:
      /* ε */      { None }
    | PRIVATE      { Some Private }
    | MUT          { Some Mutable }
    | FINAL        { Some Final }
    | LATE         { Some Late }

field:
      modifier_opt IDENT type_expr default_opt
      { { name=$2; field_type=$3; modifier=$1; default_value=$4 } }

default_opt:
      /* ε */                { None }
    | ASSIGN expr            { Some $2 }

field_list:
      /* ε */                { [] }
    | field_list field SEMICOLON { $2 :: $1 }

/* ---------- Type declarations ------------------------------------------ */
type_decl:
      TYPE IDENT STRUCT LBRACE field_list RBRACE
        { TypeStruct($2,List.rev $5) }
    | TYPE IDENT ASSIGN type_expr
        { TypeAlias($2,$4) }

/* ---------- tiny helper ------------------------------------------------- */
const_opt:
      /* empty */ { false }          /* no  const keyword */
    | CONST       { true  }           /* saw const ⇒ is_const = true */

/* ---------- Global var declarations ------------------------------------ */
global_decl:
      const_opt IDENT type_ann_opt ASSIGN expr
        { { is_const= $1;
            name=$2;
            var_type=$3;
            initializer_expr=Some $5 } }
    | const_opt IDENT DECL_ASSIGN expr
        { { is_const=Option.is_some $1;
            name=$2;
            var_type=None;
            initializer_expr=Some $4 } }

type_ann_opt:
      /* ε */         { None }
    | COLON type_expr { Some $2 }

/* ---------- Function & struct‑function --------------------------------- */
param:
      IDENT type_expr { { name=$1; param_type=$2 } }

param_list:
      /* ε */            { [] }
    | param               { [$1] }
    | param_list COMMA param { $3 :: $1 }

return_types:
      /* ε */                        { [] }
    | type_expr                      { [$1] }
    | LPAREN type_expr_list RPAREN   { List.rev $2 }

type_expr_list:
      type_expr                           { [$1] }
    | type_expr_list COMMA type_expr      { $3 :: $1 }

func_body:
      block { $1 }

func_decl:
      FUNC IDENT LPAREN param_list RPAREN return_types func_body
        { { name=$2; params=List.rev $4; return_types=$6; body=$7 } }

struct_func_decl:
      FUNC LPAREN IDENT IDENT RPAREN IDENT LPAREN param_list RPAREN return_types func_body
        { { name=$6; struct_name=$3;
            params=List.rev $8; return_types=$10; body=$11 } }

/* ---------- Statements -------------------------------------------------- */
block:
      LBRACE stmt_list RBRACE { $2 }

stmt_list:
      /* ε */            { [] }
    | stmt_list stmt      { $2 :: $1 }

stmt:
      expr SEMICOLON                { Expr $1 }
    | var_decl SEMICOLON            { $1 }
    | block                         { $1 }
    | IF expr block %prec IFX       { IfStmt($2, $3, None) }
    | IF expr block ELSE stmt       { IfStmt($2,$3,Some $5) }
    | WHILE expr block              { WhileStmt($2,$3) }
    | FOR for_clause block          { ForStmt($2.init,$2.cond,$2.step,$3) }
    | RETURN ret_opt SEMICOLON      { Return $2 }
    | BREAK SEMICOLON               { Break }
    | CONTINUE SEMICOLON            { Continue }

ret_opt:
      /* ε */           { None }
    | expr_list         { Some (List.rev $1) }

expr_list:
      expr                         { [$1] }
    | expr_list COMMA expr         { $3 :: $1 }

/* simple C‑style three‑field clause */
for_clause:
      expr_opt SEMICOLON expr_opt SEMICOLON expr_opt
        { { init=$1; cond=$3; step=$5 } }

expr_opt:
      /* ε */ { None } | expr { Some $1 }

/* ---------- Local variable decl (stmt) --------------------------------- */
var_decl:
      const_opt IDENT type_ann_opt ASSIGN expr
        { VarDecl{is_const=$1; name=$2; var_type=$3; initializer_expr=Some $5} }
    | const_opt IDENT DECL_ASSIGN expr
        { VarDecl{is_const=$1; name=$2; var_type=None; initializer_expr=Some $4} }

/* ---------- Expressions ------------------------------------------------- */
expr:
      literal                          { $1 }
    | IDENT                            { Identifier $1 }
    | LPAREN expr RPAREN               { $2 }
    | expr DOT IDENT                   { FieldAccess($1,$3) }
    | expr LBRACKET expr RBRACKET      { IndexAccess($1,$3) }
    | expr LBRACKET expr_opt COLON expr_opt RBRACKET
        { SliceExpr($1,$3,$5) }
    | IDENT LPAREN arg_list RPAREN     { FunctionCall($1,$3) }
    | expr DOT IDENT LPAREN arg_list RPAREN
        { MethodCall($1,$3,$5) }

    /* unary */
    | NOT expr                         { Unaop(Not,$2) }
    | BITNOT expr                      { Unaop(Bitnot,$2) }
    | MINUS expr %prec UMINUS          { Unaop(Neg,$2) }
    | INC expr                         { Unaop(Inc,$2) }
    | DEC expr                         { Unaop(Dec,$2) }

    /* binary (handled by precedence) */
    | expr PLUS expr                   { Binop($1,Plus,$3) }
    | expr MINUS expr                  { Binop($1,Minus,$3) }
    | expr MULT expr                   { Binop($1,Mult,$3) }
    | expr DIV expr                    { Binop($1,Div,$3) }
    | expr MOD expr                    { Binop($1,Mod,$3) }
    | expr LSHIFT expr                 { Binop($1,Lshift,$3) }
    | expr RSHIFT expr                 { Binop($1,Rshift,$3) }
    | expr BITAND expr                 { Binop($1,Bitand,$3) }
    | expr BITXOR expr                 { Binop($1,Bitxor,$3) }
    | expr BITOR expr                  { Binop($1,Bitor,$3) }
    | expr AND expr                    { Binop($1,And,$3) }
    | expr OR expr                     { Binop($1,Or,$3) }
    | expr EQ expr                     { Binop($1,Eq,$3) }
    | expr NEQ expr                    { Binop($1,Neq,$3) }
    | expr LT expr                     { Binop($1,Lt,$3) }
    | expr LE expr                     { Binop($1,Le,$3) }
    | expr GT expr                     { Binop($1,Gt,$3) }
    | expr GE expr                     { Binop($1,Ge,$3) }

    /* assignment (RIGHT‑associative, precedence set earlier) */
    | expr ASSIGN expr                 { SimpleAssign($1,$3) }
    | expr PLUS_ASSIGN  expr           { CompoundAssign($1,PlusAssign,$3) }
    | expr MINUS_ASSIGN expr           { CompoundAssign($1,MinusAssign,$3) }
    | expr TIMES_ASSIGN expr           { CompoundAssign($1,TimesAssign,$3) }
    | expr DIV_ASSIGN expr             { CompoundAssign($1,DivAssign,$3) }
    | expr MOD_ASSIGN expr             { CompoundAssign($1,ModAssign,$3) }
    | expr LSHIFT_ASSIGN expr          { CompoundAssign($1,LshiftAssign,$3) }
    | expr RSHIFT_ASSIGN expr          { CompoundAssign($1,RshiftAssign,$3) }
    | expr BITAND_ASSIGN expr          { CompoundAssign($1,BitandAssign,$3) }
    | expr BITXOR_ASSIGN expr          { CompoundAssign($1,BitxorAssign,$3) }
    | expr BITOR_ASSIGN expr           { CompoundAssign($1,BitorAssign,$3) }

    /* make / cast */
    | MAKE LPAREN type_expr COMMA expr cap_opt RPAREN
        { Make($3,$5,$6) }
    | type_expr LPAREN expr RPAREN
        { Cast($1,$3) }

arg_list:
      /* ε */                { [] }
    | expr_list              { List.rev $1 }

cap_opt:
      /* ε */                { None }
    | COMMA expr             { Some $2 }

/* ---------- Literals ---------------------------------------------------- */
literal:
      INT_LIT    { IntLit $1   }
    | FLOAT_LIT  { FloatLit $1 }
    | CHAR_LIT   { CharLit $1  }
    | STRING_LIT { StringLit $1 }
    | BOOL_LIT   { BoolLit $1  }
    | NULL       { Null }

/* ----------------------------------------------------------------------- */
%%
