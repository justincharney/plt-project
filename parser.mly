/* ----------  header ----------------------------------------------------- */
%{
open Ast
%}

/* ----------  tokens ----------------------------------------------------- */
%token PACKAGE IMPORT FUNC TYPE STRUCT RETURN BREAK IF ELSE CONTINUE FOR WHILE
%token CONST
%token FINAL MUT LATE PRIVATE ERROR
%token TRUE FALSE NULL
%token BOOL STRING
%token U8 U16 U32 U64 I8 I16 I32 I64 F32 F64
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
%token <string> TYPE_NAME
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
%nonassoc CAST
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
%type <unit> start_expr_brace
%type <unit> end_expr_brace

%%  /* ---------- grammar rules & semantic actions ------------------------ */

/* --- ASI Helper Productions --- */
start_expr_brace: /* empty */ { Scanner_state.enter_expr_brace () }
end_expr_brace:   /* empty */ { Scanner_state.exit_expr_brace () }

/* === Program hierarchy ================================================== */
program:
    PACKAGE IDENT SEMICOLON import_list decl_list EOF
      { let (t,g,f,s) = $5 in
        { package_name      = $2;
          imports           = $4;
          type_declarations = List.rev t;
          global_vars       = List.rev g;
          functions         = List.rev f;
          struct_functions  = List.rev s } }

import_list:
      /* None */                { [] }
    | import_list import_decl { $2 :: $1 }

import_decl:
      IMPORT STRING_LIT SEMICOLON      { $2 }

/* === Top‑level declarations ============================================ */
decl_list:
      /* None */                 { ([],[],[],[]) }
    | decl_list SEMICOLON       { $1 } /* Swallow any top-level semicolons */
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
    | F32    { Primitive F32   } | F64    { Primitive F64   }
    | ERROR  { Primitive Error }

type_expr:
      primitive_type                       { $1 }
    | TYPE_NAME                            { TypeName $1 }
    | LBRACKET INT_LIT RBRACKET type_expr  { Array($4,$2) }
    /*| LBRACKET RBRACKET type_expr          { Slice $3 }*/

/* ---------- Fields (inside struct) -------------------------------------- */
modifier_opt:
      /* None */      { None }
    | PRIVATE      { Some Private }
    | MUT          { Some Mutable }
    | FINAL        { Some Final }
    | LATE         { Some Late }

field:
      modifier_opt IDENT COLON type_expr default_opt
      { { name=$2; field_type=$4; modifier=$1; default_value=$5 } }

default_opt:
      /* None */                { None }
    | ASSIGN expr            { Some $2 }

field_list:
      /* None */                { [] }
    | field_list field { $2 :: $1 }

/* ---------- Type declarations ------------------------------------------ */
type_decl:
      TYPE IDENT STRUCT LBRACE field_list RBRACE
        {
            Scanner_state.register_type_name $2;
            TypeStruct($2,List.rev $5)
        }
    | TYPE IDENT ASSIGN type_expr
        {
            Scanner_state.register_type_name $2;
            TypeAlias($2,$4)
        }

/* ---------- Global var declarations ------------------------------------ */
global_decl:
      CONST IDENT type_ann_opt ASSIGN expr
        { { is_const=true;
            name=$2;
            var_type=$3;
            initializer_expr=Some $5 } }
    | IDENT DECL_ASSIGN expr
        { { is_const=false;
            name=$1;
            var_type=None;
            initializer_expr=Some $3 } }

type_ann_opt:
      /* None */         { None }
    | COLON type_expr { Some $2 }

/* ---------- Function & struct‑function --------------------------------- */
param:
      IDENT COLON type_expr { { name=$1; param_type=$3 } }

param_list:
      /* None */            { [] }
    | param               { [$1] }
    | param_list COMMA param { $3 :: $1 }

return_types:
      /* None */                        { [] }
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
      FUNC LPAREN IDENT COLON TYPE_NAME RPAREN IDENT LPAREN param_list RPAREN return_types func_body
        { { name=$7; receiver_name = $3; struct_name=$5;
            params=List.rev $9; return_types=$11; body=$12 } }

/* ---------- Statements -------------------------------------------------- */
block:
    LBRACE stmt_list RBRACE { $2 }

stmt_list:
      /* None */            { [] }
    | stmt_list stmt      { $2 :: $1 }

stmt:
      expr SEMICOLON                { Expr $1 }
    | var_decl SEMICOLON            { $1 }
    | block                         { Block $1 }
    | IF expr block %prec IFX       { IfStmt($2, $3, None) }
    | IF expr block ELSE stmt       { IfStmt($2,$3,Some $5) }
    | WHILE expr block              { WhileStmt($2,$3) }
    | FOR for_clause block          { let (init, cond, step) = $2 in ForStmt(init, cond, step, $3) }
    | RETURN ret_opt SEMICOLON      { Return $2 }
    | BREAK SEMICOLON               { Break }
    | CONTINUE SEMICOLON            { Continue }
    | SEMICOLON                     { Block [] }

ret_opt:
      /* None */           { None }
    | expr_list         { Some (List.rev $1) }

expr_list_opt:
    /* None */         { [] }
    | expr_list        { $1 }
    | expr_list COMMA  { $1 }

expr_list:
      expr                         { [$1] }
    | expr_list COMMA expr         { $3 :: $1 }

/* simple C‑style three‑field clause */
for_clause:
      expr_opt SEMICOLON expr_opt SEMICOLON expr_opt
        { (* The init part must be a statement. Wrap expr in Expr stmt *)
          (match $1 with
           | None -> None
           | Some e -> Some (Expr e)),
          (* Condition must be an expression *)
          $3,
          (* Step part must be an expression *)
          $5 }
    | var_decl SEMICOLON expr_opt SEMICOLON expr_opt
        { (* Init part is already a VarDecl statement *)
          (Some $1),
          (* Condition must be an expression *)
          $3,
          (* Step part must be an expression *)
          $5 }

expr_opt:
      /* None */ { None } | expr { Some $1 }

/* ---------- Local variable decl (stmt) --------------------------------- */

var_decl:
      CONST IDENT type_ann_opt ASSIGN expr
        { VarDecl{is_const=true; name=$2; var_type=$3; initializer_expr=Some $5} }
    | IDENT COLON type_expr ASSIGN expr
        { VarDecl{is_const=false; name=$1; var_type=Some $3; initializer_expr=Some $5} }
    | IDENT COLON type_expr
        { VarDecl{is_const=false; name=$1; var_type=Some $3; initializer_expr=None} }
    | IDENT DECL_ASSIGN expr
        { VarDecl{is_const=false; name=$1; var_type=None; initializer_expr=Some $3} }
/* ---------- Expressions ------------------------------------------------- */

/* An lvalue represents an expression that can appear on the left side of an assignment */
lvalue:
      IDENT                         { Identifier $1 }         /* Simple variable */
    | expr DOT IDENT                { FieldAccess($1, $3) }   /* Struct field access */
    | expr LBRACKET expr RBRACKET   { IndexAccess($1, $3) }   /* Array element access */
  ;

expr:
      literal                          { $1 }
    | IDENT                            { Identifier $1 }
    | LPAREN type_expr RPAREN expr %prec CAST
                                       { Cast($2, $4) }
    | TYPE_NAME start_expr_brace LBRACE field_assign_list_opt RBRACE end_expr_brace
                                       { StructLit($1, List.rev $4) }
    | LPAREN expr RPAREN               { $2 }
    | expr DOT IDENT                   { FieldAccess($1,$3) }
    | expr LBRACKET expr RBRACKET      { IndexAccess($1,$3) }
    /*| expr LBRACKET expr_opt COLON expr_opt RBRACKET
        { SliceExpr($1,$3,$5) }*/
    | IDENT LPAREN arg_list RPAREN     { FunctionCall($1,$3) }
    | expr DOT IDENT LPAREN arg_list RPAREN
        { MethodCall($1,$3,$5) }
    | LBRACKET INT_LIT RBRACKET type_expr start_expr_brace LBRACE expr_list_opt RBRACE end_expr_brace
        { let arr_ty = Array($4, $2) in ArrayLit(arr_ty, List.rev $7) }
    /*| LBRACKET RBRACKET type_expr LBRACE expr_list_opt RBRACE
        { let slice_ty = Slice($3) in SliceLit(slice_ty, List.rev $5) }*/

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
    | lvalue ASSIGN expr                 { SimpleAssign($1,$3) }
    | lvalue PLUS_ASSIGN  expr           { CompoundAssign($1,PlusAssign,$3) }
    | lvalue MINUS_ASSIGN expr           { CompoundAssign($1,MinusAssign,$3) }
    | lvalue TIMES_ASSIGN expr           { CompoundAssign($1,TimesAssign,$3) }
    | lvalue DIV_ASSIGN expr             { CompoundAssign($1,DivAssign,$3) }
    | lvalue MOD_ASSIGN expr             { CompoundAssign($1,ModAssign,$3) }
    | lvalue LSHIFT_ASSIGN expr          { CompoundAssign($1,LshiftAssign,$3) }
    | lvalue RSHIFT_ASSIGN expr          { CompoundAssign($1,RshiftAssign,$3) }
    | lvalue BITAND_ASSIGN expr          { CompoundAssign($1,BitandAssign,$3) }
    | lvalue BITXOR_ASSIGN expr          { CompoundAssign($1,BitxorAssign,$3) }
    | lvalue BITOR_ASSIGN expr           { CompoundAssign($1,BitorAssign,$3) }

    /* make (slice constructor) */
    /*| MAKE LPAREN type_expr COMMA expr cap_opt RPAREN
        { Make($3,$5,$6) }*/

field_assign_list_opt:
      /* None */                { [] }
    | field_assign_list         { $1 }

field_assign_list:
      field_assign               { [$1] }
    | field_assign_list COMMA field_assign { $3 :: $1 }
    | field_assign_list COMMA             { $1 }

field_assign:
      IDENT COLON expr          { ($1, $3) }

arg_list:
      /* None */               { [] }
    | expr_list              { List.rev $1 }

/* cap_opt removed (only used in make) */

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
