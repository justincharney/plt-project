(* AST FOR P.A.T. *)

(* TYPES *)
type primitive_type =
  | Bool
  | String
  | U8 | U16 | U32 | U64
  | I8 | I16 | I32 | I64
  | F32 | F64
  | Error

(* Represent the non-primitive types in our language *)
type type_expr =
  | Primitive of primitive_type
  | Pointer of type_expr
  | Array of type_expr * int (* element type, length *)
  | Slice of type_expr (* element type *)
  | Struct of string (* struct name *)
  | TypeName of string (* ??? where is this used? named type - like example 3.6 *)

(* Modifiers *)
type type_modifier =
  | Private
  | Mutable
  | Final
  | Late

(* OPERATORS *)
(* all binary operators in lang *)
type biop =
  | Plus | Minus | Div | Mult | Mod
  | Lshift | Rshift | Bitxor | Bitor | Bitand
  | Eq | Neq | Lt | Le | Gt | Ge | And | Or

(* all unary operators in lang *)
type unop =
  | Bitnot | Not | Neg
  | Inc | Dec

(* compound operators in lang *)
type assign_op =
| RegAssign | DeclAssign
| PlusAssign | MinusAssign | TimesAssign | DivAssign | ModAssign
| LshiftAssign | RshiftAssign | BitandAssign | BitxorAssign | BitorAssign

(* some basic expression in lang *)
type expr =
  | SubExpr of expr (* ? for parentheses expressions like (1+2)? specifically for parser to parse into these expressions...unsure *)
  (* Literals *)
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | StringLit of string
  | Null (* For null literal *)
  | ArrayLit of type_expr * expr list (* [3]i32{1, 2, 3} *)
  | StructLit of expr * (expr * expr) list (* e.g. goody{x:"funky supreme", y:1000} *)
  | SliceLit of type_expr * expr list (* []i32{1, 2, 3} *)

  (* Variables and access *)
  | Identifier of string
  | FieldAccess of expr * expr (* For struct.field *)
  | IndexAccess of expr * expr (* array_or_slice_exp[index_exp] *)
  | SliceExpr of expr * expr * expr option (* arr[start:end?] *)

  (* Operations *)
  | Binop of expr * biop * expr (* e.g. a + b *)
  | Unaop of unop * expr (* e.g. -x *)
  | Assignment of expr * assign_op * expr  (* For operations like x += y | x = y | x &= y *)

  | FunctionCall of expr * expr list (* func_name(arg1, arg2) *)
  | MethodCall of expr * expr * expr list (* myStruct.someMethod(arg1, arg2) *)

  | Cast of type_expr * expr (* i64(x) | error("system fail") | f32(1.2) *)

  | Break 
  | Continue

(* Struct field definition *)
type field = {
  name: string;
  field_type: type_expr;
  modifier: type_modifier option;
  default_value: expr option;
}

(* Type declaration *)
type type_decl =
  | TypeStruct of string * field list (* struct definition *)
  | TypeAlias of string * type_expr (* type alias *)

(* Variable declaration *)
type var_decl = (* x = i64(2) *)
  | InferType of {
    is_const: bool;
    name: string;
    var_type: type_expr option; (* is this needed if this inference? *)
    initializer_expr: expr;
  }

  | StrictType of { (* i64 x = 2 *)
    is_const: bool;
    name: string;
    var_type: type_expr;
    initializer_expr: expr;
  }

(* Top level declarations *)
(* statement in lang, only decl or expr *)
type stmt =
  | Expr of expr
  | VarDecl of var_decl

  (* Control flow *)
  | IfStmt of expr * stmt list * stmt list  (* condition, then_block, else_block (else can be Block or IfStmt for else if) *)
  | ForStmt of stmt option * expr option * expr option * stmt list (* init; condition; update; body *)
  | WhileStmt of expr * stmt list (* condition; body *)
  | Return of expr list (* return val1, var2; or return; *)

(* Function parameters *)
type param = {
  name: string;
  param_type: type_expr;
  is_variadic: bool; (* in semantic checker must check that if param variadic also last param in the list *)
}

(* Function declaration *)
type func_decl = {
  name: string;
  params: param list;
  return_types: type_expr list;
  body: stmt list;
}

type struct_func = {
  name: string;
  struct_name: string;
  params: param list;
  return_types: type_expr list;
  body: stmt list;
}

(* Package and import declarations *)
type package_decl = string
type import_decl = string

(* Overall program structure *)
type program = {
  package_name: package_decl;
  imports: import_decl list;
  type_declarations: type_decl list;
  global_vars: var_decl list;
  functions: func_decl list; (* enforce main function inclusion during semantic analysis *)
  struct_functions: struct_func list; (* enforce struct functions not overriding within each struct and struct actually exists *)
}
