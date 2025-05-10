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
  | Array of type_expr * int (* element type, length *)
  | TypeName of string (* refer to Example 3.6 *)

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
(* Note: Can further break down by creating a literal type, but we optd not to.*)
type expr =
  | SubExpr of expr (* For expressions like (1 + (2 * 3)) *)
  
  (* Literals *)
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | StringLit of string
  | ArrayLit of expr * type_expr * expr list (* [3]i32{1, 2, 3} *)
  | StructLit of expr * (expr * expr) list (* e.g. goody{x:"funky supreme", y:1000} *)
  | Null (* For null literal *)

  (* Variables and Field/Index Access *)
  | Identifier of string
  | FieldAccess of expr * expr (* For struct.field *)
  | IndexAccess of expr * expr (* array_exp[index_exp] *)

  (* Operations *)
  | Binop of expr * biop * expr (* e.g. a + b *)
  | Unaop of unop * expr (* e.g. -x *)
  | Assignment of expr * assign_op * expr  (* For operations like x += y and x = y *)

  (* Error Expression and Casting *)
  | Cast of type_expr * expr (* i64(x) | error("system fail") | f32(1.2) *)

  (* Function and Method Calls *)
  | FunctionCall of string * expr list (* func_name(arg1, arg2) *)
  | MethodCall of expr * expr * expr list (* myStruct.someMethod(arg1, arg2) *)

  (* Loop Controls *)
  | Continue
  | Break 

(* Struct field definition *)
type field = {
  name: string;
  field_type: type_expr;
  modifier: type_modifier;
  default_value: expr option;
}

(* Type declaration *)
type type_decl =
  | TypeStruct of string * field list (* struct definition *)
  | TypeAlias of string * type_expr (* type alias *)

(* Variable declaration *)
(* note: non-initialized variables are not allowed when type is inferred *)
type var_decl = 
  | InferType of { (* x = i64(2) *)
    is_const: bool;
    name: string;
    var_type: type_expr option;
    initializer_expr: expr;
  }

  | StrictType of { (* i64 x = 2 *)
    is_const: bool;
    name: string;
    var_type: type_expr;
    initializer_expr: expr option;
  }

(* Top level declarations *)
(* statement in lang, only decl or expr *)
type stmt =
  | Expr of expr
  | VarDecl of var_decl

  (* Control flow *)
  | IfStmt of expr * stmt list * stmt list  (* condition, then_block, else_block *)
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
