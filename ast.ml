(* AST FOR P.A.T. *)

(* TYPES *)
type primitive_type =
  | Bool
  | String
  | U8 | U16 | U32 | U64
  | I8 | I16 | I32 | I64
  | F16 | F32
  | Error

(* Represent the non-primitive types in our language *)
type type_expr =
  | Primitive of primitive_type
  | Array of type_expr * int (* element type, length *)
  | Slice of type_expr (* element type *)
  | Struct of string (* struct name *)
  | TypeName of string (* named type - like example 3.6 *)

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
type compound_op =
| PlusAssign | MinusAssign | TimesAssign | DivAssign | ModAssign
| LshiftAssign | RshiftAssign | BitandAssign | BitxorAssign | BitorAssign

(* some basic expression in lang *)
type expr =
  (* Literals *)
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | StringLit of string
  | Null (* For null literal *)
  | ArrayLit of type_expr * expr list (* [3]i32{1, 2, 3} *)
  | StructLit of string * (string * expr) list (* e.g. goody{"funky supreme", 1000} *)
  | SliceLit of type_expr * expr list (* []i32{1, 2, 3} *)

  (* Variables and access *)
  | Identifier of string
  | FieldAccess of expr * string (* For struct.field *)
  | IndexAccess of expr * expr (* array_or_slice_exp[index_exp] *)
  | SliceExpr of expr * expr option * expr option (* arr[start?:end?] *)

  (* Operations *)
  | Binop of expr * biop * expr (* e.g. a + b *)
  | Unaop of unop * expr (* e.g. -x *)
  | SimpleAssign of expr * expr (* For basic assignment: x = y *)
  | CompoundAssign of expr * compound_op * expr  (* For operations like x += y *)

  | Sequence of expr * expr (* expr1; expr2 - Evaluates expr1, then expr2, returns value of expr2 *)

  | FunctionCall of string * expr list (* func_name(arg1, arg2) *)
  | MethodCall of expr * string * expr list (* myStruct.someMethod(arg1, arg2) *)
  | Make of type_expr * expr * expr option (* make([]type, len, cap?) *)

  | Cast of type_expr * expr (* i64(x) *)

(* statement in lang, only decl or expr *)
and stmt =
  | Block of stmt list
  | Expr of expr

  (* Control flow *)
  | IfStmt of expr * stmt list * stmt option  (* condition, then_block, else_block (else can be Block or IfStmt for else if) *)
  | ForStmt of stmt option * expr option * expr option * stmt list (* init; condition; update; body *)
  | WhileStmt of expr * stmt list (* condition; body *)
  | Return of expr list option (* return val1, var2; or return; *)
  | Break
  | Continue

  (* Declarations (infered type) *)
  | VarDecl of {
      is_const: bool; (* true for const, false for var *)
      name: string; (* variable name *)
      var_type: type_expr option; (* None for inferred type with := *)
      initializer_expr: expr option;
    }

(* Top level declarations *)

(* Function parameters *)
type param = {
  name: string;
  param_type: type_expr
}

(* Struct field definition *)
type field = {
  name: string;
  field_type: type_expr;
  modifier: type_modifier option;
  default_value: expr option;
}


(* Function declaration *)
type func_decl = {
  name: string;
  params: param list;
  return_types: type_expr list ;
  body: stmt list;
}

type struct_func = {
  name: string;
  receiver_name: string;
  struct_name: string;
  params: param list;
  return_types: type_expr list;
  body: stmt list;
}

(* Type declaration *)
type type_decl =
  | TypeStruct of string * field list (* struct definition *)
  | TypeAlias of string * type_expr (* type alias *)

(* Package and import declarations *)
type package_decl = string
type import_decl = string

(* global variable declaration - same as regular var decl but only for global scope *)
type global_decl = {
  is_const: bool;
  name: string;
  var_type: type_expr option;
  initializer_expr: expr option;
}

(* Overall program structure *)
type program = {
  package_name: package_decl;
  imports: import_decl list;
  type_declarations: type_decl list;
  global_vars: global_decl list;
  functions: func_decl list; (* enforce main function inclusion during semantic analysis *)
  struct_functions: struct_func list; (* enforce struct functions not overriding within each struct and struct actually exists *)
}
