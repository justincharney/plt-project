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
  | Pointer of type_expr
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
  | Plus | Minus | Div | Asterisk | Mod
  | Lshift | Rshift | Bitxor | Bitor | Bitand
  | Eq | Neq | Lt | Le | Gt | Ge | And | Or

(* all unary operators in lang *)
(* Renamed to AddrOf and Deref for memory operators *)
type unop =
  | Bitnot | Not | AddrOf | Deref | Neg
  | Inc | Dec

(* all assignment operators in lang *)
type asgn =
| Assign | Decl_assign | Plus_assign | Minus_assign
| Div_assign | Mod_assign | Lshift_assign | Rshift_assign
| Bitand_assign | Bitxor_assign | Bitor_assign

(* Not sure if we want this or just have them as regular FunctionCalls *)
type builtin_func =
  | Println
  | Sprintf
  | Len
  | Cap
  | Assert
  | Exit
  | Append

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
  | SliceExpr of expr * expr * expr (* arr[start:end] *)

  (* Operations *)
  | Binop of expr * biop * expr (* e.g. a + b *)
  | Unaop of unop * expr (* e.g. -x *)
  | SimpleAssign of expr * expr     (* For basic assignment: x = y *)
  | CompoundAssign of expr * biop * expr  (* For operations like x += y *)

  | FunctionCall of string * expr list (* func_name(arg1, arg2) *)
  | BuiltInCall of builtin_func
  | MethodCall of expr * string * expr list (* myStruct.someMethod(arg1, arg2) *)
  | Make of type_expr * expr * expr option (* make([]type, len, cap?) *)

  | Cast of type_expr * expr (* i64(x) *)
  | Malloc of type_expr

(* statement in lang, only decl or expr *)
and stmt =
  | Block of stmt list
  | Expr of expr

  (* Control flow *)
  | IfStmt of expr * stmt list * stmt option  (* condition, then_block, else_block (else can be Block or IfStmt for else if) *)
  | ForStmt of stmt option * expr option * expr option * stmt list (* init; condition; update; body *)
  | WhileStmt of expr * stmt list (* condition; body *)
  | Return of expr option (* return; *)
  | Break
  | Continue

  (* Memory - free is used for its side effect *)
  | Free of expr (* free(pointer_expr) *)

  | Declaration of type_expr * string * expr
  | InfDeclaration of string * expr

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

type http_method = GET | POST | DELETE

(* Function declaration *)
type func_decl = {
  name: string;
  params: param list;
  return_type: type_expr ;
  body: stmt list;
}

(* Type declaration *)
type type_decl =
  | TypeStruct of string * field list (* struct definition *)
  | TypeAlias of string * type_expr (* type alias *)

(* HTTP func declaration *)
type http_func_decl = {
  method_type: http_method;
  path: string list; (* List of path segments like ["ping", "pong"] for /ping/pong *)
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
  global_vars: stmt list;
  functions: func_decl list;
  http_functions: http_func_decl list;
}
