(* SAST FOR P.A.T. *)

open Ast

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
  | Slice of type_expr (* element type *)
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
type sexpr =
  | SSubExpr of sexpr (* ? for parentheses expressions like (1+2)? specifically for parser to parse into these expressions...unsure *)
  
  (* Literals *)
  | SIntLit of int
  | SBoolLit of bool
  | SCharLit of char
  | SFloatLit of float
  | SStringLit of string
  | SArrayLit of sexpr * type_expr * sexpr list (* [3]i32{1, 2, 3} | The {...} is not in LRM *)
  | SStructLit of sexpr * (sexpr * sexpr) list (* e.g. goody{x:"funky supreme", y:1000} *)
  | SSliceLit of type_expr * sexpr list (* []i32{1, 2, 3} | Not in LRM? *)
  | SNull (* For null literal *)

  (* Variables and Field/Index Access *)
  | SIdentifier of string
  | SFieldAccess of sexpr * sexpr (* For struct.field | LRM says identifier . identifier *)
  | SIndexAccess of sexpr * sexpr (* array_or_slice_exp[index_exp] | LRM says identifier '[' expr ']' *)
  | SSliceExpr of sexpr * sexpr * sexpr option (* arr[start:end?]  | LRM says identifier '[' digit+ ':' (digit+)? ']' *)

  (* Operations *)
  | SBinop of sexpr * biop * sexpr (* e.g. a + b *)
  | SUnaop of unop * sexpr (* e.g. -x *)
  | SAssignment of sexpr * assign_op * sexpr  (* For operations like x += y and x = y | NOT CORRECT?*)

  (* Error Expression and Casting *)
  | SCast of type_expr * sexpr (* i64(x) | error("system fail") | f32(1.2) *)

  (* Function and Method Calls*)
  | SFunctionCall of string * sexpr list (* func_name(arg1, arg2) *)
  | SMethodCall of sexpr * sexpr * sexpr list (* myStruct.someMethod(arg1, arg2) *)

  (* Loop Controls*)
  | SContinue
  | SBreak 

(* Struct field definition *)
type field = {
  name: string;
  field_type: type_expr;
  modifier: type_modifier option;
  default_value: sexpr option;
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
    var_type: type_expr option;
    initializer_expr: sexpr;
  }

  | StrictType of { (* i64 x = 2 *)
    is_const: bool;
    name: string;
    var_type: type_expr;
    initializer_expr: sexpr;
  }

(* Top level declarations *)
(* statement in lang, only decl or expr *)
type sstmt =
  | SExpr of sexpr
  | SVarDecl of var_decl

  (* Control flow *)
  | SIfStmt of sexpr * sstmt list * sstmt list  (* condition, then_block, else_block (else can be Block or IfStmt for else if) *)
  | SForStmt of sstmt option * sexpr option * sexpr option * sstmt list (* init; condition; update; body *)
  | SWhileStmt of sexpr * sstmt list (* condition; body *)
  | SReturn of sexpr list (* return val1, var2; or return; *)

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
  body: sstmt list;
}

type struct_func = {
  name: string;
  struct_name: string;
  params: param list;
  return_types: type_expr list;
  body: sstmt list;
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