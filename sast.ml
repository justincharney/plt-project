
open Ast (* re-use primitive_type, biop, … *)

(* --------------------------------------------------------------------- *)
(*  Types                                                                *)
(* --------------------------------------------------------------------- *)
type ty =
  | TyPrim   of primitive_type
  | TyArray  of ty * int
  | TySlice  of ty
  | TyStruct of string
  | TyError

(* A helper for readable printing / debugging *)
let rec string_of_ty = function
  | TyPrim p ->
      (* Map Ast primitive types to their string names *)
      begin match p with
      | Bool -> "bool" | String -> "string"
      | U8 -> "u8" | U16 -> "u16" | U32 -> "u32" | U64 -> "u64"
      | I8 -> "i8" | I16 -> "i16" | I32 -> "i32" | I64 -> "i64"
      | F16 -> "f16" | F32 -> "f32"
      | Error -> "<primitive_error>"
      end
  | TyArray (t, n) -> Printf.sprintf "[%d]%s" n (string_of_ty t)
  | TySlice t      -> Printf.sprintf "[]%s" (string_of_ty t)
  | TyStruct n     -> n
  | TyError        -> "<type_error>"
  | TyVoid         -> "void"

(* --------------------------------------------------------------------- *)
(*  Expressions (each carries its inferred type)                         *)
(* --------------------------------------------------------------------- *)
type sexpr = ty * sx
and sx =
  (* literals *)
  | SIntLit      of int
  | SBoolLit     of bool
  | SCharLit     of char
  | SFloatLit    of float
  | SStringLit   of string
  | SNull
  | SArrayLit    of ty * sexpr list
  | SStructLit   of string * (string * sexpr) list
  | SSliceLit    of ty * sexpr list

  (* l-values and basic primaries *)
  | SIdentifier    of string
  | SFieldAccess   of sexpr * string
  | SIndexAccess   of sexpr * sexpr
  | SSliceExpr     of sexpr * sexpr option * sexpr option

  (* operators *)
  | SBinop         of sexpr * biop * sexpr
  | SUnaop         of unop  * sexpr
  | SSimpleAssign  of sexpr * sexpr
  | SCompoundAssign of sexpr * compound_op * sexpr
  | SSequence      of sexpr * sexpr

  (* calls / built-ins *)
  | SFunctionCall  of string * sexpr list
  | SMethodCall    of sexpr * string * sexpr list
  | SMake          of ty * sexpr * sexpr option
  | SCast          of ty * sexpr

(* --------------------------------------------------------------------- *)
(*  Statements                                                           *)
(* --------------------------------------------------------------------- *)
type sstmt =
  | SExpr     of sexpr
  | SVarDecl  of { is_const: bool; name: string; var_type: ty; init: sexpr option; }
  | SBlock    of sstmt list
  | SIf       of sexpr * sstmt list * sstmt option
  | SWhile    of sexpr * sstmt list
  | SFor      of sstmt option * sexpr option * sexpr option * sstmt list
  | SReturn   of sexpr list option
  | SBreak
  | SContinue

(* --------------------------------------------------------------------- *)
(*  Functions & program                                                  *)
(* --------------------------------------------------------------------- *)

(* Represents a checked parameter *)
type sparam = {
  name : string;
  param_type : ty;
}

type sfield = {
  name : string;
  field_type : ty;
  modifier: type_modifier option; (* From the AST *)
  default_value: sexpr option;
}

type sfunc_decl = {
  name : string;
  params : sparam list;
  return_types : ty list;
  body : sstmt list;
}

(* Represents a checked struct method *)
type sstruct_func = {
  name         : string; (* Method name *)
  struct_name  : string; (* Name of the struct it belongs to *)
  params       : sparam list; (* Regular parameters *)
  return_types : ty list;
  body         : sstmt list;
}

type stype_decl =
  | STypeStruct of string * sfield list
  | STypeAlias of string * ty

(* Represents a checked global variable *)
type sglobal_decl = {
  is_const : bool;
  name : string;
  var_type : ty;
  init : sexpr option;
}

type sprogram = {
  sp_package : package_decl; (* From the AST *)
  sp_imports : import_decl list; (* From the AST *)
  sp_types   : type_decl list; (* From the AST *)
  sp_globals : sglobal_decl list;
  sp_funcs   : sfunc_decl list;
  sp_methods : sstruct_func list;
}
