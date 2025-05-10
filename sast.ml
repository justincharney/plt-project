(* SAST FOR P.A.T. *)

open Ast

(* ------------------------------------------------------------------ *)
(* S A S T   D E F I N I T I O N S                                    *)
(* ------------------------------------------------------------------ *)

type sexpr = type_expr * sx
and sx =
  | SIntLit      of int
  | SBoolLit     of bool
  | SCharLit     of char
  | SFloatLit    of float
  | SStringLit   of string
  | SNull
  | SId          of string
  | SBinop       of sexpr * biop * sexpr
  | SUnaop       of unop * sexpr
  | SAssignment  of assign_op * sexpr * sexpr
  | SCast        of type_expr * sexpr
  | SArrayLit    of type_expr * sexpr list
  | SStructLit   of type_expr * (string * sexpr) list
  | SFieldAccess of sexpr * string
  | SIndexAccess of sexpr * sexpr
  | SFunctionCall of string * sexpr list
  | SMethodCall  of sexpr * string * sexpr list
  | SContinue
  | SBreak

type sstmt =
  | SExpr      of sexpr
  | SVarDecl   of bool * string * type_expr option * sexpr
  | SIf        of sexpr * sstmt list * sstmt list
  | SFor       of sstmt option * sexpr option * sexpr option * sstmt list
  | SWhile     of sexpr * sstmt list
  | SReturn    of sexpr list

type sparam = {
  sp_name       : string;
  sp_type       : type_expr;
  sp_is_variadic: bool;
}

type sfunc = {
  sf_name         : string;
  sf_params       : sparam list;
  sf_return_types : type_expr list;
  sf_body         : sstmt list;
}

type sstruct_func = {
  ss_struct       : string;
  ss_name         : string;
  ss_params       : sparam list;
  ss_return_types : type_expr list;
  ss_body         : sstmt list;
}

type sprogram = {
  sp_package         : package_decl;
  sp_imports         : import_decl list;
  sp_type_aliases    : (string * type_expr) list;
  sp_structs         : (string * (string * type_expr) list) list;
  sp_globals         : (string * type_expr) list;
  sp_functions       : sfunc list;
  sp_struct_functions: sstruct_func list;
}