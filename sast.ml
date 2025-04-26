(* SAST FOR P.A.T. *)

open Ast

(* Represent the non-primitive types in our language *)
type stype_expr =
  | SPrimitive of primitive_type
  | SArray of stype_expr * int (* element type, length *)
  | SSlice of stype_expr (* element type *)
  | STypeName of string (* refer to Example 3.6 *)

(* some basic expression in lang *)
type sexpr = stype_expr * sx
and sx =
  | SSubExpr of sexpr (* ? for parentheses expressions like (1+2)? specifically for parser to parse into these expressions...unsure *)
  (* Literals *)
  | SIntLit of int
  | SBoolLit of bool
  | SCharLit of char (* does this even exist in the language? *)
  | SFloatLit of float
  | SStringLit of string
  | SArrayLit of sexpr * stype_expr * sexpr list (* [3]i32{1, 2, 3} | The {...} is not in LRM *)
  | SStructLit of sexpr * (sexpr * sexpr) list (* e.g. goody{x:"funky supreme", y:1000} *)
  | SSliceLit of stype_expr * sexpr list (* []i32{1, 2, 3} | Not in LRM? *)
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
  | SCast of stype_expr * sexpr (* i64(x) | error("system fail") | f32(1.2) *)

  (* Function and Method Calls*)
  | SFunctionCall of string * sexpr list (* func_name(arg1, arg2) *)
  | SMethodCall of sexpr * sexpr * sexpr list (* myStruct.someMethod(arg1, arg2) *)

  (* Loop Controls*)
  | SContinue
  | SBreak 

(* Struct field definition *)
type sfield = {
  sname: string;
  sfield_type: stype_expr;
  smodifier: type_modifier;
  sdefault_value: sexpr option;
}

(* Type declaration *)
type stype_decl =
  | STypeStruct of string * sfield list (* struct definition *)
  | STypeAlias of string * stype_expr (* type alias *)

(* Variable declaration *)
type svar_decl = (* x = i64(2) *)
  | SInferType of {
    sis_const: bool;
    sname: string;
    svar_type: stype_expr option;
    sinitializer_expr: sexpr;
  }

  | SStrictType of { (* i64 x = 2 *)
    sis_const: bool;
    sname: string;
    svar_type: stype_expr;
    sinitializer_expr: sexpr option;
  }

(* Top level declarations *)
(* statement in lang, only decl or expr *)
type sstmt =
  | SExpr of sexpr
  | SVarDecl of svar_decl

  (* Control flow *)
  | SIfStmt of sexpr * sstmt list * sstmt list  (* condition, then_block, else_block (else can be Block or IfStmt for else if) *)
  | SForStmt of sstmt option * sexpr option * sexpr option * sstmt list (* init; condition; update; body *)
  | SWhileStmt of sexpr * sstmt list (* condition; body *)
  | SReturn of sexpr list (* return val1, var2; or return; *)

(* Function parameters *)
type sparam = {
  sname: string;
  sparam_type: stype_expr;
  sis_variadic: bool; (* in semantic checker must check that if param variadic also last param in the list *)
}

(* Function declaration *)
type sfunc_decl = {
  sname: string;
  sparams: sparam list;
  sreturn_types: stype_expr list;
  sbody: sstmt list;
}

type sstruct_func = {
  sname: string;
  sstruct_name: string;
  sparams: sparam list;
  sreturn_types: stype_expr list;
  sbody: sstmt list;
}

(* Package and import declarations *)
type spackage_decl = string
type simport_decl = string

(* Overall program structure *)
type sprogram = {
  spackage_name: spackage_decl;
  simports: simport_decl list;
  stype_declarations: stype_decl list;
  sglobal_vars: svar_decl list;
  sfunctions: sfunc_decl list; (* enforce main function inclusion during semantic analysis *)
  sstruct_functions: sstruct_func list; (* enforce struct functions not overriding within each struct and struct actually exists *)
}
