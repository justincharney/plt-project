
open Ast (* re-use primitive_type, biop, … *)

(* --------------------------------------------------------------------- *)
(*  Types                                                                *)
(* --------------------------------------------------------------------- *)
type ty =
  | TyPrim   of primitive_type
  | TyArray  of ty * int
  (* | TySlice  of ty *)
  | TyStruct of string
  | TyTuple  of ty list
  | TyNull
  | TyUnit
  | TyError

(* A helper for readable printing / debugging *)
let rec string_of_ty = function
  | TyPrim p ->
      (* Map Ast primitive types to their string names *)
      begin match p with
      | Bool -> "bool" | String -> "string"
      | U8 -> "u8" | U16 -> "u16" | U32 -> "u32" | U64 -> "u64"
      | I8 -> "i8" | I16 -> "i16" | I32 -> "i32" | I64 -> "i64"
      | F32 -> "f32" | F64 -> "f64"
      | Error -> "<primitive_error>"
      end
  | TyArray (t, n) -> Printf.sprintf "[%d]%s" n (string_of_ty t)
  (* | TySlice t      -> Printf.sprintf "[]%s" (string_of_ty t) *)
  | TyStruct n     -> n
  | TyNull         -> "null"
  | TyUnit         -> "unit" (* For functions without returns *)
  | TyTuple ts     -> Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_ty ts))
  | TyError        -> "<type_error>"

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
  (* | SSliceLit    of ty * sexpr list *)

  (* l-values and basic primaries *)
  | SIdentifier    of string
  | SFieldAccess   of sexpr * string
  | SIndexAccess   of sexpr * sexpr
  (* | SSliceExpr     of sexpr * sexpr option * sexpr option *)

  (* operators *)
  | SBinop         of sexpr * biop * sexpr
  | SUnaop         of unop  * sexpr
  | SSimpleAssign  of sexpr * sexpr
  | SCompoundAssign of sexpr * compound_op * sexpr
  | SSequence      of sexpr * sexpr

  (* calls / built-ins *)
  | SFunctionCall  of string * sexpr list
  | SMethodCall    of sexpr * string * sexpr list
  (*| SMake          of ty * sexpr * sexpr option*)
  | SCast          of ty * sexpr

(* --------------------------------------------------------------------- *)
(*  Statements                                                           *)
(* --------------------------------------------------------------------- *)
type sstmt =
  | SExpr     of sexpr
  | SVarDecl  of { is_const: bool; name: string; var_type: ty; initializer_expr: sexpr option; }
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
  receiver_name: string; (* Name of the receiver *)
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
  initializer_expr : sexpr option;
}

type sprogram = {
  sp_package : package_decl; (* From the AST *)
  sp_imports : import_decl list; (* From the AST *)
  sp_types   : stype_decl list;
  sp_globals : sglobal_decl list;
  sp_funcs   : sfunc_decl list;
  sp_methods : sstruct_func list;
}

(* --------------------------------------------------------------------- *)
(*  Helper functions for converting AST operators to strings.                                                 *)
(* --------------------------------------------------------------------- *)

let _sast_string_of_biop = function
  | Plus -> "+" | Minus -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | And -> "&&" | Or -> "||"
  | Lshift -> "<<" | Rshift -> ">>" | Bitand -> "&" | Bitor -> "|" | Bitxor -> "^"

let _sast_string_of_unop = function
  | Neg -> "-" | Not -> "!" | Bitnot -> "~"
  | Inc -> "++" (* Ensure these match your Ast.unop definition *)
  | Dec -> "--" (* Ensure these match your Ast.unop definition *)

let _sast_string_of_compound_op = function
  | PlusAssign -> "+=" | MinusAssign -> "-=" | TimesAssign -> "*=" | DivAssign -> "/=" | ModAssign -> "%="
  | LshiftAssign -> "<<=" | RshiftAssign -> ">>=" | BitandAssign -> "&=" | BitxorAssign -> "^=" | BitorAssign -> "|="

let rec string_of_sexpr_node s : string =
  match s with
  | SIntLit i      -> Printf.sprintf "SIntLit(%d)" i
  | SBoolLit b     -> Printf.sprintf "SBoolLit(%b)" b
  | SCharLit c     -> Printf.sprintf "SCharLit('%C')" c (* Use %C for OCaml char literals if preferred *)
  | SFloatLit f    -> Printf.sprintf "SFloatLit(%f)" f
  | SStringLit str -> Printf.sprintf "SStringLit(\"%s\")" (String.escaped str)
  | SNull          -> "SNull"
  | SArrayLit (ty, sexprs) ->
      Printf.sprintf "SArrayLit(%s, [%s])"
        (string_of_ty ty)
        (String.concat "; " (List.map string_of_sexpr sexprs))
  | SStructLit (name, fields) ->
      Printf.sprintf "SStructLit(%s, {%s})"
        name
        (String.concat "; " (List.map (fun (fname, se) -> Printf.sprintf "%s = %s" fname (string_of_sexpr se)) fields))
  | SIdentifier id -> Printf.sprintf "SIdentifier(%s)" id
  | SFieldAccess (se, field) ->
      Printf.sprintf "SFieldAccess(%s, %s)" (string_of_sexpr se) field
  | SIndexAccess (se1, se2) ->
      Printf.sprintf "SIndexAccess(%s, %s)" (string_of_sexpr se1) (string_of_sexpr se2)
  | SBinop (se1, op, se2) ->
      Printf.sprintf "SBinop(%s, %s, %s)"
        (string_of_sexpr se1)
        (_sast_string_of_biop op)
        (string_of_sexpr se2)
  | SUnaop (op, se) ->
      Printf.sprintf "SUnaop(%s, %s)"
        (_sast_string_of_unop op)
        (string_of_sexpr se)
  | SSimpleAssign (se1, se2) ->
      Printf.sprintf "SSimpleAssign(%s, %s)" (string_of_sexpr se1) (string_of_sexpr se2)
  | SCompoundAssign (se1, op, se2) ->
      Printf.sprintf "SCompoundAssign(%s, %s, %s)"
        (string_of_sexpr se1)
        (_sast_string_of_compound_op op)
        (string_of_sexpr se2)
  | SSequence (se1, se2) ->
      Printf.sprintf "SSequence(%s, %s)" (string_of_sexpr se1) (string_of_sexpr se2)
  | SFunctionCall (name, args) ->
      Printf.sprintf "SFunctionCall(%s, [%s])"
        name
        (String.concat "; " (List.map string_of_sexpr args))
  | SMethodCall (recv, name, args) ->
      Printf.sprintf "SMethodCall(%s, %s, [%s])"
        (string_of_sexpr recv)
        name
        (String.concat "; " (List.map string_of_sexpr args))
  | SCast (ty, se) ->
      Printf.sprintf "SCast(%s, %s)" (string_of_ty ty) (string_of_sexpr se)

and string_of_sexpr ((t, s) : sexpr) : string =
  Printf.sprintf "(%s : %s)" (string_of_sexpr_node s) (string_of_ty t)

(* function for a list of sexpr too, for convenience *)
let string_of_sexpr_list (sexprs : sexpr list) : string =
  "[" ^ (String.concat "; " (List.map string_of_sexpr sexprs)) ^ "]"
