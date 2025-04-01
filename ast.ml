(* AST FOR P.A.T. *)

(* all binary operators in lang *)
type biop = 
  | Plus | Minus | Div | Times | Mod 
  | Lshift | Rshift | Bitxor | Bitor | Bitand 
  | Eq | Neq | Lt | Le | Gt | Ge | And | Or

(* all unary operators in lang *)
type unop = 
  | Bitnot | Not | Inc | Dec | Ampersand

(* all assignment operators in lang *)
type asgn = 
| Assign | Decl_assign | Plus_assign | Minus_Assign 
| Div_assign | Mod_assign | Lshift_assing | Rshift_assign
| Bitand_assign | Bitxor_assign | Bitor_assign

(* some basic expression in lang *)
type expr =
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | StringLit of string
  | Identifier of string

  | Binop of expr * biop * expr
  | Unaop of unop * expr

type assignment = 
  | Assignment of expr * asgn * expr 

(* statement in lang, only decl or expr *)
type stmt =
  | Block of seq
  | Expr of expr
  | If of expr * stmt * stmt (* Handle empty else case *)
  | While of expr * stmt
  | For of expr * stmt

(* old code *)
type primitive_type =
  | TBool
  | TUint8 | TUint16 | TUint32 | TUint64
  | TInt8 | TInt16 | TInt32 | TInt64
  | TFloat32 | TFloat64
  | TString