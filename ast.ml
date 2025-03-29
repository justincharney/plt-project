(* AST FOR P.A.T. *)

(* all binary operators in lang *)
type biop = 
  | Plus | Minus | Div | Times | Mod 
  | Lshift | Rshift | Bitxor | Bitor | Bitand 
  | Eq | Neq | Lt | Le | Gt | Ge | And | Or

(* all unary operators in lang *)
type unop = 
  | BITNOT | NOT | INC | DEC

(* some basic expression in lang *)
type expr =
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | StringLit of string
  | Assign of string * expr
  | Binop of expr * biop * expr
  | Unaop of unop * expr
  | Sequence of expr * expr
  | Identifier of string

(* statement in lang *)
type stmt =
  | If of expr * stmt * stmt
  | While of expr * stmt

(* old code *)
type primitive_type =
  | TBool
  | TUint8 | TUint16 | TUint32 | TUint64
  | TInt8 | TInt16 | TInt32 | TInt64
  | TFloat32 | TFloat64
  | TString