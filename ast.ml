type operator = PLUS | MINUS | DIV | TIMES | MOD | LSHIFT | RSHIFT | BITXOR | BITOR | BITNOT

  type expr =
  | Binop of expr * operator * expr
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | StringLit of string
  | Id of string
  | Assign of string * expr

  type primitive_type =
  | TBool
  | TUint8 | TUint16 | TUint32 | TUint64
  | TInt8 | TInt16 | TInt32 | TInt64
  | TFloat32 | TFloat64
  | TString