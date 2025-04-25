(* Semantic checking for the P.A.T. compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
 * throws an exception if something is wrong.
 * Check each global variable, then check each function *)

let rec check sprogram = 

  let rec cstmt_list = function
      | [] -> []
      | stmt :: stmtl -> cstmt stmt :: cstmt_list stmtl
      
    and cstmt = function 
      | Expr expr -> SExpr (cexpr expr)
      | SVarDecl -> []
      | IfStmt (expr, stmt1, stmt2) -> SIfStmt (cboolexpr expr, cstmt stmt1, cstmt stmt2)
      | WhileStmt (expr, stmt) -> SWhileStmt (cboolexpr expr, cstmt stmt)
      | SForStmt -> []
      | Return e -> 
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
      in sprogram