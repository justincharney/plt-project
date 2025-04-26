(* Semantic checking for the P.A.T. compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
 * throws an exception if something is wrong. *)

let check sprogram = 

  let rec cexpr = function
    | SubExpr expr -> let (typ, expr') = cexpr expr in (typ, SSubExpr expr')
    | IntLit int -> (Int, SIntLit int)
    | BoolLit bool -> (Bool, SBoolLit bool)
    | FloatLit float -> (Float, SFloatLit float)
    | StringList str -> (String, SStringLit str)
    | ArrayLit (expr, typ, exprs) -> 
      let (typ', expr') = cexpr expr in
      let exprs' = List.map cexpr exprs in
      if typ' = typ then (Array (typ, List.length exprs), SArrayLit (expr', typ, exprs'))
      else raise (Failure ("type mismatch in array literal: " ^ string_of_type typ' ^ " vs " ^ string_of_type typ))
    
    | StructLit (expr, fields) ->
      let (typ, expr') = cexpr expr in
      let fields' = List.map (fun (f1, f2) -> (cexpr f1, cexpr f2)) fields in
      (typ, SStructLit (expr', fields'))
    
    | SliceLit (typ, exprs) ->
      let exprs' = List.map cexpr exprs in
      (Slice typ, SSliceLit (typ, exprs'))
    
  in
(* function for checking boolean expressions *)
  let cboolexpr expr = 
    let (typ, expr') = cexpr expr in
      match typ with
      | Bool -> (typ, expr')
      | _ -> raise (Failure ("expected boolean expression in " ^ string_of_type expr))
    in 

  (* function for checking statement semantics, goes from statement list to single statement *)
  let rec cstmt_list = function
      | [] -> []
      | stmt :: stmtl -> cstmt stmt :: cstmt_list stmtl

    and cstmt = function 
      | Expr expr -> SExpr (cexpr expr) (* goes to check expr function *)
      | VarDecl vardec -> SVarDecl (cvardec vardec) (* goes to check variable declaration function *)
      | IfStmt (expr, stmts1, stmts2) -> SIfStmt (cboolexpr expr, cstmt_list stmts1, cstmt_list stmts2) (* evaluate bool then check statements *)
      | ForStmt (stmt1, expr1, expr2, stmts) ->
        let stmt1' = match stmt1 with (* int x = y *)
          | Some stmt -> cstmt stmt
          | None -> None in
        let expr1' = match expr1 with (* x < z *)
          | Some expr -> cboolexpr expr
          | None -> None in
        let expr2' = match expr2 with (* x++ *)
          | Some expr -> cexpr expr
          | None -> None 
        in SForStmt (stmt1', expr1', expr2', cstmt_list stmts) (* check all statements in for loop *)
      | WhileStmt (expr, stmts) -> SWhileStmt (cboolexpr expr, cstmt_list stmts)
      | Return exprs -> None (* will finish later *)
  in sprogram