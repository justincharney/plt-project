(* plt-project/semant.ml *)

open Ast
open Sast

exception Semantic_error of string

module StringMap = Map.Make(String)

(* ------------------------------------------------------------------ *)
(* Environment Definitions                                            *)
(* ------------------------------------------------------------------ *)

(* Represents resolved types during checking *)
type type_entry = Sast.ty

(* Represents entries in the value namespace (vars, funcs) *)
type value_entry =
  | VVar of { v_ty: Sast.ty; v_const: bool } (* Store resolved SAST type *)
  | VFunc of Ast.func_decl (* Keep original for full signature access *)

(* The checking environment *)
type env = {
  type_map         : type_entry StringMap.t; (* Maps type name -> Sast.ty *)
  value_map        : value_entry StringMap.t; (* Maps identifier -> value_entry *)
  struct_fields    : (string * string, Sast.ty) Hashtbl.t; (* (struct<name>, field<name>) -> Sast.ty *)
  current_func_retty: Sast.ty list option; (* Expected return type(s) in current func *)
  is_in_loop       : bool; (* Track if currently inside a loop for break/continue *)
  (* Add field for current struct name if needed for method checks *)
}

(* Initial empty environment *)
let empty_env = {
  type_map = StringMap.empty;
  value_map = StringMap.empty;
  struct_fields = Hashtbl.create 16;
  current_func_retty = None;
  is_in_loop = false;
}

(* Environment modification helpers *)
let add_type name ty env = { env with type_map = StringMap.add name ty env.type_map }
let add_value name entry env = { env with value_map = StringMap.add name entry env.value_map }
let add_struct_field sname fname fty env =
  Hashtbl.replace env.struct_fields (sname, fname) fty; env
let set_current_func_retty types env = { env with current_func_retty = Some types }
let set_in_loop status env = { env with is_in_loop = status }

(* Environment lookup helpers *)
let lookup_type name env : type_entry =
  try StringMap.find name env.type_map
  with Not_found -> raise (Semantic_error ("Unknown type name: " ^ name))

let lookup_value name env : value_entry =
  try StringMap.find name env.value_map
  with Not_found -> raise (Semantic_error ("Unbound identifier: " ^ name))

let lookup_struct_field sname fname env : Sast.ty =
  try Hashtbl.find env.struct_fields (sname, fname)
  with Not_found -> raise (Semantic_error (Printf.sprintf "Struct '%s' has no field '%s'" sname fname))

(* ------------------------------------------------------------------ *)
(* Type Translation and Helpers                                       *)
(* ------------------------------------------------------------------ *)

(* Translate AST type_expr to SAST ty, resolving names *)
let rec ast_type_to_sast_ty (env: env) (t: Ast.type_expr) : Sast.ty =
  match t with
  | Primitive p -> TyPrim p
  | Array (et, n) -> TyArray (ast_type_to_sast_ty env et, n)
  | Slice et -> TySlice (ast_type_to_sast_ty env et)
  | Struct name ->
      begin match lookup_type name env with
      | TyStruct sname when sname = name -> TyStruct name
      | _ -> raise (Semantic_error (Printf.sprintf "Identifier '%s' is not a struct type" name))
      end
  | TypeName name ->
      lookup_type name env (* Type aliases are resolved here *)

(* Structural equality on SAST types *)
let rec equal_sast_ty (a: Sast.ty) (b: Sast.ty) : bool =
  match a, b with
  | TyPrim p1, TyPrim p2 -> p1 = p2
  | TyArray (t1, n1), TyArray (t2, n2) -> n1 = n2 && equal_sast_ty t1 t2
  | TySlice t1, TySlice t2 -> equal_sast_ty t1 t2
  | TyStruct s1, TyStruct s2 -> s1 = s2
  | TyVoid, TyVoid -> true
  | TyError, TyError -> true (* Allow error propagation *)
  | _ -> false

(* Type checking helpers *)
let is_numeric = function TyPrim (U8|U16|U32|U64|I8|I16|I32|I64|F16|F32) -> true | _ -> false
let is_integer = function TyPrim (U8|U16|U32|U64|I8|I16|I32|I64) -> true | _ -> false
let is_boolean = function TyPrim Bool -> true | _ -> false
let is_comparable ty = is_numeric ty || is_boolean ty || (match ty with TyPrim String | TyStruct _ -> true | _ -> false)

(* Check if an expression is a valid l-value (can be assigned to) *)
(* Returns the type and const status if it is *)
let check_lvalue (env: env) (e: Ast.expr) : Sast.sexpr * bool =
    match e with
    | Identifier id ->
        begin match lookup_value id env with
        | VVar { v_ty; v_const } -> ((v_ty, SIdentifier id), v_const)
        | _ -> raise (Semantic_error (Printf.sprintf "'%s' is not a variable" id))
        end
    | FieldAccess (receiver_expr, field_name) ->
        let (receiver_ty, sreceiver) as se_receiver = check_expr env receiver_expr in
         (* Fields themselves aren't const, but the receiver might be part of a const var *)
         (* TODO: Track const-ness through field access if receiver is const variable *)
        begin match receiver_ty with
        | TyStruct struct_name ->
            let field_ty = lookup_struct_field struct_name field_name env in
            ((field_ty, SFieldAccess (se_receiver, field_name)), false) (* Assuming fields are mutable for now *)
        | _ -> raise (Semantic_error ("Field access requires a struct"))
        end
    | IndexAccess (arr_expr, idx_expr) ->
        let (arr_ty, sarr) as se_arr = check_expr env arr_expr in
        let (idx_ty, sidx) as se_idx = check_expr env idx_expr in
         if not (is_integer idx_ty) then raise (Semantic_error "Index must be integer");
         (* TODO: Track const-ness through index access *)
        let elem_ty = match arr_ty with
            | TyArray (et, _) | TySlice et -> et
            | _ -> raise (Semantic_error "Indexing requires array or slice")
        in
        ((elem_ty, SIndexAccess(se_arr, se_idx)), false) (* Assuming elements mutable *)
    | _ -> raise (Semantic_error "Expression is not assignable (not an l-value)")

and check_expr (env: env) (e: expr) : sexpr = (* sexpr = ty * sx *)
  match e with
  (* --- Literals --- *)
  | IntLit i    -> (TyPrim I32, SIntLit i)
  | BoolLit b   -> (TyPrim Bool, SBoolLit b)
  | CharLit c   -> (TyPrim I8, SCharLit c)
  | FloatLit f  -> (TyPrim F32, SFloatLit f)
  | StringLit s -> (TyPrim String, SStringLit s)
  | Null        -> raise (Semantic_error "Null literal type not yet supported")

  (* --- Identifiers & Basic Access --- *)
  | Identifier id ->
      begin match lookup_value id env with
      | VVar { v_ty; v_const=_ } -> (v_ty, SIdentifier id)
      | VFunc _ -> raise (Semantic_error (Printf.sprintf "'%s' is a function, not a value" id))
      end

  | FieldAccess (receiver_expr, field_name) ->
      let (receiver_ty, sreceiver) as se_receiver = check_expr env receiver_expr in
      begin match receiver_ty with
      | TyStruct struct_name ->
          let field_ty = lookup_struct_field struct_name field_name env in
          (field_ty, SFieldAccess (se_receiver, field_name))
      | _ -> raise (Semantic_error (Printf.sprintf "Field access requires a struct type, got %s"
               (Sast.string_of_ty receiver_ty)))
      end

  | IndexAccess (arr_expr, idx_expr) ->
      let (arr_ty, sarr) as se_arr = check_expr env arr_expr in
      let (idx_ty, sidx) as se_idx = check_expr env idx_expr in
      if not (is_integer idx_ty) then
          raise (Semantic_error (Printf.sprintf "Index expression requires an integer type, got %s" (Sast.string_of_ty idx_ty)));
      let elem_ty = match arr_ty with
          | TyArray (et, _) -> et
          | TySlice et -> et
          | _ -> raise (Semantic_error (Printf.sprintf "Index access requires an array or slice type, got %s" (Sast.string_of_ty arr_ty)))
      in
      (elem_ty, SIndexAccess(se_arr, se_idx))

  | SliceExpr(arr_expr, start_opt, end_opt) ->
     let (arr_ty, sarr) as se_arr = check_expr env arr_expr in
     let elem_ty = match arr_ty with
         | TyArray (et, _) | TySlice et -> et
         | _ -> raise (Semantic_error (Printf.sprintf "Slicing requires an array or slice type, got %s" (Sast.string_of_ty arr_ty)))
     in
     let check_bound bound_name opt_expr = match opt_expr with
         | None -> None
         | Some exp ->
             let (ty, sx) as se = check_expr env exp in
             if not (is_integer ty) then
                 raise (Semantic_error (Printf.sprintf "Slice %s bound requires an integer type, got %s" bound_name (Sast.string_of_ty ty)));
             Some se
     in
     let se_start = check_bound "start" start_opt in
     let se_end = check_bound "end" end_opt in
     (TySlice elem_ty, SSliceExpr(se_arr, se_start, se_end))

  (* --- Operators --- *)
  | Binop (e1, op, e2) ->
      let (t1, sxe1) as se1 = check_expr env e1 in
      let (t2, sxe2) as se2 = check_expr env e2 in
      let final_type = match op with
        | Plus | Minus | Mult | Div | Mod ->
            if not (is_numeric t1 && is_numeric t2) then
              raise (Semantic_error (Printf.sprintf "Arithmetic operator '%s' requires numeric operands, got %s and %s"
                (Ast.string_of_biop op) (Sast.string_of_ty t1) (Sast.string_of_ty t2)));
            if not (equal_sast_ty t1 t2) then
              raise (Semantic_error (Printf.sprintf "Type mismatch in arithmetic op '%s': %s vs %s"
                (Ast.string_of_biop op) (Sast.string_of_ty t1) (Sast.string_of_ty t2)));
            t1 (* Result type is same as operands *)
        | Lshift | Rshift | Bitand | Bitxor | Bitor ->
             if not (is_integer t1 && is_integer t2) then
               raise (Semantic_error (Printf.sprintf "Bitwise operator '%s' requires integer operands, got %s and %s"
                 (Ast.string_of_biop op) (Sast.string_of_ty t1) (Sast.string_of_ty t2)));
             if not (equal_sast_ty t1 t2) then
                 raise (Semantic_error (Printf.sprintf "Type mismatch in bitwise op '%s': %s vs %s"
                   (Ast.string_of_biop op) (Sast.string_of_ty t1) (Sast.string_of_ty t2)));
             t1
        | Eq | Neq | Lt | Le | Gt | Ge ->
            if not (is_comparable t1 && is_comparable t2) then
                 raise (Semantic_error (Printf.sprintf "Comparison operator '%s' cannot compare types %s and %s"
                   (Ast.string_of_biop op) (Sast.string_of_ty t1) (Sast.string_of_ty t2)));
            if not (equal_sast_ty t1 t2) then
              raise (Semantic_error (Printf.sprintf "Type mismatch in comparison op '%s': %s vs %s"
                (Ast.string_of_biop op) (Sast.string_of_ty t1) (Sast.string_of_ty t2)));
            TyPrim Bool
        | And | Or ->
            if not (is_boolean t1 && is_boolean t2) then
               raise (Semantic_error (Printf.sprintf "Logical operator '%s' requires boolean operands, got %s and %s"
                 (Ast.string_of_biop op) (Sast.string_of_ty t1) (Sast.string_of_ty t2)));
            TyPrim Bool
      in
      (final_type, SBinop (se1, op, se2))

  | Unaop (op, e1) ->
      let (t1, sxe1) as se1 = check_expr env e1 in
      let final_type = match op with
        | Neg ->
            if not (is_numeric t1) then
                 raise (Semantic_error (Printf.sprintf "Unary '-' requires a numeric operand, got %s" (Sast.string_of_ty t1)));
            t1
        | Not ->
            if not (is_boolean t1) then
              raise (Semantic_error (Printf.sprintf "Logical '!' requires boolean operand, got %s" (Sast.string_of_ty t1)));
            TyPrim Bool
        | Bitnot ->
            if not (is_integer t1) then
                 raise (Semantic_error (Printf.sprintf "Bitwise '~' requires an integer operand, got %s" (Sast.string_of_ty t1)));
            t1
        | Inc | Dec ->
             if not (is_numeric t1) then
                 raise (Semantic_error (Printf.sprintf "'%s' requires a numeric operand, got %s" (Ast.string_of_unop op) (Sast.string_of_ty t1)));
            let _, is_const = check_lvalue env e1 in (* Check if it's an l-value *)
            if is_const then raise (Semantic_error (Printf.sprintf "Cannot apply '%s' to a constant" (Ast.string_of_unop op)));
            t1 (* Type remains the same *)
      in
      (final_type, SUnaop(op, se1))

  | SimpleAssign (lhs_expr, rhs_expr) ->
      let (selhs, is_const) = check_lvalue env lhs_expr in
      let (tl, _) = selhs in
      if is_const then raise (Semantic_error "Cannot assign to a constant");
      let (tr, srhs) as serhs = check_expr env rhs_expr in
      if not (equal_sast_ty tl tr) then
        raise (Semantic_error (Printf.sprintf "Assignment type mismatch: cannot assign %s to %s"
          (Sast.string_of_ty tr) (Sast.string_of_ty tl)));
      (tl, SSimpleAssign (selhs, serhs)) (* Assignment expr type is LHS type *)

  | CompoundAssign (lhs_expr, comp_op, rhs_expr) ->
      let (selhs, is_const) = check_lvalue env lhs_expr in
      let (tl, _) = selhs in
       if is_const then raise (Semantic_error (Printf.sprintf "Cannot apply compound assignment '%s' to a constant" (Ast.string_of_compound_op comp_op)));
       let (tr, srhs) as serhs = check_expr env rhs_expr in
       (* Check type compatibility based on the specific compound op *)
       let is_compatible = match comp_op with
         | PlusAssign | MinusAssign | TimesAssign | DivAssign | ModAssign -> is_numeric tl && equal_sast_ty tl tr
         | LshiftAssign | RshiftAssign | BitandAssign | BitxorAssign | BitorAssign -> is_integer tl && equal_sast_ty tl tr
       in
       if not is_compatible then
           raise (Semantic_error (Printf.sprintf "Compound assignment '%s' type mismatch or invalid operand types: %s vs %s"
              (Ast.string_of_compound_op comp_op) (Sast.string_of_ty tl) (Sast.string_of_ty tr)));
       (tl, SCompoundAssign(selhs, comp_op, serhs))

  (* --- Calls & Builtins --- *)
  | FunctionCall (fname, args) ->
      begin match lookup_value fname env with
      | VFunc fdecl ->
          if List.length fdecl.params <> List.length args then
            raise (Semantic_error (Printf.sprintf "Function '%s' expects %d arguments, got %d"
              fname (List.length fdecl.params) (List.length args)));

          let sargs = List.map (check_expr env) args in
          (* Check argument types against parameter types *)
          List.iter2 (fun param sarg ->
            let expected_ty = ast_type_to_sast_ty env param.param_type in
            let (actual_ty, _) = sarg in
            if not (equal_sast_ty expected_ty actual_ty) then
              raise (Semantic_error (Printf.sprintf "Type mismatch for argument '%s' in call to '%s': expected %s, got %s"
                param.name fname (Sast.string_of_ty expected_ty) (Sast.string_of_ty actual_ty)))
          ) fdecl.params sargs;

          (* Determine SAST return type list *)
          let retty_list = List.map (ast_type_to_sast_ty env) fdecl.return_types in
          (* Simplify: Assume 0 or 1 return value for now *)
          let final_retty = match retty_list with
             | [] -> TyVoid
             | [t] -> t
             | _ -> raise (Semantic_error (Printf.sprintf "Multiple return values from function '%s' not yet supported" fname))
          in
          (final_retty, SFunctionCall(fname, sargs))

      | _ -> raise (Semantic_error (Printf.sprintf "'%s' is not a function" fname))
      end

  | Cast (target_ast_ty, expr_to_cast) ->
     let target_ty = ast_type_to_sast_ty env target_ast_ty in
     let (source_ty, _) as se_cast = check_expr env expr_to_cast in
     (* TODO: Implement actual casting rules between types *)
     if equal_sast_ty source_ty target_ty then
        (* Warn about redundant cast? *)
        se_cast
     else
        (* Apply casting logic (basic placeholder: allow numeric <-> numeric) *)
        if is_numeric source_ty && is_numeric target_ty then
            (target_ty, SCast(target_ty, se_cast))
        else
            raise (Semantic_error (Printf.sprintf "Invalid cast from %s to %s"
                (Sast.string_of_ty source_ty) (Sast.string_of_ty target_ty)))

 | Make (ast_ty, len_expr, cap_opt_expr) ->
     let ty = ast_type_to_sast_ty env ast_ty in
     let (len_ty, _) as se_len = check_expr env len_expr in
     if not (is_integer len_ty) then
          raise (Semantic_error (Printf.sprintf "'make' length requires an integer type, got %s" (Sast.string_of_ty len_ty)));

     let se_cap_opt = match cap_opt_expr with
        | None -> None
        | Some cap_expr ->
            let (cap_ty, _) as se_cap = check_expr env cap_expr in
            if not (is_integer cap_ty) then
                 raise (Semantic_error (Printf.sprintf "'make' capacity requires an integer type, got %s" (Sast.string_of_ty cap_ty)));
            (* TODO: Check len <= cap *)
            Some se_cap
     in
     begin match ty with
     | TySlice elem_ty -> (TySlice elem_ty, SMake(elem_ty, se_len, se_cap_opt))
     (*| TyMap (k,v) -> ... *)
     | _ -> raise (Semantic_error (Printf.sprintf "'make' can only create slices (or maps/channels), not %s" (Sast.string_of_ty ty)))
     end

 | Sequence (e1, e2) ->
     let _ = check_expr env e1 in (* Check e1 for effects, discard result type *)
     check_expr env e2 (* Type of sequence is type of second expr *)

  (* --- Other Literals --- *)
  | ArrayLit (ast_elem_ty, exprs) ->
      let elem_ty = ast_type_to_sast_ty env ast_elem_ty in
      let sexprs = List.map (check_expr env) exprs in
      List.iter (fun (et, _) ->
          if not (equal_sast_ty et elem_ty) then
              raise (Semantic_error (Printf.sprintf "Array literal element type mismatch: expected %s, got %s"
                  (Sast.string_of_ty elem_ty) (Sast.string_of_ty et)))
      ) sexprs;
      let array_ty = TyArray (elem_ty, List.length sexprs) in
      (array_ty, SArrayLit (elem_ty, sexprs))

 | StructLit (struct_name, fields) ->
     let struct_ty = lookup_type struct_name env in
     let actual_struct_name = match struct_ty with
        | TyStruct name -> name
        | _ -> raise (Semantic_error (Printf.sprintf "'%s' is not a struct type" struct_name))
     in
     (* TODO: Check all required fields are present and types match *)
     let sfields = List.map (fun (fname, e) ->
         let field_expected_ty = lookup_struct_field actual_struct_name fname env in
         let (field_actual_ty, _) as se_field = check_expr env e in
         if not (equal_sast_ty field_expected_ty field_actual_ty) then
             raise (Semantic_error (Printf.sprintf "Type mismatch for field '%s' in struct '%s' literal: expected %s, got %s"
                fname actual_struct_name (Sast.string_of_ty field_expected_ty) (Sast.string_of_ty field_actual_ty)));
         (fname, se_field)
       ) fields
     in
     (TyStruct actual_struct_name, SStructLit(actual_struct_name, sfields))

  | SliceLit (ast_elem_ty, exprs) ->
      let elem_ty = ast_type_to_sast_ty env ast_elem_ty in
      let sexprs = List.map (check_expr env) exprs in
      List.iter (fun (et, _) ->
          if not (equal_sast_ty et elem_ty) then
              raise (Semantic_error (Printf.sprintf "Slice literal element type mismatch: expected %s, got %s"
                  (Sast.string_of_ty elem_ty) (Sast.string_of_ty et)))
      ) sexprs;
      (TySlice elem_ty, SSliceLit (elem_ty, sexprs))

  | MethodCall _ -> raise (Semantic_error "Method call checking not implemented yet")

(* ------------------------------------------------------------------ *)
(* Statement Checking                                                 *)
(* ------------------------------------------------------------------ *)
and check_stmt (env: env) (s: stmt) : env * sstmt =
  match s with
  | Expr e ->
      let checked_sexpr = check_expr env e in
      (env, SExpr checked_sexpr)

  | VarDecl { is_const; name; var_type; initializer_expr } ->
      (* Check for redeclaration in the same scope (Basic check) *)
      if StringMap.mem name env.value_map then
          raise (Semantic_error (Printf.sprintf "Variable '%s' already declared in this scope" name));

      let (init_ty, _) as init_sexpr =
        match initializer_expr with
        | None -> raise (Semantic_error "Variable declarations require an initializer")
        | Some init_e -> check_expr env init_e
      in

      let declared_ty = match var_type with
        | None -> init_ty (* Infer type *)
        | Some ast_t ->
            let explicit_ty = ast_type_to_sast_ty env ast_t in
            if not (equal_sast_ty explicit_ty init_ty) then
              raise (Semantic_error (Printf.sprintf "Initializer type mismatch for '%s': declared %s, got %s"
                name (Sast.string_of_ty explicit_ty) (Sast.string_of_ty init_ty)));
            explicit_ty
      in

      let env' = add_value name (VVar { v_ty = declared_ty; v_const = is_const }) env in
      let s_var_decl = SVarDecl {
          v_ty = declared_ty;
          v_const = is_const;
          v_name = name;
          v_init = init_sexpr;
        } in
      (env', s_var_decl)

  | Block stmts ->
      (* TODO: Implement proper block scoping. This version doesn't isolate scope. *)
      let env_final, sstmts_rev = List.fold_left
          (fun (current_env, acc_sstmts) stmt ->
            let env', sstmt = check_stmt current_env stmt in
            (env', sstmt :: acc_sstmts)
          ) (env, []) stmts
      in
      (env, SBlock (List.rev sstmts_rev)) (* Return original env, changes were local *)

  | Return expr_list_opt ->
      let sexprs_opt, return_types = match expr_list_opt with
          | None -> None, [TyVoid] (* Treat "return;" as returning void *)
          | Some exprs ->
              let ses = List.map (check_expr env) exprs in
              (Some ses, List.map fst ses)
      in
      (* Check against current function's expected return type *)
      begin match env.current_func_retty with
      | None -> raise (Semantic_error "Return statement found outside function body")
      | Some expected_retty_list ->
          let expected_types = if expected_retty_list = [] then [TyVoid] else expected_retty_list in
          if List.length expected_types <> List.length return_types then
             raise (Semantic_error (Printf.sprintf "Return statement arity mismatch: function expects %d values, got %d"
                 (List.length expected_types) (List.length return_types)))
          else
             List.iter2 (fun expected actual ->
                if not (equal_sast_ty expected actual) then
                   raise (Semantic_error (Printf.sprintf "Return type mismatch: function expects %s, got %s"
                      (Sast.string_of_ty expected) (Sast.string_of_ty actual)))
             ) expected_types return_types
      end;
      (env, SReturn sexprs_opt)

  | IfStmt (cond_expr, then_block_stmts, else_stmt_opt) ->
      let (cond_ty, _) as se_cond = check_expr env cond_expr in
      if not (is_boolean cond_ty) then
          raise (Semantic_error (Printf.sprintf "If condition requires a boolean type, got %s" (Sast.string_of_ty cond_ty)));

      let _, sthen = check_stmt env (Block then_block_stmts) in
      let sthen_list = match sthen with SBlock l -> l | _ -> failwith "Internal error: If 'then' block check failed" in

      let selse_opt = match else_stmt_opt with
        | None -> None
        | Some else_stmt -> let _, selse = check_stmt env else_stmt in Some selse
      in
      (env, SIf (se_cond, sthen_list, selse_opt))

  | WhileStmt (cond_expr, body_stmts) ->
      let (cond_ty, _) as se_cond = check_expr env cond_expr in
       if not (is_boolean cond_ty) then
          raise (Semantic_error (Printf.sprintf "While condition requires a boolean type, got %s" (Sast.string_of_ty cond_ty)));
      (* Check body in loop context *)
      let env_in_loop = set_in_loop true env in
      let _, sbody = check_stmt env_in_loop (Block body_stmts) in
      let sbody_list = match sbody with SBlock l -> l | _ -> failwith "Internal error: While body check failed" in
      (env, SWhile(se_cond, sbody_list))

  | ForStmt (init_stmt_opt, cond_expr_opt, update_expr_opt, body_stmts) ->
      (* TODO: Proper scoping for init statement *)
      let env_after_init, sinit_opt = match init_stmt_opt with
          | None -> env, None
          | Some init_stmt ->
              let env', sinit = check_stmt env init_stmt in (env', Some sinit)
      in
      let scond_opt = match cond_expr_opt with
          | None -> None (* No condition means always true loop (use break) *)
          | Some cond_expr ->
              let (cond_ty, _) as se_cond = check_expr env_after_init cond_expr in
              if not (is_boolean cond_ty) then
                 raise (Semantic_error (Printf.sprintf "For condition requires a boolean type, got %s" (Sast.string_of_ty cond_ty)));
              Some se_cond
      in
      let supdate_opt = match update_expr_opt with
          | None -> None
          | Some update_expr -> Some (check_expr env_after_init update_expr)
      in
      (* Check body in loop context *)
      let env_in_loop = set_in_loop true env_after_init in
      let _, sbody = check_stmt env_in_loop (Block body_stmts) in
      let sbody_list = match sbody with SBlock l -> l | _ -> failwith "Internal error: For body check failed" in
      (env, SFor(sinit_opt, scond_opt, supdate_opt, sbody_list))

  | Break ->
      if not env.is_in_loop then raise (Semantic_error "'break' outside of loop");
      (env, SBreak)
  | Continue ->
      if not env.is_in_loop then raise (Semantic_error "'continue' outside of loop");
      (env, SContinue)

(* ------------------------------------------------------------------ *)
(* Top-Level Checking                                                 *)
(* ------------------------------------------------------------------ *)

(* Add built-in primitive types to the type map *)
let populate_primitive_types env =
  let primitives = [
      ("bool", TyPrim Bool); ("string", TyPrim String);
      ("u8", TyPrim U8); ("u16", TyPrim U16); ("u32", TyPrim U32); ("u64", TyPrim U64);
      ("i8", TyPrim I8); ("i16", TyPrim I16); ("i32", TyPrim I32); ("i64", TyPrim I64);
      ("f16", TyPrim F16); ("f32", TyPrim F32);
      ("error", TyPrim Error);
    ] in
  List.fold_left (fun acc (name, ty) -> add_type name ty acc) env primitives

(* First pass: Add type names (structs, aliases) to type map and struct field info *)
let populate_type_declarations env type_decls =
  (* Pass 1a: Add all type names first to handle forward references within types *)
  let env_with_names = List.fold_left (fun acc td ->
      match td with
      | TypeStruct (name, _) -> if StringMap.mem name acc.type_map then
                                     raise (Semantic_error ("Duplicate type name: " ^ name))
                                  else add_type name (TyStruct name) acc
      | TypeAlias (name, _) -> if StringMap.mem name acc.type_map then
                                     raise (Semantic_error ("Duplicate type name: " ^ name))
                                  else add_type name TyError acc (* Placeholder for alias *)
  ) env type_decls in

  (* Pass 1b: Resolve aliases and populate struct fields *)
  List.fold_left (fun acc td ->
      match td with
      | TypeStruct (sname, fields) ->
          (* Add fields to field lookup table *)
          List.fold_left (fun current_env field ->
              let field_sast_ty = ast_type_to_sast_ty current_env field.field_type in
              (* Check for duplicate field names within the struct *)
              if Hashtbl.mem current_env.struct_fields (sname, field.name) then
                  raise (Semantic_error (Printf.sprintf "Duplicate field '%s' in struct '%s'" field.name sname));
              add_struct_field sname field.name field_sast_ty current_env
          ) acc fields
      | TypeAlias (alias_name, target_type_expr) ->
          let resolved_target_ty = ast_type_to_sast_ty acc target_type_expr in
          (* Update the type map with the resolved alias type *)
          { acc with type_map = StringMap.add alias_name resolved_target_ty acc.type_map }
  ) env_with_names type_decls


(* Second pass: Add function signatures to the value map *)
let populate_func_signatures env funcs =
   List.fold_left (fun acc f ->
     if StringMap.mem f.name acc.value_map then
         raise (Semantic_error (Printf.sprintf "Duplicate function name: '%s'" f.name));
     add_value f.name (VFunc f) acc
   ) env funcs

(* Check a single function *)
let check_function env (f: Ast.func_decl) : Sast.sfunc =
  (* Translate parameter AST types to SAST types *)
  let sf_params = List.map (fun p ->
    { p_name = p.name; p_ty = ast_type_to_sast_ty env p.param_type }
  ) f.params in

  (* Translate return AST types to SAST types *)
  let sf_retty = List.map (ast_type_to_sast_ty env) f.return_types in

  (* Create environment for the function body *)
  let env_with_params = List.fold_left
      (fun acc sparam -> add_value sparam.p_name (VVar { v_ty = sparam.p_ty; v_const = true }) acc) (* Params are const *)
      env sf_params
  in
  (* Set expected return type for checking return statements *)
  let env_for_body = set_current_func_retty sf_retty env_with_params in

  (* Check the function body *)
  let _, checked_body_stmt = check_stmt env_for_body (Block f.body) in
  let sf_body = match checked_body_stmt with
    | SBlock b -> b
    | _ -> failwith "Internal error: Function body check did not return an SBlock"
  in
  { s_fname = f.name; s_params = sf_params; s_retty = sf_retty; s_body = sf_body }

(* Check a single struct method *)
let check_struct_method env (sf: Ast.struct_func) : Sast.sstruct_func =
   (* Check struct exists *)
   let receiver_sast_type = match lookup_type sf.struct_name env with
      | TyStruct name when name = sf.struct_name -> TyStruct name
      | _ -> raise (Semantic_error (Printf.sprintf "Cannot define method for non-struct type '%s'" sf.struct_name))
   in
   (* TODO: Get receiver name from parser/AST adjustment *)
   let receiver_name = "self" in (* Hardcoded placeholder *)
   let ss_receiver = { p_name = receiver_name; p_ty = receiver_sast_type } in

   (* Translate parameter/return types *)
   let ss_params = List.map (fun p -> { p_name = p.name; p_ty = ast_type_to_sast_ty env p.param_type }) sf.params in
   let ss_retty = List.map (ast_type_to_sast_ty env) sf.return_types in

   (* Create environment for the method body *)
   let env_with_receiver = add_value receiver_name (VVar { v_ty = receiver_sast_type; v_const = true }) env (* TODO: Receiver mutability *) in
   let env_with_params = List.fold_left
       (fun acc sparam -> add_value sparam.p_name (VVar { v_ty = sparam.p_ty; v_const = true }) acc)
       env_with_receiver ss_params
   in
   let env_for_body = set_current_func_retty ss_retty env_with_params in

   (* Check the method body *)
   let _, checked_body_stmt = check_stmt env_for_body (Block sf.body) in
   let ss_body = match checked_body_stmt with
     | SBlock b -> b
     | _ -> failwith "Internal error: Method body check did not return an SBlock"
   in
   { ss_name = sf.name; ss_struct_name = sf.struct_name; ss_receiver; ss_params; ss_retty; ss_body }


(* Main analysis function *)
let analyse (prog: Ast.program) : Sast.sprogram =
  (* Pass 1: Primitives and Type Declarations *)
  let env_pass1 = populate_primitive_types empty_env in
  let env_pass2 = populate_type_declarations env_pass1 prog.type_declarations in
  (* TODO: Check for type cycles here if supporting recursive types *)

  (* Pass 2: Function Signatures *)
  let env_pass3 = populate_func_signatures env_pass2 prog.functions in
  (* TODO: Add method signatures here, check for conflicts *)

  (* Pass 3: Check Globals *)
  let env_after_globals, sglobals_rev =
    List.fold_left (fun (current_env, acc_sglobals) g ->
        let vd_stmt = VarDecl {
            is_const = g.is_const;
            name = g.name;
            var_type = g.var_type;
            initializer_expr = g.initializer_expr;
          } in
        (* Check the global declaration *)
        let next_env, checked_stmt = check_stmt current_env vd_stmt in
        match checked_stmt with
        | SVarDecl vd ->
            let sglobal_rec = {
                sg_name = vd.v_name;
                sg_const = vd.v_const;
                sg_ty = vd.v_ty;
                sg_init = vd.v_init; (* Initializer mandatory in Sast.sglobal *)
              } in
            (next_env, sglobal_rec :: acc_sglobals)
        | _ -> failwith "Internal error: Global variable check did not produce SVarDecl"
      ) (env_pass3, []) prog.global_vars
  in

  (* Pass 4: Check function bodies *)
  let sfuncs = List.map (check_function env_after_globals) prog.functions in

  (* Pass 5: Check struct method bodies *)
  let smethods = List.map (check_struct_method env_after_globals) prog.struct_functions in
  (* TODO: Associate methods with struct types for lookup during method calls *)

  (* Construct final SAST program *)
  {
    sp_package = prog.package_name;
    sp_imports = prog.imports;
    sp_types   = prog.type_declarations; (* Keep original type decls *)
    sp_globals = List.rev sglobals_rev;
    sp_funcs   = sfuncs;
    sp_methods = smethods;
  }
