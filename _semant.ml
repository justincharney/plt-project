open Ast
open Sast

exception Semantic_error of string

module StringMap = Map.Make(String)

(*  The value environment stores variables and function headers that are
    in scope at a given point.  For semantic analysis we only need the
    type of a variable or a function (its parameter and return types). *)

    type func_sig = {
      params: ty list;
      returns: ty list;
    }

    type value_entry =
      | VVar of ty
      | VFunc of func_sig

    (* A separate map for structure definitions to that field lookup is quick
       and we can verify that a struct referred to from the program exists *)
    type env = {
      types: ty StringMap.t;  (* Named type + aliases *)
        values: value_entry StringMap.t; (* Variables and functions *)
        structs: (sfield list) StringMap.t; (* Structs and their fields *)
    }

    let empty_env = {
      types = StringMap.empty;
      values = StringMap.empty;
      structs = StringMap.empty;
    }

    let add_type name t env =
      { env with types = StringMap.add name t env.types}

    let add_value name v env =
      { env with values = StringMap.add name v env.values}

    let add_struct name fields env =
      { env with structs = StringMap.add name fields env.structs}

    let find_type name env =
      try StringMap.find name env.types with Not_found -> raise (Semantic_error ("Type " ^ name ^ " not found"))

    let find_value name env =
      try StringMap.find name env.values with Not_found -> raise (Semantic_error ("Value " ^ name ^ " not found"))

    let find_struct name env =
      try StringMap.find name env.structs with Not_found -> raise (Semantic_error ("Struct " ^ name ^ " not found"))

    (* Helpers*)

    let rec ty_equal t1 t2 =
      match t1, t2 with
      | TyPrim p1, TyPrim p2 -> p1 = p2
      | TyArray (et1, n1), TyArray (et2, n2) -> n1 = n2 && ty_equal et1 et2
      | TySlice et1, TySlice et2 -> ty_equal et1, et2
      | TyStruct s1, TyStruct s2 -> s1 = s2
      | TyError, _ | _, TyError -> true (* allow error propagation*)
      | _ -> false

    let rec resolve_type_expr env (t: type_expr) : ty =
      match t with
      | Primitive p -> TyPrim p
      | Array (te, n) -> TyArray (resolve_type_expr env te, n)
      | Slice te -> TySlice (resolve_type_expr env et)
      | Struct s -> (ignore (find_struct s env); TyStruct s)
      | TypeName n -> find_type n env

    let ensure_bool (t: ty) (loc: string) =
      if not (ty_equal t (TyPrim Bool)) then
      raise (Semantic_error (loc ^ ": Expected boolean expression"))

    (* Expression checking *)

    let rec check_binop (e1_t : ty) (op : biop) (e2_t : ty) : ty =
      match op with
      | Plus | Minus | Mult | Div | Mod ->
        if ty_equal e1_t e2_t && (match e1_t with TyPrim _ -> true | _ -> false)
        then e1_t
        else TyError
      | Eq | Neq | Lt | Le | Gt | Ge ->
        if ty_equal e1_t e2_t && (match e1_t with TyPrim _ -> true | _ -> false)
        then TyPrim Bool
        else TyError
      | And | Or ->
        (* Both need to be boolean *)
        if ty_equal et_1 (TyPrim Bool) && ty_equal et_2 (TyPrim Bool)
        then TyPrim Bool
        else TyError
      | Lshift | Rshift | Bitxor | Bitor | Bitand ->
        if ty_equal e1_t e2_t then e1_t else TyError

    let rec check_expr env (e : expr) : sexpr =
      match e with
      | IntLit i -> (TyPrim U32, SIntLit i) (* u32 is the default *)
      | BoolLit b -> (TyPrim bool, SBoolLit b)
      | CharLit c -> (TyPrim U8, SCharLit c)
      | FloatLit f -> (TyPrim F32, SFloatLit f)
      | StringLit s -> (TySlice (TyPrim U8), SStringLit s)
      | Null -> (TyError, SNull)

      | ArrayLit (te, elems) ->
        let elt_ty = resolve_type_expr env te in
        let selems = List.map (check_expr env) elems in
        List.iter (fun (t,_) -> if not (ty_equal t elt_ty) then raise (Semantic_error "array literal element type mismatch")) selems;
        (TyArray(elt_ty, List.length elems), SArrayLit(elt_ty, selems))

      | StructLit (name, field_inits) ->
        let sfields = find_struct name env in
        let field_ty_map = List.fold_left(fun m f -> StringMap.add f.name f.field_type m) StringMap.empty sfields in
        let check_field (fname, fexpr) =
          if not (StringMap.mem fname field_ty_map) then raise (Semantic_error "unknown field");
          let expected_ty = StringMap.find fname field_ty_map in
          let actual_ty = check_expr env fexpr in
          if not (ty_equal expected_ty actual_ty) then
            raise (Semantic_error (Printf.sprintf "Field %s type mismatch" fname));
          (fname, actual_ty)
        in
        let sfield_inits = List.map check_field field_inits in
        (TyStruct name, SStructLit (name, sfield_inits))

      | SliceLit

      | Identifier id ->
        begin match find_value id env with
          | VVar t -> (t, SIdentifier id)
          | VFunc _ -> raise (Semantic_error (id ^ " is a function, not a variable"))
          end

      | Binop (e1, op, e2) ->
        let se1 = check_expr env e1 in
        let se2 = check_expr env e2 in
        let t = check_binop (fst se1) op (fst se2) in
        if t = TyError then
          raise (Semantic_error ("Type mismatch in binary operation"))
        else (t, SBinop (se1, op, se2))

      | Unop (u, e1) ->
        let se1 = check_expr env e1 in
        (match u with
        | Neg | Bitnot -> if (match fst se1 with TyPrim _ -> true | _ -> false)
          then (fst se1, SUnop (u, se1))
          else raise (Semantic_error ("Unop expects numbric type"))
        | Not -> ensure_bool (fst, se1) "Unary not"; (TyPrim Bool, SUnaop (u, se1))
        | Inc | Dec -> (fst se1, SUnaop (u, se1))

      | SimpleAssign (lhs, rhs) ->
        (* SHOULD WE BE DOING A SPECIFIC CHECK FOR THE LHS HERE? *)
        let slhs = check_expr env lhs in
        let srhs = check_expr env rhs in
        if not (ty_equal (fst slhs) (fst srhs)) then
          raise (Semantic_error "Assignment type mismatch")
        else (fst, slhs, SimpleAssign(slhs, srhs))

      | CompoundAssign (lhs, op, rhs) ->
        let (t1, slhs) = check_expr env lhs in
        let (t2, srhs) = check_expr env rhs in
        if not (ty_equal t1 t2) then raise (Semantic_error "Compound assignment type mismatch");
        ensure_numeric t1;
        (t1, SCompoundAssign (slhs, op, srhs))

      | Sequence (e1, e2) ->
        let se1 = check_expr env e1 in
        let se2 = check_expr env e2 in
        (fst se2, SSequence (se1, se2))

      | _ -> raise (Semantic_error "Expression form not yet supported in semantic checker")

    (* Statement checking *)

    let rec check_stmt env (s: stmt) : env * sstmt =
      match s with
      | Expr e ->
        let se = check_expr env e in
        (env, SExpr se)

      | VarDecl {is_const; name; var_type; init} ->
        if StringMap.mem name env then raise (Semantic_error ("Variable already declared"));
        let inferred_ty, sinit =
        match var_type, init with
        | Some te, Some ie ->
        let declared_ty = resolve_type_expr env te in
        let sie = check_expr env ie in
        if not (ty_equal declared_ty (fst sie)) then
          raise (Semantic_error "Type mismatch in variable declaration");
          (declared_ty, Some sie)
        | Some te, None -> (resolve_type_expr env te, None)
        | None, Some ie -> let sie = check_expr env ie in (fst sie, Some sie)
        | None, None -> raise (Semantic_error "Cannot infer type that hasn't been initialized")

      | Block stmts ->
        let _, sstmts, _ = List.fold_left (fun (env_acc, sstmts_acc, _) st ->
          let env', sst = check_stmt env_acc st in
          (env', sstmts_acc @ [sst], ())) (env, [], ()) stmts in
        (env, SBlock sstmts)

      | IfStmt (cond, then_blk, else_opt) ->
        let scond = check_expr env cond in
        ensure_bool (fst scond) "if condition";
        let _, sthen = check_stmt env then_blk in
        let selse_opt =
          match else_opt with
          | None -> None
          | Some s -> let _, se = check_stmt env s in Some se
        in
        (env, SIf (scond, sthen, selse_opt))

      | WhileStmt (cond, body) ->
        let scond = check_expr env cond in
        ensure_bool (fst scond) "while condition";
        let _, sbody = check_stmt env body in
        (env, SWhile (scond, sbody))

      | ForStmt (init, cond, update, body) ->
        let env_after_init, sinit_opt =
        match init with
        | None -> (env, None)
        | Some st -> let env', sst = check_stmt env st in (env', Some sst)
        in
        let scond_opt = Option.map (check_expr env_after_init) cond in
        Option.iter (fun (t, _) -> ensure_bool t "for condition") scond_opt;
        let supd_opt = Option.map (check_expr env_after_init) update in
        let _, sbody = check_block env_after_init body in
        (env, SFor (sinit_opt, Option.map fst scond_opt, Option.map fst supd_opt, sbody))

      | Return exprs_opt ->
        let sexprs_opt = Option.map (List.map (check_expr env)) exprs_opt in
        (env, SReturn sexprs_opt)

      | Break ->
        (env, SBreak)

      | Continue ->
        (env, SContinue)
