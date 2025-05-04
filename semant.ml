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
      match StringMap.find_opt name env.types with
      | Some v -> v
      | None -> raise (Semantic_error ("Type " ^ name ^ " not found"))

    let find_value name env =
      match StringMap.find_opt name env.values with
      | Some v -> v
      | None -> raise (Semantic_error ("Value " ^ name ^ " not found"))

    let find_struct name env =
      match StringMap.find_opt name env.structs with
      | Some v -> v
      | None -> raise (Semantic_error ("Struct " ^ name ^ " not found"))

    (* ----- Helpers ----- *)

    let numeric_prim = function
      | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | F16 | F32 -> true
      | _ -> false

    let int_prim = function
      | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 -> true
      | _ -> false

    let is_numeric = function TyPrim p when numeric_prim p -> true | _ -> false
    let is_integer = function TyPrim p when int_prim p -> true | _ -> false
    let is_boolean = function TyPrim Bool -> true | _ -> false
    let is_nullable = function TyNull | TySlice _ | TyStruct _ -> true | _ -> false
    let is_string = function TyPrim String -> true | _ -> false

    let ensure_numeric ty ctx =
      if not (is_numeric ty) then
        raise (Semantic_error (ctx ^ ": numeric type required"))

    let ensure_bool ty ctx =
      if not (is_boolean ty) then
        raise (Semantic_error (ctx ^ ": boolean type required"))

    let rec ty_equal t1 t2 =
      match t1, t2 with
      | TyNull, t | t, TyNull -> is_nullable t
      | TyPrim p1, TyPrim p2 -> p1 = p2
      | TyArray (et1, n1), TyArray (et2, n2) -> n1 = n2 && ty_equal et1 et2
      | TySlice et1, TySlice et2 -> ty_equal et1 et2
      | TyStruct s1, TyStruct s2 -> s1 = s2
      | TyTuple ts1, TyTuple ts2 -> List.length ts1 = List.length ts2 && List.for_all2 ty_equal ts1 ts2
      | TyError, _
      | _, TyError -> true (* allow error propagation*)
      | _ -> false

    let rec resolve_type_expr env (t: type_expr) : ty =
      match t with
      | Primitive p -> TyPrim p
      | Array (te, n) -> TyArray (resolve_type_expr env te, n)
      | Slice te -> TySlice (resolve_type_expr env te)
      | Struct s ->
        if not (StringMap.mem s env.structs) then
          raise (Semantic_error ("Struct " ^ s ^ " not found"));
        TyStruct s
      | TypeName n -> find_type n env

    let mangle (struct_name : string) (method_name : string) : string =
      struct_name ^ "$" ^ method_name

    let dup n what = raise (Semantic_error ("Duplicate " ^ what ^ " " ^ n))

    let is_lvalue = function
      | SIdentifier _ -> true
      | SFieldAccess (_,_) -> true
      | SIndexAccess (_, _) -> true
      | _ -> false

    (* Expression checking *)
    let check_binop t1 op t2 =
      match op with
      (* String ops *)
      | Plus when is_string t1 && is_string t2 -> TyPrim String (* Concatenation *)
      | Eq | Neq when is_string t1 && is_string t2 -> TyPrim Bool (* Equality *)
      (* Numeric Ops *)
      | Plus | Minus | Mult | Div | Mod
      | Lshift | Rshift | Bitand | Bitxor | Bitor ->
          if is_numeric t1 && ty_equal t1 t2 then t1 else TyError
      | Eq | Neq | Lt | Le | Gt | Ge ->
          if is_numeric t1 && ty_equal t1 t2 then TyPrim Bool else TyError
      (* Boolean ops *)
      | And | Or ->
          if is_boolean t1 && is_boolean t2 then TyPrim Bool else TyError


    let rec check_expr env (e : expr) : sexpr =
      match e with
      | IntLit i -> (TyPrim I32, SIntLit i) (* I32 is the default *)
      | BoolLit b -> (TyPrim Bool, SBoolLit b)
      | CharLit c -> (TyPrim U8, SCharLit c)
      | FloatLit f -> (TyPrim F32, SFloatLit f)
      | StringLit s -> (TyPrim String, SStringLit s)
      | Null -> (TyNull, SNull)

      | ArrayLit (arr_ty_expr, elems) ->
        (* 1. Resolve the provided type expression (e.g., [2]i32) to a Sast.ty *)
        let resolved_ty = resolve_type_expr env arr_ty_expr in

        (* 2. Validate it's an array type and get element type/size *)
        let (elt_ty, expected_len) =
          match resolved_ty with
          | TyArray (et, len) -> (et, len)
          | _ -> raise (Semantic_error "Array literal's type specification is not an array type")
        in

        (* 3. Check if the number of elements matches the declared size *)
        if List.length elems <> expected_len then
          raise (Semantic_error (Printf.sprintf "Array literal size mismatch: expected %d elements, got %d"
                                    expected_len (List.length elems)));

        (* 4. Type-check each element against the expected element type *)
        let selems = List.map (check_expr env) elems in
        List.iter (fun (actual_ty, _) ->
          if not (ty_equal actual_ty elt_ty) then
            raise (Semantic_error (Printf.sprintf "Array literal element type mismatch: expected %s, got %s"
                                      (string_of_ty elt_ty) (string_of_ty actual_ty)))
        ) selems;

        (* 5. Return the Sast node: the overall type is resolved_ty,
              and SArrayLit likely needs the element type and the checked elements *)
        (resolved_ty, SArrayLit (elt_ty, selems))


      | StructLit (name, field_inits) ->
        (* 1) resolve the alias or struct-name to an actual TyStruct *)
        let ty = resolve_type_expr env (TypeName name) in
        let real_struct_name =
          match ty with
          | TyStruct s -> s
          | _ -> raise (Semantic_error (name ^ " is not a struct type"))
        in
        (* 2) now fetch the real struct’s field list *)
        let sfields = find_struct real_struct_name env in
        let field_ty_map = List.fold_left(fun m (f : sfield) -> StringMap.add f.name f.field_type m)
          StringMap.empty sfields in
        let check_field (fname, fexpr) =
          if not (StringMap.mem fname field_ty_map) then raise (Semantic_error "unknown field");
          let expected_ty = StringMap.find fname field_ty_map in
          let actual_sexpr = check_expr env fexpr in
          let actual_ty = fst actual_sexpr in
          if not (ty_equal expected_ty actual_ty) then
            raise (Semantic_error (Printf.sprintf "Field %s type mismatch" fname));
          (fname, actual_sexpr) in
          let sfield_inits = List.map check_field field_inits in
          (* return the *resolved* struct type, but keep the literal name in the AST if you like *)
          (ty, SStructLit (name, sfield_inits))

      | SliceLit (te, elems) ->
        let elt_ty = resolve_type_expr env te in
        let selems = List.map (check_expr env) elems in
        List.iter (fun (t, _) -> if not (ty_equal t elt_ty) then
          raise (Semantic_error "Slice literal element type mismatch")) selems;
        (TySlice elt_ty, SSliceLit (elt_ty, selems))

      | Identifier id ->
        begin match find_value id env with
          | VVar t -> (t, SIdentifier id)
          | VFunc _ -> raise (Semantic_error (id ^ " is a function, not a variable"))
          end

      | FieldAccess (e, fname) ->
        let se = check_expr env e in
        let recv_ty = fst se in
        let fty =
          match recv_ty with
          | TyStruct sname ->
            let sfields = find_struct sname env in
            begin match List.find_opt(fun (f : sfield) -> f.name = fname) sfields with
            | None -> raise (Semantic_error (Printf.sprintf "Field %s not found in struct %s" fname sname))
            | Some f -> f.field_type
            end
          | _ -> raise (Semantic_error "Field access on non-struct type")
        in
        (fty, SFieldAccess (se, fname))

      | IndexAccess (coll, idx) ->
        let scoll = check_expr env coll in
        let sidx = check_expr env idx in

        (* Check index type *)
        if not (is_integer (fst sidx)) then
            raise (Semantic_error "index must be an integer type");

        let elt_ty =
          match fst scoll with
          | TyArray (t, _) | TySlice t -> t (* Indexing array/slice gives element type *)
          | TyPrim String -> TyPrim U8      (* Indexing string gives byte/char (u8) *)
          | TyTuple tys ->
              (* For tuples, require index to be a compile-time constant integer *)
              let index_val =
                match snd sidx with
                | SIntLit i -> i
                | _ -> raise (Semantic_error "tuple index must be an integer literal constant")
              in
              if index_val < 0 || index_val >= List.length tys then
                raise (Semantic_error (Printf.sprintf "tuple index %d out of bounds for tuple of size %d"
                                          index_val (List.length tys)))
              else
                List.nth tys index_val (* Return the type of the element at that index *)
          | ty -> raise (Semantic_error ("Type " ^ string_of_ty ty ^ " cannot be indexed (requires array, slice, string, or tuple)")) (* Updated error *)
        in
        (elt_ty, SIndexAccess (scoll, sidx))

      | SliceExpr (arr, lo, hi) ->
        let sarr = check_expr env arr in
        let slo = Option.map(check_expr env) lo in
        let shi = Option.map(check_expr env) hi in
        Option.iter (fun (t,_) -> if not (is_integer t) then
          raise (Semantic_error "slice index must be an integer")) slo;
        Option.iter (fun (t,_) -> if not (is_integer t) then
          raise (Semantic_error "slice index must be an integer")) shi;
        let elt_ty =
          match fst sarr with
          | TyArray (t, _) | TySlice t -> t
          | TyPrim String -> TyPrim String (* Gives you the substring *)
          | _ -> raise (Semantic_error "Slice on non-array/slice type")
        in
        (TySlice elt_ty, SSliceExpr (sarr, slo, shi))

      | Binop (e1, op, e2) ->
        let se1 = check_expr env e1 in
        let se2 = check_expr env e2 in
        let t = check_binop (fst se1) op (fst se2) in
        if t = TyError then
          raise (Semantic_error ("Type mismatch in binary operation"))
        else (t, SBinop (se1, op, se2))

      | Unaop (u, e1) ->
        let se1 = check_expr env e1 in
        begin match u with
        | Neg | Bitnot -> ensure_numeric (fst se1) "unary op"; (fst se1, SUnaop (u, se1))
        | Not -> ensure_bool (fst se1) "!"; (TyPrim Bool, SUnaop (u, se1))
        | Inc | Dec -> ensure_numeric (fst se1) "++/--"; (fst se1, SUnaop (u, se1))
        end

      | SimpleAssign (lhs, rhs) ->
        (* specific lhs check *)
        let slhs = check_expr env lhs in
        if not (is_lvalue (snd slhs)) then
          raise (Semantic_error "Left-hand side of assignment is not assignable");
        let srhs = check_expr env rhs in
        if not (ty_equal (fst slhs) (fst srhs)) then
          raise (Semantic_error "Assignment type mismatch")
        else (fst slhs, SSimpleAssign(slhs, srhs))

      | CompoundAssign (lhs, cop, rhs) ->
        (* Convert compond op to equivalent binop *)
        let binop_of_comp = function
          | PlusAssign -> Plus
          | MinusAssign -> Minus
          | TimesAssign -> Mult
          | DivAssign -> Div
          | ModAssign -> Mod
          | LshiftAssign -> Lshift
          | RshiftAssign -> Rshift
          | BitandAssign -> Bitand
          | BitorAssign -> Bitor
          | BitxorAssign -> Bitxor
        in
        let slhs = check_expr env lhs in
        if not (is_lvalue (snd slhs)) then
          raise (Semantic_error "Left-hand side of assignment is not assignable");
        let srhs = check_expr env rhs in
        ignore (check_binop (fst slhs) (binop_of_comp cop) (fst srhs));
        (fst slhs, SCompoundAssign(slhs, cop, srhs))

      | Sequence (e1, e2) ->
        let se1 = check_expr env e1 in
        let se2 = check_expr env e2 in
        (fst se2, SSequence (se1, se2))

      | FunctionCall (fname, args) ->
        (* 1) if fname is actually a type name and exactly one arg, do a numeric or identity cast *)
        if StringMap.mem fname env.types && List.length args = 1 then
          (* 1) semantically check the one argument, *keeping* the full sexpr *)
          let se_arg = check_expr env (List.hd args) in
          let src_ty = fst se_arg in

          (* 2) compute the target type *)
          let target_ty = resolve_type_expr env (TypeName fname) in

          (* 3) enforce your cast policy *)
          let ok =
            ty_equal src_ty target_ty           (* identity cast *)
            || (is_numeric src_ty && is_numeric target_ty)
          in
          if not ok then
            raise (Semantic_error
              (Printf.sprintf "illegal cast from %s to %s"
                  (string_of_ty src_ty)
                  (string_of_ty target_ty)));

          (* 4) build the *sexpr*: pair the new ty with an sx node *)
          ( target_ty,
            SCast (target_ty, se_arg) )
        else
          (* 2) otherwise do ordinary function‐call lookup *)
          begin match find_value fname env with
          | VFunc fsig ->
            if List.length args <> List.length fsig.params then
              raise (Semantic_error (Printf.sprintf "Function %s expects %d args" fname (List.length fsig.params)));
            let sargs = List.map (check_expr env) args in
            List.iter2 (fun (a_ty, _) p_ty -> if not (ty_equal a_ty p_ty) then
              raise (Semantic_error ("argument type mismatch in call to " ^ fname))) sargs fsig.params;
            (* Not sure what we should return for void function *)
            let ret_ty = match fsig.returns with [] -> TyUnit | [t] -> t | ts -> TyTuple ts in
            (ret_ty, SFunctionCall(fname, sargs))
          | _ -> raise (Semantic_error (fname ^ " is not a function"))
          end

      | MethodCall (recv, mname, args) ->
        (* Minimal support: treat struct methods as free functions structName$method(recv, ...) already collected later *)
        let srecv = check_expr env recv in
        let recv_ty = fst srecv in
        begin match recv_ty with
        | TyStruct sname ->
            (* search for a function named sname$mname *)
            let mangled = sname ^ "$" ^ mname in
            begin match find_value mangled env with
            | VFunc fsig ->
                if List.length args <> List.length fsig.params - 1 then
                  raise (Semantic_error "method call arg count");
                let sargs = List.map (check_expr env) args in
                (* compare receiver type *)
                if not (ty_equal recv_ty (List.hd fsig.params)) then
                  raise (Semantic_error "receiver type mismatch");
                List.iter2 (fun (aty,_) pty -> if not (ty_equal aty pty) then
                  raise (Semantic_error "method arg type mismatch")) sargs (List.tl fsig.params);
                let ret_ty = match fsig.returns with [] -> TyUnit | [t] -> t | _::_ -> TyError in
                (ret_ty, SMethodCall(srecv, mname, sargs))
            | _ -> raise (Semantic_error (Printf.sprintf "Unknown method %s for struct %s" mname sname))
            end
        | _ -> raise (Semantic_error "method call on non‑struct")
        end

      | Make (te, len_expr, cap_expr_opt) ->
        let elt_ty = resolve_type_expr env te in
        let slen = check_expr env len_expr in
        if not (is_integer (fst slen)) then
          raise (Semantic_error "make length argument must be an integer");
        let scap_opt = Option.map (check_expr env) cap_expr_opt in
        Option.iter (fun (t,_) -> if not (is_integer t) then
          raise (Semantic_error "make capacity argument must be an integer"))scap_opt;
        (TySlice elt_ty, SMake(elt_ty, slen, scap_opt))

      | Cast (ast_ty, e) ->
        let target = resolve_type_expr env ast_ty in
        let se = check_expr env e in
        if ty_equal target (fst se) then (target, snd se |> fun sx -> SCast (target, (fst se, sx)))
        else if is_numeric target && is_numeric (fst se) then (target, SCast (target, se))
        else raise (Semantic_error (Printf.sprintf "invalid cast from %s to %s" (string_of_ty (fst se)) (string_of_ty target)))

  (* Statement checking *)
  let rec check_block env expected stmts : sstmt list =
    let rec aux local_env = function
      | [] -> []
      | st :: t1 ->
        let env', sst = check_stmt local_env expected st in
        sst :: aux env' t1
    in
    let reversed_stmts = List.rev stmts in
    aux { env with values = env.values } reversed_stmts

  and check_stmt env expected = function
    | Expr e ->
      let se = check_expr env e in
      (env, SExpr se)

    | VarDecl {is_const; name; var_type; initializer_expr} ->
      if StringMap.mem name env.values then
        raise (Semantic_error ("Variable already declared"));
      let inferred_ty, sinit =
        match (var_type, initializer_expr) with
        | Some te, Some ie ->
          let declared_ty = resolve_type_expr env te in
          let sie = check_expr env ie in
          if not (ty_equal declared_ty (fst sie)) then
            raise (Semantic_error "Type mismatch in variable declaration");
          (declared_ty, Some sie)
        | Some te, None -> (resolve_type_expr env te, None)
        | None, Some ie -> let sie = check_expr env ie in (fst sie, Some sie)
        | None, None -> raise (Semantic_error "Cannot infer type that hasn't been initialized")
      in
      let env' = add_value name (VVar inferred_ty) env in
      (env', SVarDecl {is_const; name; var_type = inferred_ty; initializer_expr = sinit})

    | Block b ->
      let sbody = check_block env expected b in
      (env, SBlock sbody)

    | IfStmt (cond, then_blk, else_opt) ->
      let scond = check_expr env cond in
      ensure_bool (fst scond) "if condition";
      let sthen = check_block env expected then_blk in
      let selse_opt =
        match else_opt with
        | None -> None
        | Some s -> let _, sst = check_stmt env expected s in Some sst
      in
      (env, SIf (scond, sthen, selse_opt))

    | WhileStmt (cond, body) ->
      let scond = check_expr env cond in
      ensure_bool (fst scond) "while condition";
      let sbody = check_block env expected body in
      (env, SWhile (scond, sbody))

    | ForStmt (init_opt, cond_opt, update_opt, body) ->
      let env_after_init, sinit_opt =
      match init_opt with
        | None -> (env, None)
        | Some st -> let env', sst = check_stmt env expected st in (env', Some sst)
      in
      let scond_opt = match cond_opt with
        | None -> None
        | Some c -> let sc = check_expr env_after_init c in ensure_bool (fst sc) "for condition"; Some sc
      in
      let supd_opt = Option.map (check_expr env_after_init) update_opt in
      let sbody = check_block env_after_init expected body in
      (env, SFor (sinit_opt, scond_opt, supd_opt, sbody))

    | Return rexprs_opt ->
      let srexprs_opt = Option.map (List.map (check_expr env)) rexprs_opt in
      check_return expected srexprs_opt;
      (env, SReturn srexprs_opt)

    | Break ->
      (env, SBreak)

    | Continue ->
      (env, SContinue)

  and check_return expected sexprs_opt =
    match expected, sexprs_opt with
    | [], None -> () (* void function *)
    | [], Some _ -> raise (Semantic_error "Return statement with non-void function")
    | _, None -> raise (Semantic_error "Return statement with no value")
    | etys, Some actuals ->
      if List.length etys <> List.length actuals then
        raise (Semantic_error "Return statement with wrong number of arguments");
      List.iter2
        (fun ety (aty, _) -> if not (ty_equal ety aty) then
          raise (Semantic_error "Return type mismatch"))
        etys actuals

  (* ---- Top level declarations ----- *)
  let check_field env (f : field) : sfield =
    {
      name = f.name;
      field_type = resolve_type_expr env f.field_type;
      modifier = f.modifier;
      default_value = Option.map (check_expr env) f.default_value
    }

  let rec collect_structs env = function
    | [] -> env
    | TypeStruct (name, fields) :: tl ->
        if StringMap.mem name env.structs then
          raise (Semantic_error ("Duplicate struct " ^ name));
        let sfields = List.map (check_field env) fields in
        collect_structs (add_struct name sfields env) tl
    | _ :: tl -> collect_structs env tl

  let extract_func_sig (fd : func_decl) env : func_sig =
    let param_types = List.map (fun (p : param) -> resolve_type_expr env p.param_type) fd.params in
    let ret_types   = List.map (resolve_type_expr env) fd.return_types in
    { params = param_types; returns = ret_types }

  let add_func_header env (fd : func_decl) : env =
    if StringMap.mem fd.name env.values then
      raise (Semantic_error ("Duplicate function " ^ fd.name));
    let fsig = extract_func_sig fd env in
    add_value fd.name (VFunc fsig) env

  let check_function env (fd : func_decl) : sfunc_decl =
    let fsig = match find_value fd.name env with VFunc s -> s | _ -> assert false in
    (* Build an environment containing parameters *)
    let env_with_params =
      List.fold_left2 (fun e (p : param) ty -> add_value p.name (VVar ty) e) env fd.params fsig.params
    in
    let sbody = check_block env_with_params fsig.returns fd.body in
    {
      name = fd.name;
      params = List.map2 (fun (p : param) t -> { name = p.name; param_type = t }) fd.params fsig.params;
      return_types = fsig.returns;
      body = sbody;
    }

  let extract_method_sig env (md: struct_func) : string * func_sig =
    let recv_ty = TyStruct md.struct_name in
    let param_types =
      recv_ty :: List.map(fun (p : param) -> resolve_type_expr env p.param_type) md.params
    in
    let return_types = List.map(resolve_type_expr env) md.return_types in
    (md.struct_name ^ "$" ^ md.name, { params = param_types; returns = return_types })

  let add_method_header env (md: struct_func) : env =
    (* Make sure the struct exists and the name is still free *)
    if not (StringMap.mem md.struct_name env.structs) then
      raise (Semantic_error ("Struct " ^ md.struct_name ^ " not found"));
    let mangled, fsig = extract_method_sig env md in
    if StringMap.mem mangled env.values then
      raise (Semantic_error (Printf.sprintf "Duplicate method %s for struct %s"
                                   md.name md.struct_name));
    add_value mangled (VFunc fsig) env

  let check_struct_method env (md : struct_func) : sstruct_func =
    let mangled = mangle md.struct_name md.name in
    let msig =
      match find_value mangled env with
      | VFunc s -> s
      | _ -> assert false
    in
    (* Build a local env *)
    let env_with_recv = add_value md.receiver_name (VVar (List.hd msig.params)) env in
    let env_with_params =
      List.fold_left2 (fun e (p : param) ty -> add_value p.name (VVar ty) e) env_with_recv md.params (List.tl msig.params)
    in
    (* Check block accepts env as first argument *)
    let sbody = check_block env_with_params msig.returns md.body in
    {
      name = md.name;
      receiver_name = md.receiver_name;
      struct_name = md.struct_name;
      params = List.map2(fun (p : param) ty -> {name = p.name; param_type = ty}) md.params (List.tl msig.params);
      return_types = msig.returns;
      body = sbody;
    }

  (* ----- Program entry ------ *)
  let check_program (p : program) : sprogram =
    (* ------------------------------------------------------ *)
    (* Pass 0 - declare every struct & alias name - no bodies *)
    (* ------------------------------------------------------ *)
    let env0  =
      List.fold_left (fun e td ->
        match td with
        | TypeStruct (n, _) ->
          if StringMap.mem n e.types then dup n "type";
          add_type n (TyStruct n) e
        | TypeAlias (n, _) ->
          if StringMap.mem n e.types then dup n "type";
          add_type n TyError e)
      empty_env p.type_declarations
    in
    (* ------------------------------------------------------ *)
    (* Pass 1 - resolve alias RHS types now that names exist *)
    (* ------------------------------------------------------ *)
    let env1 =
      List.fold_left (fun e td ->
        match td with
        | TypeAlias (name, te) ->
            add_type name (resolve_type_expr e te) e
        | _ -> e
      ) env0 p.type_declarations
    in
    (* ------------------------------------------------------ *)
    (* Pass 2 - resolve struct fields *)
    (* ------------------------------------------------------ *)
    let env2, stype_decls =
      List.fold_left (fun (e_acc, sd_acc) td ->
        match td with
        | TypeStruct (n, flds) ->
          let sfields = List.map (check_field e_acc) flds in
          let e' = add_struct n sfields e_acc in
          (e', sd_acc @ [STypeStruct (n, sfields)])
        | TypeAlias (n, _) ->
          (e_acc, sd_acc @ [STypeAlias (n, find_type n e_acc)])
      ) (env1, []) p.type_declarations
    in
    (* Phase 3: enter all function headers so functions can mutually recurse. *)
    let env3 = List.fold_left add_func_header env2 p.functions in
    (* Phase 3b: collect method headers so they can mutually recurse with ordinary functions
      or with one another *)
    let env3b = List.fold_left add_method_header env3 p.struct_functions in
    (* Phase 4: check globals. *)
    let env4, sglobals =
      List.fold_left (fun (e_acc, sg_acc) (g : global_decl) ->
        if StringMap.mem g.name e_acc.values then
          raise (Semantic_error ("Duplicate global " ^ g.name));
        let inferred_ty, sinit =
          match g.var_type, g.initializer_expr with
          | Some te, Some ie ->
            let t = resolve_type_expr e_acc te in
            let sie = check_expr e_acc ie in
            if not (ty_equal t (fst sie)) then
              raise (Semantic_error "Global initializer mismatch");
            (t, Some sie)
          | Some te, None -> (resolve_type_expr e_acc te, None)
          | None, Some ie -> let sie = check_expr e_acc ie in (fst sie, Some sie)
          | None, None -> raise (Semantic_error "Cannot infer global type")
        in
        let e' = add_value g.name (VVar inferred_ty) e_acc in
        let sg = { is_const = g.is_const; name = g.name; var_type = inferred_ty; initializer_expr = sinit } in
        (e', sg_acc @ [sg])
      ) (env3b, []) p.global_vars
    in
    (* Phase 5: fully check each function body. *)
    let sfuncs = List.map (check_function env4) p.functions in
    (* Phase 5b: fully check each struct method body. *)
    let smethods = List.map (check_struct_method env4) p.struct_functions in

    {
      sp_package = p.package_name;
      sp_imports = p.imports;
      sp_types   = stype_decls;
      sp_globals = sglobals;
      sp_funcs   = sfuncs;
      sp_methods = smethods;
    }
