open Ast
open Sast

exception Semantic_error of string

module StringMap = Map.Make(String)

(* --- Local String Conversion Helpers for Error Messages --- *)
let local_string_of_biop = function
  | Plus -> "+" | Minus -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | And -> "&&" | Or -> "||"
  | Lshift -> "<<" | Rshift -> ">>" | Bitand -> "&" | Bitor -> "|" | Bitxor -> "^"

let local_string_of_unop = function
  | Neg -> "-" | Not -> "!" | Bitnot -> "~" | Inc -> "++" | Dec -> "--"

let local_string_of_compound_op = function
  | PlusAssign -> "+=" | MinusAssign -> "-=" | TimesAssign -> "*=" | DivAssign -> "/=" | ModAssign -> "%="
  | LshiftAssign -> "<<=" | RshiftAssign -> ">>=" | BitandAssign -> "&=" | BitxorAssign -> "^=" | BitorAssign -> "|="

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

    let initial_env =
      let buit_in_funcs =
        List.fold_left (fun acc (name, func_sig) -> StringMap.add name (VFunc func_sig) acc)
        StringMap.empty
        [
          ("printf", {params = [TyPrim Ast.String]; returns = [TyPrim Ast.I32]});
          ("print_int", {params = []; returns = [TyPrim Ast.I32]});
          ("print_float", {params = []; returns = [TyPrim Ast.I32]});
          ("print_fancy", {params = [TyPrim Ast.String; TyPrim Ast.I32; TyPrim Ast.I32; TyPrim Ast.Bool; TyPrim Ast.Bool]; returns = [TyPrim Ast.I32]});
          ("len", {params = [TyPrim Ast.String]; returns = [TyPrim Ast.I32]});
          ("cap", {params = [TyPrim Ast.String]; returns = [TyPrim Ast.I32]});
          ("assert", {params = [TyPrim Ast.Bool]; returns = []});
          ("exit", {params = [TyPrim Ast.I32]; returns = []})
        ]
      in
    {empty_env with values = buit_in_funcs}

    let add_type name t env =
      { env with types = StringMap.add name t env.types}

    let add_value name v env =
      { env with values = StringMap.add name v env.values}

    let add_struct name fields env =
      { env with structs = StringMap.add name fields env.structs}

    let find_type name env =
      match StringMap.find_opt name env.types with
      | Some v -> v
      | None -> raise (Semantic_error (Printf.sprintf "Type '%s' not found" name))

    let find_value name env =
      match StringMap.find_opt name env.values with
      | Some v -> v
      | None -> raise (Semantic_error (Printf.sprintf "Identifier '%s' not found" name))

    let find_struct name env =
      match StringMap.find_opt name env.structs with
      | Some v -> v
      | None -> raise (Semantic_error (Printf.sprintf "Struct '%s' not found" name))

    (* ----- Helpers ----- *)

    let numeric_prim = function
      | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | F64 | F32 -> true
      | _ -> false

    let int_prim = function
      | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 -> true
      | _ -> false

    let is_numeric = function TyPrim p when numeric_prim p -> true | _ -> false
    let is_integer = function TyPrim p when int_prim p -> true | _ -> false
    let is_boolean = function TyPrim Bool -> true | _ -> false
    let is_nullable = function TyNull | TyStruct _ -> true | _ -> false
    let is_string = function TyPrim String -> true | _ -> false

    let ensure_numeric ty ctx =
      if not (is_numeric ty) then
        raise (Semantic_error (Printf.sprintf "%s: numeric type required, but got %s" ctx (string_of_ty ty)))

    let ensure_bool ty ctx =
      if not (is_boolean ty) then
        raise (Semantic_error (Printf.sprintf "%s: boolean type required, but got %s" ctx (string_of_ty ty)))

    let rec ty_equal t1 t2 =
      match t1, t2 with
      | TyNull, t | t, TyNull -> is_nullable t
      | TyPrim p1, TyPrim p2 -> p1 = p2
      | TyArray (et1, n1), TyArray (et2, n2) -> n1 = n2 && ty_equal et1 et2
      (* | TySlice et1, TySlice et2 -> ty_equal et1 et2 *)
      | TyStruct s1, TyStruct s2 -> s1 = s2
      | TyTuple ts1, TyTuple ts2 -> List.length ts1 = List.length ts2 && List.for_all2 ty_equal ts1 ts2
      | TyError, _
      | _, TyError -> true (* allow error propagation*)
      | _ -> false

    let rec resolve_type_expr env (t: type_expr) : ty =
      match t with
      | Primitive p -> TyPrim p
      | Array (te, n) -> TyArray (resolve_type_expr env te, n)
      (* | Slice te -> TySlice (resolve_type_expr env te) *)
      | Struct s ->
        if not (StringMap.mem s env.structs) then
          raise (Semantic_error (Printf.sprintf "Struct '%s' used in type expression not found" s));
        TyStruct s
      | TypeName n -> find_type n env

    let mangle (struct_name : string) (method_name : string) : string =
      struct_name ^ "$" ^ method_name

    let dup n what = raise (Semantic_error (Printf.sprintf "Duplicate %s '%s'" what n))

    let is_lvalue = function
      | SIdentifier _ -> true
      | SFieldAccess (_,_) -> true
      | SIndexAccess (_, _) -> true
      | _ -> false

    (* Expression checking *)
    let check_binop t1 op t2 =
      match op with
      (* String ops *)
      | Plus when is_string t1 && is_string t2 -> TyPrim String
      | Eq | Neq when is_string t1 && is_string t2 -> TyPrim Bool

      (* Arithmetic operations that can involve floats and type promotion *)
      | Plus | Minus | Mult | Div ->
          if is_numeric t1 && is_numeric t2 then
            if ty_equal t1 t2 then t1 (* e.g., I32+I32->I32, F32+F32->F32 *)
            (* Promotion: int and F32 -> F32 *)
            else if (is_integer t1 && t2 = TyPrim F32) || (t1 = TyPrim F32 && is_integer t2) then TyPrim F32
            (* Promotion: int and F64 -> F64 (assuming F64 is a distinct float type) *)
            else if (is_integer t1 && t2 = TyPrim F64) || (t1 = TyPrim F64 && is_integer t2) then TyPrim F64
            (* Promotion: F64 and F32 -> F32 *)
            else if (t1 = TyPrim F64 && t2 = TyPrim F32) || (t1 = TyPrim F32 && t2 = TyPrim F64) then TyPrim F32
            else TyError (* Other mixed numeric types (e.g. different int types without explicit rule, or other float combos) *)
          else TyError

      (* Integer-only or same-type bitwise/mod operations *)
      | Mod | Lshift | Rshift | Bitand | Bitxor | Bitor ->
          if is_integer t1 && is_integer t2 && ty_equal t1 t2 then t1 (* Must be same integer type *)
          else TyError

      (* Comparison operations *)
      | Eq | Neq | Lt | Le | Gt | Ge ->
          if is_numeric t1 && is_numeric t2 then
            (* Allow comparison between different numeric types if they can be promoted/compared *)
            if ty_equal t1 t2 || (* Same types *)
               (is_integer t1 && (t2 = TyPrim F32 || t2 = TyPrim F64)) || (* int vs float *)
               ((t1 = TyPrim F32 || t1 = TyPrim F64) && is_integer t2) || (* float vs int *)
               (t1 = TyPrim F64 && t2 = TyPrim F32) || (t1 = TyPrim F32 && t2 = TyPrim F64) (* F64 vs F32 *)
            then TyPrim Bool
            else TyError
          else if is_string t1 && is_string t2 && ty_equal t1 t2 then TyPrim Bool (* String comparison *)
          else if is_boolean t1 && is_boolean t2 && ty_equal t1 t2 then TyPrim Bool (* Bool comparison *)
          else TyError

      | And | Or -> (* Boolean ops *)
          if is_boolean t1 && is_boolean t2 then TyPrim Bool
          else TyError


    let rec check_expr env (e : expr) : sexpr =
      match e with
      | IntLit i -> (TyPrim I32, SIntLit i) (* I32 is the default *)
      | BoolLit b -> (TyPrim Bool, SBoolLit b)
      | CharLit c -> (TyPrim U8, SCharLit c)
      | FloatLit f -> (TyPrim F32, SFloatLit f)
      | StringLit s -> (TyPrim String, SStringLit s)
      | Null -> (TyNull, SNull)

      | ArrayLit (arr_ty_expr, elems) ->
        let resolved_ty = resolve_type_expr env arr_ty_expr in
        let (elt_ty, expected_len) =
          match resolved_ty with
          | TyArray (et, len) -> (et, len)
          | _ -> raise (Semantic_error (Printf.sprintf "Array literal's type specification must be an array type, but got %s"
                                          (string_of_ty resolved_ty)))
        in
        if List.length elems <> expected_len then
          raise (Semantic_error (Printf.sprintf "Array literal size mismatch for type %s: expected %d elements, got %d"
                                    (string_of_ty resolved_ty) expected_len (List.length elems)));
        let selems = List.map (check_expr env) elems in
        List.iter (fun (actual_ty, _) ->
          if not (ty_equal actual_ty elt_ty) then
            raise (Semantic_error (Printf.sprintf "Array literal element type mismatch for array of type %s: expected element type %s, got %s"
                                      (string_of_ty resolved_ty) (string_of_ty elt_ty) (string_of_ty actual_ty)))
        ) selems;
        (resolved_ty, SArrayLit (elt_ty, selems))


      | StructLit (name, field_inits) ->
        let ty = resolve_type_expr env (TypeName name) in
        let real_struct_name =
          match ty with
          | TyStruct s -> s
          | _ -> raise (Semantic_error (Printf.sprintf "'%s' is not a struct type, but %s" name (string_of_ty ty)))
        in
        let sfields = find_struct real_struct_name env in
        let field_ty_map = List.fold_left(fun m (f : sfield) -> StringMap.add f.name f.field_type m)
          StringMap.empty sfields in
        let check_field (fname, fexpr) =
          if not (StringMap.mem fname field_ty_map) then
            raise (Semantic_error (Printf.sprintf "Struct literal for '%s' has unknown field '%s'" real_struct_name fname));
          let expected_ty = StringMap.find fname field_ty_map in
          let actual_sexpr = check_expr env fexpr in
          let actual_ty = fst actual_sexpr in
          if not (ty_equal expected_ty actual_ty) then
            raise (Semantic_error (Printf.sprintf "Field '%s' in struct literal '%s' type mismatch: expected %s, got %s"
                                      fname real_struct_name (string_of_ty expected_ty) (string_of_ty actual_ty)));
          (fname, actual_sexpr) in
          let sfield_inits = List.map check_field field_inits in
          (ty, SStructLit (name, sfield_inits))

      (* | SliceLit (te, elems) ->
        let elt_ty = resolve_type_expr env te in
        let selems = List.map (check_expr env) elems in
        List.iter (fun (t, _) -> if not (ty_equal t elt_ty) then
          raise (Semantic_error (Printf.sprintf "Slice literal element type mismatch: expected element type %s, got %s"
                                    (string_of_ty elt_ty) (string_of_ty t)))) selems;
        (TySlice elt_ty, SSliceLit (elt_ty, selems)) *)

      | Identifier id ->
        begin match find_value id env with
          | VVar t -> (t, SIdentifier id)
          | VFunc _ -> raise (Semantic_error (Printf.sprintf "'%s' is a function, not a variable. Use function call syntax if you intend to call it." id))
          end

      | FieldAccess (e, fname) ->
        let se = check_expr env e in
        let recv_ty = fst se in
        let fty =
          match recv_ty with
          | TyStruct sname ->
            let sfields = find_struct sname env in
            begin match List.find_opt(fun (f : sfield) -> f.name = fname) sfields with
            | None -> raise (Semantic_error (Printf.sprintf "Field '%s' not found in struct '%s'" fname sname))
            | Some f -> f.field_type
            end
          | _ -> raise (Semantic_error (Printf.sprintf "Field access '%s' on non-struct type %s" fname (string_of_ty recv_ty)))
        in
        (fty, SFieldAccess (se, fname))

      | IndexAccess (coll, idx) ->
        let scoll = check_expr env coll in
        let sidx = check_expr env idx in
        let ty_idx = fst sidx in
        if not (is_integer ty_idx) then
            raise (Semantic_error (Printf.sprintf "Index expression must be an integer type, but got %s" (string_of_ty ty_idx)));
        let elt_ty =
          match fst scoll with
          | TyArray (t, _) -> t
          (* | TySlice t -> t *)
          | TyPrim String -> TyPrim U8
          | TyTuple tys ->
              let index_val =
                match snd sidx with
                | SIntLit i -> i
                | _ -> raise (Semantic_error "Tuple index must be an integer literal constant")
              in
              if index_val < 0 || index_val >= List.length tys then
                raise (Semantic_error (Printf.sprintf "Tuple index %d out of bounds for tuple of size %d"
                                          index_val (List.length tys)))
              else
                List.nth tys index_val
          | ty -> raise (Semantic_error (Printf.sprintf "Type %s cannot be indexed (requires array, string, or tuple)" (string_of_ty ty)))
        in
        (elt_ty, SIndexAccess (scoll, sidx))

      (* | SliceExpr (arr, lo, hi) ->
        let sarr = check_expr env arr in
        let slo = Option.map(check_expr env) lo in
        let shi = Option.map(check_expr env) hi in
        Option.iter (fun (t,_) -> if not (is_integer t) then
          raise (Semantic_error (Printf.sprintf "Slice 'low' index must be an integer type, got %s" (string_of_ty t)))) slo;
        Option.iter (fun (t,_) -> if not (is_integer t) then
          raise (Semantic_error (Printf.sprintf "Slice 'high' index must be an integer type, got %s" (string_of_ty t)))) shi;
        let (_, result_slice_ty) =
          match fst sarr with
          | TyArray (t, _) -> (t, TySlice t)
          | TySlice t -> (t, TySlice t)
          | TyPrim String -> (TyPrim U8, TyPrim String) (* Slicing string gives string, elements are u8 conceptually *)
          | ty -> raise (Semantic_error (Printf.sprintf "Slice operation on invalid type %s (requires array, slice or string)" (string_of_ty ty)))
        in
        (result_slice_ty, SSliceExpr (sarr, slo, shi)) *)


      | Binop (e1, op, e2) ->
        let se1 = check_expr env e1 in
        let se2 = check_expr env e2 in
        let t1 = fst se1 and t2 = fst se2 in
        let res_t = check_binop t1 op t2 in
        if res_t = TyError then
          raise (Semantic_error (Printf.sprintf "Type mismatch in binary operation '%s': cannot apply to operands of type %s and %s"
                                    (local_string_of_biop op) (string_of_ty t1) (string_of_ty t2)))
        else (res_t, SBinop (se1, op, se2))

      | Unaop (u, e1) ->
        let se1 = check_expr env e1 in
        let t1 = fst se1 in
        begin match u with
        | Neg | Bitnot -> ensure_numeric t1 (Printf.sprintf "Unary operator '%s'" (local_string_of_unop u)); (t1, SUnaop (u, se1))
        | Not -> ensure_bool t1 (Printf.sprintf "Unary operator '%s'" (local_string_of_unop u)); (TyPrim Bool, SUnaop (u, se1))
        | Inc | Dec -> ensure_numeric t1 (Printf.sprintf "Unary operator '%s'" (local_string_of_unop u)); (t1, SUnaop (u, se1))
        end

      | SimpleAssign (lhs, rhs) ->
        let slhs = check_expr env lhs in
        let ty_lhs = fst slhs in
        if not (is_lvalue (snd slhs)) then
          (* It's hard to stringify 'lhs' (Ast.expr) here well. SExpr to string isn't a parser. *)
          raise (Semantic_error "Left-hand side of assignment is not an assignable l-value");
        let srhs = check_expr env rhs in
        let ty_rhs = fst srhs in
        if not (ty_equal ty_lhs ty_rhs) then
          raise (Semantic_error (Printf.sprintf "Assignment type mismatch: cannot assign type %s to type %s"
                                    (string_of_ty ty_rhs) (string_of_ty ty_lhs)))
        else (ty_lhs, SSimpleAssign(slhs, srhs))

      | CompoundAssign (lhs, cop, rhs) ->
        let binop_of_comp = function
          | PlusAssign -> Plus | MinusAssign -> Minus | TimesAssign -> Mult | DivAssign -> Div | ModAssign -> Mod
          | LshiftAssign -> Lshift | RshiftAssign -> Rshift | BitandAssign -> Bitand | BitorAssign -> Bitor | BitxorAssign -> Bitxor
        in
        let slhs = check_expr env lhs in
        let ty_lhs = fst slhs in
        if not (is_lvalue (snd slhs)) then
          raise (Semantic_error (Printf.sprintf "Left-hand side of compound assignment '%s' is not an assignable l-value" (local_string_of_compound_op cop)));
        let srhs = check_expr env rhs in
        let ty_rhs = fst srhs in
        let equiv_binop = binop_of_comp cop in
        let op_result_ty = check_binop ty_lhs equiv_binop ty_rhs in

        if op_result_ty = TyError then
          raise (Semantic_error (Printf.sprintf "Invalid types for compound assignment '%s': cannot apply underlying '%s' operation to %s and %s"
                                    (local_string_of_compound_op cop) (local_string_of_biop equiv_binop) (string_of_ty ty_lhs) (string_of_ty ty_rhs)))
        else if not (ty_equal ty_lhs op_result_ty) then
           (* This case catches if e.g. `bool_var &= int_val` where `bool & int` might be valid and result in `int`, but `int` cannot be assigned back to `bool_var` *)
             raise (Semantic_error (Printf.sprintf "Type mismatch in compound assignment '%s': result of (%s %s %s) is %s, which cannot be assigned to LHS of type %s"
                (local_string_of_compound_op cop) (string_of_ty ty_lhs) (local_string_of_biop equiv_binop) (string_of_ty ty_rhs)
                (string_of_ty op_result_ty) (string_of_ty ty_lhs)))
        else (ty_lhs, SCompoundAssign(slhs, cop, srhs))


      | Sequence (e1, e2) ->
        let se1 = check_expr env e1 in
        let se2 = check_expr env e2 in
        (fst se2, SSequence (se1, se2))
      
      | FunctionCall ("len", [input]) ->
          let sexpr =
              check_expr env input
          in
          (match fst sexpr with
          | TyPrim String
          | TyArray _ ->
              (TyPrim I32, SFunctionCall ("len", [sexpr]))
          | _ -> raise (Semantic_error "len() cannot be applied"))

      | FunctionCall ("cap", [input]) ->
          let sexpr =
              check_expr env input
          in
          (match fst sexpr with
          | TyArray _ ->
              (TyPrim I32, SFunctionCall ("cap", [sexpr]))
          | _ -> raise (Semantic_error "cap() cannot be applied"))

      | FunctionCall ("assert", [input]) ->
          let sexpr = 
              check_expr env input 
          in 
          (match fst sexpr with
          | TyPrim Bool ->
              (TyUnit, SFunctionCall ("assert", [sexpr]))
          | _ -> raise (Semantic_error "assert() cannot be applied"))

      | FunctionCall (fname, args) ->
          begin match find_value fname env with
          | VFunc fsig ->
            if fname = "print_int" || fname = "print_float" 
            then let sargs = List.map (check_expr env) args in (List.hd fsig.returns,SFunctionCall (fname,sargs)) else
            if List.length args <> List.length fsig.params then
              raise (Semantic_error (Printf.sprintf "Function '%s' expects %d argument(s) but got %d"
                                        fname (List.length fsig.params) (List.length args))) else
            let sargs = List.map (check_expr env) args in
            List.iteri (fun i p_ty ->
                let (a_ty, _) = List.nth sargs i in
                if not (ty_equal a_ty p_ty) then
                  raise (Semantic_error (Printf.sprintf "Argument type mismatch in call to function '%s': argument %d expected type %s, got %s"
                                            fname (i+1) (string_of_ty p_ty) (string_of_ty a_ty)))
            ) fsig.params;
            let ret_ty = match fsig.returns with [] -> TyUnit | [t] -> t | ts -> TyTuple ts in
            (ret_ty, SFunctionCall(fname, sargs))
          | VVar v_ty -> raise (Semantic_error (Printf.sprintf "'%s' is a variable of type %s, not a function" fname (string_of_ty v_ty)))
          (* Note: find_value would raise if name is not found at all *)
          end

      | MethodCall (recv, mname, args) ->
        let srecv = check_expr env recv in
        let recv_ty = fst srecv in
        begin match recv_ty with
        | TyStruct sname ->
            let mangled = mangle sname mname in
            begin match find_value mangled env with
            | VFunc fsig ->
                (* fsig.params includes receiver as first param *)
                if List.length args <> (List.length fsig.params - 1) then
                  raise (Semantic_error (Printf.sprintf "Method '%s.%s' expects %d argument(s) but got %d"
                                            sname mname (List.length fsig.params - 1) (List.length args)));
                let sargs = List.map (check_expr env) args in
                if not (ty_equal recv_ty (List.hd fsig.params)) then
                  raise (Semantic_error (Printf.sprintf "Receiver type mismatch for method '%s.%s': method defined for %s, called on %s"
                                            sname mname (string_of_ty (List.hd fsig.params)) (string_of_ty recv_ty)));
                List.iteri (fun i p_ty ->
                    let (a_ty, _) = List.nth sargs i in
                    if not (ty_equal a_ty p_ty) then
                      raise (Semantic_error (Printf.sprintf "Argument type mismatch in call to method '%s.%s': argument %d expected type %s, got %s"
                                                sname mname (i+1) (string_of_ty p_ty) (string_of_ty a_ty)))
                ) (List.tl fsig.params); (* Compare against fsig.params excluding the receiver *)
                let ret_ty = match fsig.returns with [] -> TyUnit | [t] -> t | ts -> TyTuple ts in
                (ret_ty, SMethodCall(srecv, mname, sargs))
            | VVar v_ty -> raise (Semantic_error (Printf.sprintf "Identifier '%s' (mangled from method '%s.%s') is a variable of type %s, not a method/function"
                                                    mangled sname mname (string_of_ty v_ty)))
            (* find_value would raise "Value 'mangled_name' not found" if the mangled name isn't in env.values *)
            end
        | _ -> raise (Semantic_error (Printf.sprintf "Method call '%s' on non-struct type %s" mname (string_of_ty recv_ty)))
        end

      (* | Make (ast_slice_type_expr, len_expr, cap_expr_opt) ->
        let resolved_sast_slice_type = resolve_type_expr env ast_slice_type_expr in
        let sast_element_type =
          match resolved_sast_slice_type with
          | TySlice et -> et
          | _ -> raise (Semantic_error (Printf.sprintf "'make' expects a slice type as its first argument (e.g. []Point or []int). Got type: %s" (string_of_ty resolved_sast_slice_type)))
        in
        let slen = check_expr env len_expr in
        if not (is_integer (fst slen)) then
          raise (Semantic_error (Printf.sprintf "'make' length argument must be an integer type, but got %s" (string_of_ty (fst slen))));
        let scap_opt = Option.map (check_expr env) cap_expr_opt in
        Option.iter (fun (t,_) -> if not (is_integer t) then
          raise (Semantic_error (Printf.sprintf "'make' capacity argument must be an integer type, but got %s" (string_of_ty t)))) scap_opt;
        (resolved_sast_slice_type, SMake(sast_element_type, slen, scap_opt)) *)

      | Cast (ast_ty, e) ->
        let target_ty = resolve_type_expr env ast_ty in
        let se = check_expr env e in
        let src_ty = fst se in
        if ty_equal target_ty src_ty then (target_ty, SCast (target_ty, se)) (* Identity cast *)
        else if is_numeric target_ty && is_numeric src_ty then (target_ty, SCast (target_ty, se)) (* Numeric cast *)
        else raise (Semantic_error (Printf.sprintf "Invalid cast from type %s to type %s"
                                      (string_of_ty src_ty) (string_of_ty target_ty)))

  (* Statement checking *)
  let rec check_block env expected stmts_from_ast : sstmt list =
    (* stmts_from_ast is in reverse source order due to ocamlyacc typical list building,
       e.g. [stmt_N; ...; stmt_1] *)
    let stmts_in_source_order = List.rev stmts_from_ast in

    (* Helper to iterate through statements in source order, updating the environment *)
    let rec process_stmts_in_order current_env processed_sast_stmts_acc = function
      | [] -> List.rev processed_sast_stmts_acc (* Accumulator is reversed, so reverse back at the end *)
      | ast_stmt :: rest_ast_stmts ->
          let env_after_this_stmt, sast_stmt = check_stmt current_env expected ast_stmt in
          process_stmts_in_order env_after_this_stmt (sast_stmt :: processed_sast_stmts_acc) rest_ast_stmts
    in
    (* Create a new local scope for the block inheriting from the outer env.
       The 'values' map within this local_block_env will accumulate local declarations. *)
    let local_block_env = { env with values = env.values } in
    process_stmts_in_order local_block_env [] stmts_in_source_order

  and check_stmt env expected = function
    | Expr e ->
      let se = check_expr env e in
      (env, SExpr se)

    | VarDecl {is_const; name; var_type; initializer_expr} ->
      if StringMap.mem name env.values then
        raise (Semantic_error (Printf.sprintf "Variable '%s' already declared in this scope" name));
      let inferred_ty, sinit =
        match (var_type, initializer_expr) with
        | Some te, Some ie ->
          let declared_ty = resolve_type_expr env te in
          let sie = check_expr env ie in
          let actual_ty = fst sie in
          if not (ty_equal declared_ty actual_ty) then
            raise (Semantic_error (Printf.sprintf "Type mismatch for variable '%s': declared as %s but initializer has type %s"
                                      name (string_of_ty declared_ty) (string_of_ty actual_ty)));
          (declared_ty, Some sie)
        | Some te, None -> (resolve_type_expr env te, None)
        | None, Some ie -> let sie = check_expr env ie in (fst sie, Some sie)
        | None, None -> raise (Semantic_error (Printf.sprintf "Cannot infer type for variable '%s' without an initializer or explicit type annotation" name))
      in
      let env' = add_value name (VVar inferred_ty) env in
      (env', SVarDecl {is_const; name; var_type = inferred_ty; initializer_expr = sinit})

    | Block b ->
      let sbody = check_block env expected b in (* Uses new env for block scope, original env for after block *)
      (env, SBlock sbody)

    | IfStmt (cond, then_blk, else_opt) ->
      let scond = check_expr env cond in
      ensure_bool (fst scond) "if condition";
      let sthen = check_block env expected then_blk in
      let selse_opt =
        match else_opt with
        | None -> None
        | Some s -> let _, sst = check_stmt env expected s in Some sst (* else shares env with if *)
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
        | Some st ->
            (* VarDecl in for-loop init should be scoped to the for-loop *)
            let env_for_init, sst = check_stmt env expected st in
            (env_for_init, Some sst)
      in
      let scond_opt = match cond_opt with
        | None -> None
        | Some c -> let sc = check_expr env_after_init c in ensure_bool (fst sc) "for condition"; Some sc
      in
      (* Desugar ++i to i+=1 and --i to i-=1 in the update clause *)
      let desugared_update_opt =
        match update_opt with
        | Some (Unaop(Inc, Identifier var_name)) ->
            Some(CompoundAssign(Identifier var_name, PlusAssign, IntLit 1))
        | Some (Unaop(Dec, Identifier var_name)) ->
            Some (CompoundAssign(Identifier var_name, MinusAssign, IntLit 1))
        | Some (Unaop(Inc, (FieldAccess _ as lval_expr))) | Some (Unaop(Inc, (IndexAccess _ as lval_expr))) ->
            Some (CompoundAssign(lval_expr, PlusAssign, IntLit 1))
        | Some (Unaop(Dec, (FieldAccess _ as lval_expr))) | Some (Unaop(Dec, (IndexAccess _ as lval_expr))) ->
            Some (CompoundAssign(lval_expr, MinusAssign, IntLit 1))
        | other_update_opt -> other_update_opt (* No change *)
      in
      let supd_opt = Option.map (check_expr env_after_init) desugared_update_opt in
      let sbody = check_block env_after_init expected body in
      (env, SFor (sinit_opt, scond_opt, supd_opt, sbody)) (* Original env is returned, init scope is local *)

    | Return rexprs_opt ->
      let srexprs_opt = Option.map (List.map (check_expr env)) rexprs_opt in
      check_return expected srexprs_opt;
      (env, SReturn srexprs_opt)

    | Break -> (env, SBreak)
    | Continue -> (env, SContinue)

  and check_return expected_return_types sexprs_opt =
    match expected_return_types, sexprs_opt with
    | [], None -> () (* void function, no return value is correct *)
    | [], Some actual_sexprs ->
        let num_actuals = List.length actual_sexprs in
        raise (Semantic_error (Printf.sprintf "Return statement in void function has %d value(s), expected none. Types returned: (%s)"
                                  num_actuals (String.concat ", " (List.map (fun (t,_) -> string_of_ty t) actual_sexprs))))
    | etys, None ->
        raise (Semantic_error (Printf.sprintf "Return statement has no value(s), but function expects to return (%s)"
                                  (String.concat ", " (List.map string_of_ty etys))))
    | etys, Some actual_sexprs ->
      if List.length etys <> List.length actual_sexprs then
        raise (Semantic_error (Printf.sprintf "Return statement has %d value(s), but function expects %d value(s) of type (%s). Actual types: (%s)"
                                  (List.length actual_sexprs) (List.length etys)
                                  (String.concat ", " (List.map string_of_ty etys))
                                  (String.concat ", " (List.map (fun (t,_) -> string_of_ty t) actual_sexprs)) ));
      List.iteri (fun i expected_ty ->
          let (actual_ty, _) = List.nth actual_sexprs i in
          if not (ty_equal expected_ty actual_ty) then
            raise (Semantic_error (Printf.sprintf "Return type mismatch for value %d: expected %s, got %s. Full expected return signature: (%s)"
                                      (i+1) (string_of_ty expected_ty) (string_of_ty actual_ty)
                                      (String.concat ", " (List.map string_of_ty etys))))
      ) etys

  (* ---- Top level declarations ----- *)
  let check_field env (f : field) : sfield =
    {
      name = f.name;
      field_type = resolve_type_expr env f.field_type;
      modifier = f.modifier;
      default_value = Option.map (fun expr ->
        let sexpr = check_expr env expr in
        let f_ty = resolve_type_expr env f.field_type in
        if not (ty_equal f_ty (fst sexpr)) then
          raise (Semantic_error (Printf.sprintf "Default value for field '%s' of type %s has incorrect type %s"
            f.name (string_of_ty f_ty) (string_of_ty (fst sexpr)) ));
        sexpr
      ) f.default_value
    }

  let rec collect_structs env = function
    | [] -> env
    | TypeStruct (name, fields) :: tl ->
        if StringMap.mem name env.structs then
          dup name "struct"; (* dup already creates a good message *)
        (* Temporarily add struct name to types for recursive field types,
           but its fields are not yet resolved. This is usually handled by multi-pass.
           Here, assuming fields are checked after all struct names are known (env1 in check_program).
           The 'check_field' uses the env passed to it. *)
        let sfields = List.map (check_field env) fields in
        collect_structs (add_struct name sfields env) tl
    | _ :: tl -> collect_structs env tl

  let extract_func_sig (fd : func_decl) env : func_sig =
    let param_types = List.map (fun (p : param) -> resolve_type_expr env p.param_type) fd.params in
    let ret_types   = List.map (resolve_type_expr env) fd.return_types in
    { params = param_types; returns = ret_types }

  let add_func_header env (fd : func_decl) : env =
    if StringMap.mem fd.name env.values then
      dup fd.name "function";
    let fsig = extract_func_sig fd env in
    add_value fd.name (VFunc fsig) env

  let check_function env (fd : func_decl) : sfunc_decl =
    let fsig = match find_value fd.name env with
        | VFunc s -> s
        | _ -> assert false (* Should have been added as VFunc *)
    in
    let env_with_params =
      List.fold_left2 (fun e (p : param) ty ->
        if StringMap.mem p.name e.values then
            raise (Semantic_error (Printf.sprintf "Function parameter '%s' in function '%s' shadows existing declaration" p.name fd.name));
        add_value p.name (VVar ty) e
      ) env fd.params fsig.params
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
    (mangle md.struct_name md.name, { params = param_types; returns = return_types })

  let add_method_header env (md: struct_func) : env =
    if not (StringMap.mem md.struct_name env.structs) then
      raise (Semantic_error (Printf.sprintf "Cannot define method '%s' for unknown struct '%s'" md.name md.struct_name));
    let mangled, fsig = extract_method_sig env md in
    if StringMap.mem mangled env.values then
      raise (Semantic_error (Printf.sprintf "Duplicate method '%s' for struct '%s' (mangled name '%s')"
                                   md.name md.struct_name mangled));
    add_value mangled (VFunc fsig) env

  let check_struct_method env (md : struct_func) : sstruct_func =
    let mangled = mangle md.struct_name md.name in
    let msig =
      match find_value mangled env with
      | VFunc s -> s
      | _ -> assert false (* Should be VFunc *)
    in
    let env_with_recv_param =
      if StringMap.mem md.receiver_name env.values then
        raise (Semantic_error (Printf.sprintf "Method receiver '%s' in method '%s.%s' shadows existing declaration" md.receiver_name md.struct_name md.name));
      add_value md.receiver_name (VVar (List.hd msig.params)) env
    in
    let env_with_all_params =
      List.fold_left2 (fun e (p : param) ty ->
        if StringMap.mem p.name e.values then
             raise (Semantic_error (Printf.sprintf "Method parameter '%s' in method '%s.%s' shadows existing declaration or receiver" p.name md.struct_name md.name));
        add_value p.name (VVar ty) e
      ) env_with_recv_param md.params (List.tl msig.params)
    in
    let sbody = check_block env_with_all_params msig.returns md.body in
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
    let env0  =
      List.fold_left (fun e td ->
        match td with
        | TypeStruct (n, _) ->
          if StringMap.mem n e.types then dup n "type (struct name)";
          add_type n (TyStruct n) e (* Tentative, fields resolved later *)
        | TypeAlias (n, _) ->
          if StringMap.mem n e.types then dup n "type (alias name)";
          add_type n TyError e (* Placeholder, resolved in pass 1 *)
        )
        initial_env p.type_declarations
    in
    let env1 =
      List.fold_left (fun e td ->
        match td with
        | TypeAlias (name, te) ->
            let resolved_te = resolve_type_expr e te in
            add_type name resolved_te e
        | _ -> e
      ) env0 p.type_declarations
    in
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
    let env3 = List.fold_left add_func_header env2 p.functions in
    let env3b = List.fold_left add_method_header env3 p.struct_functions in
    let env4, sglobals =
      List.fold_left (fun (e_acc, sg_acc) (g : global_decl) ->
        if StringMap.mem g.name e_acc.values then
          dup g.name "global variable";
        let inferred_ty, sinit =
          match g.var_type, g.initializer_expr with
          | Some te, Some ie ->
            let t = resolve_type_expr e_acc te in
            let sie = check_expr e_acc ie in
            if not (ty_equal t (fst sie)) then
              raise (Semantic_error (Printf.sprintf "Global variable '%s' initializer type mismatch: declared as %s, but initializer has type %s"
                                        g.name (string_of_ty t) (string_of_ty (fst sie))));
            (t, Some sie)
          | Some te, None -> (resolve_type_expr e_acc te, None)
          | None, Some ie -> let sie = check_expr e_acc ie in (fst sie, Some sie)
          | None, None -> raise (Semantic_error (Printf.sprintf "Cannot infer type for global variable '%s' without an initializer or explicit type annotation" g.name))
        in
        let e' = add_value g.name (VVar inferred_ty) e_acc in
        let sg = { is_const = g.is_const; name = g.name; var_type = inferred_ty; initializer_expr = sinit } in
        (e', sg_acc @ [sg])
      ) (env3b, []) p.global_vars
    in
    let sfuncs = List.map (check_function env4) p.functions in
    let smethods = List.map (check_struct_method env4) p.struct_functions in

    {
      sp_package = p.package_name;
      sp_imports = p.imports;
      sp_types   = stype_decls;
      sp_globals = sglobals;
      sp_funcs   = sfuncs;
      sp_methods = smethods;
    }
