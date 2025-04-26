(* semantic.ml *)

open Ast

exception SemanticError of string
let error msg = raise (SemanticError msg)

(* ------------------------------------------------------------------ *)
(* S A S T   D E F I N I T I O N S                                    *)
(* ------------------------------------------------------------------ *)

type sexpr = type_expr * sx
and sx =
  | SIntLit      of int
  | SBoolLit     of bool
  | SCharLit     of char
  | SFloatLit    of float
  | SStringLit   of string
  | SNull
  | SId          of string
  | SBinop       of sexpr * biop * sexpr
  | SUnaop       of unop * sexpr
  | SAssignment  of assign_op * sexpr * sexpr
  | SCast        of type_expr * sexpr
  | SArrayLit    of type_expr * sexpr list
  | SStructLit   of type_expr * (string * sexpr) list
  | SSliceLit    of type_expr * sexpr list
  | SFieldAccess of sexpr * string
  | SIndexAccess of sexpr * sexpr
  | SSliceExpr   of sexpr * sexpr * sexpr option
  | SFunctionCall of string * sexpr list
  | SMethodCall  of sexpr * string * sexpr list
  | SContinue
  | SBreak

type sstmt =
  | SExpr      of sexpr
  | SVarDecl   of bool * string * type_expr option * sexpr
  | SIf        of sexpr * sstmt list * sstmt list
  | SFor       of sstmt option * sexpr option * sexpr option * sstmt list
  | SWhile     of sexpr * sstmt list
  | SReturn    of sexpr list

type sparam = {
  sp_name       : string;
  sp_type       : type_expr;
  sp_is_variadic: bool;
}

type sfunc = {
  sf_name         : string;
  sf_params       : sparam list;
  sf_return_types : type_expr list;
  sf_body         : sstmt list;
}

type sstruct_func = {
  ss_struct       : string;
  ss_name         : string;
  ss_params       : sparam list;
  ss_return_types : type_expr list;
  ss_body         : sstmt list;
}

type sprogram = {
  sp_package         : package_decl;
  sp_imports         : import_decl list;
  sp_type_aliases    : (string * type_expr) list;
  sp_structs         : (string * (string * type_expr) list) list;
  sp_globals         : (string * type_expr) list;
  sp_functions       : sfunc list;
  sp_struct_functions: sstruct_func list;
}

(* ------------------------------------------------------------------ *)
(* E N V I R O N M E N T                                              *)
(* ------------------------------------------------------------------ *)

module StringMap = Map.Make(String)

type type_env = {
  aliases : type_expr StringMap.t;
  structs : (string * type_expr) list StringMap.t;
}

type env = {
  types           : type_env;
  globals         : type_expr StringMap.t;
  funcs           : ( (type_expr * bool) list * type_expr list ) StringMap.t;
  methods         : ( (type_expr * bool) list * type_expr list ) StringMap.t;
  locals          : type_expr StringMap.t list;
  current_returns : type_expr list option;
}

let initial_env () = {
  types = { aliases=StringMap.empty; structs=StringMap.empty };
  globals = StringMap.empty;
  funcs = StringMap.empty;
  methods = StringMap.empty;
  locals = [StringMap.empty];
  current_returns = None;
}

let push_scope env =
  { env with locals = StringMap.empty :: env.locals }

let pop_scope env =
  match env.locals with
  | [] | [_] -> error "Internal error: unmatched scope pop"
  | _ :: rest -> { env with locals = rest }

let bind_local env name t =
  let top = List.hd env.locals in
  if StringMap.mem name top then
    error ("Variable " ^ name ^ " already defined in this scope");
  let top' = StringMap.add name t top in
  { env with locals = top' :: List.tl env.locals }

let lookup_var env name =
  let rec aux = function
    | [] -> None
    | sc::rest ->
        (match StringMap.find_opt name sc with
         | Some t -> Some t
         | None -> aux rest)
  in
  match aux env.locals with
  | Some t -> t
  | None ->
      (match StringMap.find_opt name env.globals with
       | Some t -> t
       | None -> error ("Unbound variable " ^ name))

let bind_global env name t =
  if StringMap.mem name env.globals then
    error ("Global variable " ^ name ^ " already defined");
  { env with globals = StringMap.add name t env.globals }

let bind_alias env name te =
  if StringMap.mem name env.types.aliases 
     || StringMap.mem name env.types.structs then
    error ("Type name " ^ name ^ " already defined");
  let aliases' = StringMap.add name te env.types.aliases in
  { env with types = { env.types with aliases = aliases' } }

let bind_struct env name fields =
  if StringMap.mem name env.types.aliases 
     || StringMap.mem name env.types.structs then
    error ("Type name " ^ name ^ " already defined");
  let structs' = StringMap.add name fields env.types.structs in
  { env with types = { env.types with structs = structs' } }

let resolve_type env te =
  let rec aux = function
    | Primitive p -> Primitive p
    | Array(t, n) -> Array(aux t, n)
    | Slice t     -> Slice (aux t)
    | TypeName n  ->
        (match StringMap.find_opt n env.types.aliases with
         | Some te' -> aux te'
         | None ->
             if StringMap.mem n env.types.structs then
               TypeName n
             else
               error ("Unknown type " ^ n))
  in aux te

let rec type_equal env t1 t2 =
  let t1 = resolve_type env t1 in
  let t2 = resolve_type env t2 in
  match t1, t2 with
  | Primitive p1, Primitive p2 -> p1 = p2
  | Array(a1, n1), Array(a2, n2) -> n1 = n2 && type_equal env a1 a2
  | Slice s1, Slice s2 -> type_equal env s1 s2
  | TypeName n1, TypeName n2 -> n1 = n2
  | _ -> false

let bind_function env name signature =
  if StringMap.mem name env.funcs then
    error ("Function " ^ name ^ " already defined");
  { env with funcs = StringMap.add name signature env.funcs }

let lookup_function env name =
  match StringMap.find_opt name env.funcs with
  | Some sig_ -> sig_
  | None -> error ("Undefined function " ^ name)

let lookup_method env struct_name meth =
  let key = struct_name ^ "::" ^ meth in
  match StringMap.find_opt key env.methods with
  | Some sig_ -> sig_
  | None -> error ("Undefined method " ^ meth ^ " on struct " ^ struct_name)

let bind_method env struct_name meth signature =
  let key = struct_name ^ "::" ^ meth in
  if StringMap.mem key env.methods then
    error ("Method " ^ meth ^ " for struct " ^ struct_name ^ " already defined");
  { env with methods = StringMap.add key signature env.methods }

let is_numeric = function
  | Primitive (I8|I16|I32|I64|U8|U16|U32|U64|F32|F64) -> true
  | _ -> false

let is_integer = function
  | Primitive (I8|I16|I32|I64|U8|U16|U32|U64) -> true
  | _ -> false

let is_boolean = function
  | Primitive Bool -> true
  | _ -> false

(* ------------------------------------------------------------------ *)
(* B U I L D   E X P R E S S I O N S                                  *)
(* ------------------------------------------------------------------ *)

let rec build_expr env e : sexpr =
  match e with
  | IntLit i    -> (Primitive I64, SIntLit i)
  | BoolLit b   -> (Primitive Bool,  SBoolLit b)
  | CharLit c   -> (Primitive U8,    SCharLit c)
  | FloatLit f  -> (Primitive F64,   SFloatLit f)
  | StringLit s -> (Primitive String, SStringLit s)
  | Null        -> (Primitive Error,  SNull)

  | ArrayLit (size_e, te, elems) ->
    let (_, ssiz) = build_expr env size_e in
    let size_t = snd (build_expr env size_e) in
    if not (is_integer (fst (build_expr env size_e)))
    then error "Array size must be integer";
    let elem_t = resolve_type env te in
    let sels = List.map (build_expr env) elems in
    List.iter (fun (t, _) ->
      if not (type_equal env t elem_t) then
        error "Array element wrong type"
    ) sels;
    (Array(elem_t, List.length elems), SArrayLit(elem_t, sels))

  | Identifier name ->
      let t = lookup_var env name in
      (t, SId name)

  | SubExpr e ->
      let (t, sx) = build_expr env e in
      (t, sx)

  | Binop (l, op, r) ->
      let (t1, sl) = build_expr env l
      and (t2, sr) = build_expr env r in
      (* semantic checks as before *)
      let res_t = match op with
        | Plus|Minus|Mult|Div|Mod
        | Lshift|Rshift|Bitxor|Bitor|Bitand ->
            if not (is_integer t1 && is_integer t2)
            then error "Arithmetic/bitwise on non-integers";
            t1
        | Eq|Neq ->
            if not (type_equal env t1 t2)
            then error "Equality on mismatched types";
            Primitive Bool
        | Lt|Le|Gt|Ge ->
            if not (is_numeric t1 && is_numeric t2)
            then error "Ordered comparison on non-numeric";
            Primitive Bool
        | And|Or ->
            if not (is_boolean t1 && is_boolean t2)
            then error "Logical op on non-boolean";
            Primitive Bool
      in
      (res_t, SBinop((t1,sl), op, (t2,sr)))


  | Unaop (op, e1) ->
      let (t1, sx1) = build_expr env e1 in

      let () =
        match op with
        | Inc | Dec ->
            ignore (
              match e1 with
              | Identifier name ->
                  let vt = lookup_var env name in
                  if not (is_integer vt) then
                    error "Increment/decrement on non-integer";
                  ()
              | _ ->
                  error "Increment/decrement on non-variable"
            )
        | _ -> ()
      in

      let res_t =
        match op with
        | Neg ->
            if not (is_numeric t1) then
              error "Unary minus on non-numeric";
            t1
        | Not ->
            if not (is_boolean t1) then
              error "Logical not on non-boolean";
            Primitive Bool
        | Bitnot ->
            if not (is_integer t1) then
              error "Bitwise not on non-integer";
            t1
        | Inc | Dec ->
            (* we already validated it above *)
            t1
      in
      (res_t, SUnaop(op, (t1, sx1)))


  | Assignment (lhs, op, rhs) ->
      let (t1, sl) = build_expr env lhs
      and (t2, sr) = build_expr env rhs in
      if not (type_equal env t1 t2) then
        error "Assignment type mismatch";
      (t1, SAssignment(op, (t1,sl), (t2,sr)))

  | Cast (te, e1) -> (
      let target = resolve_type env te in
      let (src_t, sx1) = build_expr env e1 in
      match target, src_t with
      | Primitive _, Primitive _ ->
          (* SCast wants a sexpr = (type_expr * sx) *)
          (target, SCast(target, (src_t, sx1)))
      | _ ->
          error "Invalid cast";
    )

  | StructLit (te_expr, fields) ->
      (* te_expr is an expr (Identifier name) naming the struct’s type *)
      let struct_name =
        match te_expr with
        | Identifier s -> s
        | _ -> error "Struct literal must begin with an identifier"
      in
      let t = TypeName struct_name in
      (* look up declared fields for that struct *)
      let fdefs =
        StringMap.find_opt struct_name env.types.structs
        |> Option.value ~default:(error ("Unknown struct: " ^ struct_name))
      in
      let fmap =
        List.fold_left (fun m (fn, ty) -> StringMap.add fn ty m)
                       StringMap.empty
                       fdefs
      in
      (* now build each supplied field, extracting its name and value *)
      let spairs =
        List.map (fun (fkey_expr, fval_expr) ->
          let fkey =
            match fkey_expr with
            | Identifier s -> s
            | _ -> error "Struct field name must be identifier"
          in
          let expected_ty =
            Option.value ~default:(error ("No such field "^fkey)) 
                         (StringMap.find_opt fkey fmap)
          in
          let (actual_ty, sval) = build_expr env fval_expr in
          if not (type_equal env actual_ty expected_ty) then
            error ("Field "^fkey^" has wrong type in struct "^struct_name);
          (fkey, (actual_ty, sval))
        ) fields
      in
      (t, SStructLit(t, spairs))


  | SliceLit (te, elems) ->
      let elem_t = resolve_type env te in
      let sels = List.map (build_expr env) elems in
      List.iter (fun (t,_) ->
        if not (type_equal env t elem_t) then
          error "Slice element wrong type"
      ) sels;
      (Slice elem_t, SSliceLit(elem_t, sels))

  | FieldAccess (e1, Identifier fn) ->
      let (rt, srecv) = build_expr env e1 in
      (match rt with
       | TypeName name ->
           let fdefs = StringMap.find_opt name env.types.structs
                      |> Option.value ~default:(error ("Unknown struct " ^ name)) in
           let fmap = List.fold_left (fun m (n,ty) -> StringMap.add n ty m)
                                     StringMap.empty fdefs in
           let fty =
             Option.value ~default:(error ("Field "^fn^" not in struct "^name))
                          (StringMap.find_opt fn fmap)
           in
           (fty, SFieldAccess((rt,srecv), fn))
       | _ -> error "Field access on non-struct")

  | IndexAccess (arr, idx) ->
      let (tat, sarr) = build_expr env arr
      and (tit, sidx) = build_expr env idx in
      if not (is_integer tit) then error "Index non-integer";
      (match tat with
       | Array(et,_) -> (et, SIndexAccess((tat,sarr),(tit,sidx)))
       | Slice et    -> (et, SIndexAccess((tat,sarr),(tit,sidx)))
       | _ -> error "Index on non-array/slice")

  | SliceExpr (arr, st, end_opt) ->
      let (tat, sarr) = build_expr env arr
      and (tst, sst) = build_expr env st in
      if not (is_integer tst) then error "Slice start non-integer";
      let send_opt = Option.map (fun e ->
        let (tet, set') = build_expr env e in
        if not (is_integer tet) then error "Slice end non-integer";
        (tet, set')
      ) end_opt in
      (match tat with
        | Array (et, _) | Slice et ->
         ( Slice et
         , SSliceExpr(
             (tat, sarr)
           , (tst, sst)
           , send_opt
           )
         )
       | _ -> error "Slice on non-array/slice")

  | FunctionCall (nm, args) ->
      let (params, rets) = lookup_function env nm in
      let arity = List.length params in
      let has_var = List.exists snd params in
      let minp = if has_var then arity-1 else arity in
      if List.length args < minp
         || (not has_var && List.length args > arity)
      then error ("Function "^nm^" wrong arity");
      let sargs = List.map (build_expr env) args in
      List.iteri (fun i (at,_) ->
        let (pt,_) = List.nth params (min i (arity-1)) in
        if not (type_equal env at pt) then
          error ("Function "^nm^" arg " ^ string_of_int (i+1)^" wrong type")
      ) sargs;
      (match rets with
       | [t] -> (t, SFunctionCall(nm, sargs))
       | []  -> (Primitive Error, SFunctionCall(nm, sargs))
       | _   -> error ("Function "^nm^" returns multiple values"))

  | MethodCall (recv, Identifier mn, args) ->
      let (rt, srecv) = build_expr env recv in
      (match rt with
       | TypeName stnm ->
           let (params, rets) = lookup_method env stnm mn in
           let arity = List.length params in
           let has_var = List.exists snd params in
           let minp = if has_var then arity-1 else arity in
           if List.length args < minp
              || (not has_var && List.length args > arity)
           then error ("Method "^mn^" wrong arity");
           let sargs = List.map (build_expr env) args in
           List.iteri (fun i (at,_) ->
             let (pt,_) = List.nth params (min i (arity-1)) in
             if not (type_equal env at pt) then
               error ("Method "^mn^" arg " ^ string_of_int (i+1)^" wrong type")
           ) sargs;
           (match rets with
            | [t] -> (t, SMethodCall((rt,srecv), mn, sargs))
            | []  -> (Primitive Error, SMethodCall((rt,srecv), mn, sargs))
            | _   -> error ("Method "^mn^" returns multiple values"))
       | _ -> error "Method call on non-struct")

(* ------------------------------------------------------------------ *)
(* B U I L D   S T A T E M E N T S                                    *)
(* ------------------------------------------------------------------ *)

let rec build_stmt env stmt : env * sstmt =
  match stmt with
  | Expr e ->
      let se = build_expr env e in
      (env, SExpr se)

  | VarDecl vd ->
      (match vd with
       | StrictType { is_const; name; var_type; initializer_expr } ->
           let t = resolve_type env var_type in
            let se : sexpr =
              match initializer_expr with
              | Some e ->
                  (* keep the full sexpr so both branches are the same type *)
                  let (t', sx) as sexpr = build_expr env e in
                  if not (type_equal env t t') then
                    error ("Initializer for "^name^" wrong type");
                  sexpr
              | None ->
                  (* the default value is the null‐literal of type t *)
                  (t, SNull)
            in
           let env' = bind_local env name t in
           (env', SVarDecl(is_const, name, Some t, se))

       | InferType { is_const; name; initializer_expr } ->
       (* build_expr returns a full sexpr = (t, sx) *)
       let (t, sx) as sexpr = build_expr env initializer_expr in
       let env' = bind_local env name t in
       (* pass the entire sexpr, not just sx *)
       (env', SVarDecl(is_const, name, None, sexpr))
      )

  | IfStmt (cond, then_b, else_b) ->
      let (tcond, scond) = build_expr env cond in
      if not (is_boolean tcond) then error "If cond non-boolean";
      let env1 = push_scope env in
      let then_s = List.map (fun stmt -> snd (build_stmt env1 stmt)) then_b in
      let env2 = push_scope env in
      let else_s = List.map (fun stmt -> snd (build_stmt env2 stmt)) else_b in
      (env, SIf((tcond,scond), then_s, else_s))

  | ForStmt (init_o, cond_o, upd_o, body) ->
      let env1 = push_scope env in
      let (env2, sinit) = match init_o with
        | Some (VarDecl vd) -> build_stmt env1 stmt
        | Some (Expr e)     -> let se = build_expr env1 e in (env1, SExpr se)
        | None -> (env1, SExpr(Primitive Error, SNull))
      in
      let scond = Option.map (build_expr env2) cond_o in
      let supd  = Option.map (build_expr env2) upd_o in
      let sbody = List.map (fun stmt -> snd (build_stmt env2 stmt)) body in
      (env, SFor(Some sinit, scond, supd, sbody))

  | WhileStmt (cond, body) ->
      let (tcond, scond) = build_expr env cond in
      if not (is_boolean tcond) then error "While cond non-boolean";
      let sbody = List.map (fun stmt -> snd (build_stmt (push_scope env) stmt)) body in
      (env, SWhile((tcond,scond), sbody))

  | Return exprs ->
      (match env.current_returns with
       | None -> error "Return outside function"
       | Some exps ->
           if List.length exprs <> List.length exps then
             error "Return wrong arity";
           let sexprs = List.map (build_expr env) exprs in
           List.iter2 (fun (t,_) rt ->
             if not (type_equal env t rt) then
               error "Return wrong type"
           ) sexprs exps;
           (env, SReturn sexprs)
      )
(* ------------------------------------------------------------------ *)
(* T O P – L E V E L   C H E C K                                      *)
(* ------------------------------------------------------------------ *)

let analyze (prog : program) : sprogram =
  (* Unpack AST fields so we don’t confuse functions vs. struct_methods *)
  let { package_name
      ; imports
      ; type_declarations
      ; global_vars
      ; functions        = func_decls
      ; struct_functions = method_decls
      } = prog
  in

  (* 0. Start with an empty environment *)
  let env0 = initial_env () in

  (* 1. Process type aliases and struct declarations *)
  let (env1, aliases, structs) =
    List.fold_left (fun (env, aacc, sacc) td ->
      match td with
      | TypeAlias (nm, te) ->
          let te'  = resolve_type env te in
          let env' = bind_alias env nm te' in
          (env', (nm, te') :: aacc, sacc)

      | TypeStruct (nm, fields) ->
          let fdefs =
            List.map (fun f ->
              let ft = resolve_type env f.field_type in
              (f.name, ft)
            ) fields
          in
          let env' = bind_struct env nm fdefs in
          (env', aacc, (nm, fdefs) :: sacc)
    ) (env0, [], []) type_declarations
  in

  (* 2. Process global variable declarations *)
  let (env2, globals) =
    List.fold_left (fun (env, gacc) vd ->
      match vd with
      | StrictType { name; var_type; initializer_expr; _ } ->
          let t = resolve_type env var_type in
          (match initializer_expr with
           | Some e ->
               let (t', _) = build_expr env e in
               if not (type_equal env t t') then
                 error ("Global init " ^ name ^ " wrong type")
           | None -> ());
          let env' = bind_global env name t in
          (env', (name, t) :: gacc)

      | InferType { name; initializer_expr; _ } ->
          let (t, _) = build_expr env initializer_expr in
          let env' = bind_global env name t in
          (env', (name, t) :: gacc)
    ) (env1, []) global_vars
  in

  (* 3. Process free functions *)
  let (env3, sfuncs) =
    List.fold_left (fun (env, facc) (fdecl : func_decl) ->
      (* Register signature *)
      let param_sigs = List.map (fun p ->
        let tp = resolve_type env p.param_type in
        (tp, p.is_variadic)
      ) fdecl.params in
      let ret_ts = List.map (resolve_type env) fdecl.return_types in
      let env' = bind_function env fdecl.name (param_sigs, ret_ts) in

      (* New scope with return types known *)
      let env'' = { env' with current_returns = Some ret_ts } in
      let env_par = push_scope env'' in

      (* Bind parameters locally *)
      let sparams = List.map (fun p ->
        let tp = resolve_type env p.param_type in
        let env_par = bind_local env_par p.name tp in
        { sp_name = p.name; sp_type = tp; sp_is_variadic = p.is_variadic }
      ) fdecl.params in

      (* Build body statements *)
      let body_stmts = List.map (fun s -> snd (build_stmt env_par s)) fdecl.body in

      let sf = {
        sf_name         = fdecl.name;
        sf_params       = sparams;
        sf_return_types = ret_ts;
        sf_body         = body_stmts;
      } in

      (env', sf :: facc)
    ) (env2, []) func_decls
  in

  (* 4. Process struct methods *)
  let (env4, sstructs) =
    List.fold_left (fun (env, macc) (mdecl : struct_func) ->
      (* Ensure struct exists *)
      if not (StringMap.mem mdecl.struct_name env.types.structs) then
        error ("Method for unknown struct " ^ mdecl.struct_name);

      (* Synthetic receiver signature *)
      let recv_sig = (TypeName mdecl.struct_name, false) in

      (* Parameter signatures include receiver *)
      let param_sigs = recv_sig ::
        List.map (fun p ->
          let tp = resolve_type env p.param_type in
          (tp, p.is_variadic)
        ) mdecl.params
      in

      let ret_ts = List.map (resolve_type env) mdecl.return_types in

      (* Register method *)
      let env' = bind_method env mdecl.struct_name mdecl.name (param_sigs, ret_ts) in

      (* New scope with return types known *)
      let env'' = { env' with current_returns = Some ret_ts } in
      let env_par = push_scope env'' in

      (* Bind real parameters (skip synthetic recv) *)
      let sparams = List.map (fun p ->
        let tp = resolve_type env p.param_type in
        let env_par = bind_local env_par p.name tp in
        { sp_name = p.name; sp_type = tp; sp_is_variadic = p.is_variadic }
      ) mdecl.params in

      (* Build body statements *)
      let body_stmts = List.map (fun s -> snd (build_stmt env_par s)) mdecl.body in

      let sm = {
        ss_struct       = mdecl.struct_name;
        ss_name         = mdecl.name;
        ss_params       = sparams;
        ss_return_types = ret_ts;
        ss_body         = body_stmts;
      } in

      (env', sm :: macc)
    ) (env3, []) method_decls
  in

  (* 5. Ensure entry point ‘main’ exists *)
  if not (StringMap.mem "main" env4.funcs) then
    error "Missing entry point: main";

  (* 6. Assemble final SAST program *)
  {
    sp_package          = package_name;
    sp_imports          = imports;
    sp_type_aliases     = List.rev aliases;
    sp_structs          = List.rev structs;
    sp_globals          = List.rev globals;
    sp_functions        = List.rev sfuncs;
    sp_struct_functions = List.rev sstructs;
  }


