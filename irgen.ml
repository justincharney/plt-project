(* irgen.ml *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (sprogram : sprogram) =
  Printf.eprintf "IRGen: Starting translation for package %s\n" sprogram.sp_package; flush stderr; (* DEBUG *)
  let context    = L.global_context () in
  let the_module = L.create_module context sprogram.sp_package in

  (* Map SAST types to LLVM types *)
  let ll_struct_types : L.lltype StringMap.t ref = ref StringMap.empty in
  let ll_alias_types : L.lltype StringMap.t ref = ref StringMap.empty in

  (* String type: { data: i8*, length: i64 } *)
  let string_ll_type =
    let ty = L.named_struct_type context "String" in
    L.struct_set_body ty [| L.pointer_type (L.i8_type context); L.i64_type context |] false;
    ty
  in

  (* Return the LLVM type for a P.A.T type *)
  let rec ltype_of_sast_ty (t : ty) : L.lltype =
    match t with
    | TyPrim p ->
      (
        match p with
          | I8 | U8     -> L.i8_type      context
          | I16 | U16   -> L.i16_type     context
          | I32 | U32   -> L.i32_type     context
          | I64 | U64   -> L.i64_type     context
          | F32         -> L.float_type   context
          | F64         -> L.double_type  context
          | Bool        -> L.i1_type      context
          | String      -> string_ll_type
          | Error       -> failwith "TyError should not reach IR gen"
      )
    | TyArray(elt_t, n) -> L.array_type (ltype_of_sast_ty elt_t) n
    (* | SSlice t ->
        let ptr = L.pointer_type (ltype_of_typ t) in
        L.struct_type context [| ptr; L.i64_type context |] *)
    | TyStruct name -> (
      try StringMap.find name !ll_struct_types
        with Not_found ->
          try StringMap.find name !ll_alias_types
          with Not_found -> failwith ("Unknown struct type in ltype_of_sast_ty: " ^ name)
    )
    | TyTuple []        -> L.void_type context (* Should not happen if TyUnit is used *)
    | TyTuple [ty_elt]  -> ltype_of_sast_ty ty_elt (* Single element tuple is just the element type *)
    | TyTuple ts        -> L.struct_type context (Array.of_list (List.map ltype_of_sast_ty ts))
    | TyNull            -> L.pointer_type (L.i8_type context) (* Generic void pointer for null *)
    | TyUnit            -> L.void_type context
    | TyError           -> failwith "TyError should not reach IR generation (ltype_of_sast_ty)"
  in

  (* Pre-declare all struct types from sprogram.sp_types *)
  let process_type_declarations () =
    let struct_defs = List.filter_map (function STypeStruct(n,f) -> Some (n,f) | _ -> None) sprogram.sp_types in
    let alias_defs = List.filter_map (function STypeAlias(n,t) -> Some (n,t) | _ -> None) sprogram.sp_types in

    (* First pass: create named struct types (empty bodies for now if recursive) *)
    List.iter (fun (name, _) ->
      let ll_ty = L.named_struct_type context name in
      ll_struct_types := StringMap.add name ll_ty !ll_struct_types
    ) struct_defs;

    (* Second pass: define struct bodies *)
    List.iter (fun (name, sfields) ->
      let ll_ty = StringMap.find name !ll_struct_types in
      let field_ll_types = List.map (fun f -> ltype_of_sast_ty f.field_type) sfields in
      L.struct_set_body ll_ty (Array.of_list field_ll_types) false
    ) struct_defs;

    (* Handle aliases *)
    List.iter (fun (name, sast_ty) ->
        let ll_ty = ltype_of_sast_ty sast_ty in
        ll_alias_types := StringMap.add name ll_ty !ll_alias_types
    ) alias_defs
  in
  Printf.eprintf "IRGen: Processing type declarations...\n"; flush stderr; (* DEBUG *)
  process_type_declarations ();
  Printf.eprintf "IRGen: Finished type declarations.\n"; flush stderr; (* DEBUG *)

  (* Map for global variables *)
  let global_vars_map : L.llvalue StringMap.t ref = ref StringMap.empty in
  let rec build_global_initializer (var_ty: ty) (init_expr_opt: sexpr option) (builder_dummy : L.llbuilder) : L.llvalue =
    match init_expr_opt with
    | None -> L.const_null (ltype_of_sast_ty var_ty)
    | Some (sexpr_ty, sx) -> (* sexpr_ty should match var_ty or be compatible *)
        (match sx with
        | SIntLit i ->
            (match sexpr_ty with (* Use sexpr_ty to determine bitwidth *)
                | TyPrim (A.I8 | A.U8)   -> L.const_int (L.i8_type context) i
                | TyPrim (A.I16 | A.U16) -> L.const_int (L.i16_type context) i
                | TyPrim (A.I32 | A.U32) -> L.const_int (L.i32_type context) i
                | TyPrim (A.I64 | A.U64) -> L.const_int (L.i64_type context) i
                | _ -> L.const_int (L.i32_type context) i (* Default for safety, but should be typed *)
            )
        | SBoolLit b  -> L.const_int (L.i1_type context) (if b then 1 else 0)
        | SFloatLit f ->
            (match sexpr_ty with
                | TyPrim A.F32 -> L.const_float (L.float_type context) f
                | TyPrim A.F64 -> L.const_float (L.double_type context) f
                | _ -> L.const_float (L.double_type context) f (* Default *)
            )
        | SStringLit s ->
            let global_str_val = L.define_global ".str_const" (L.const_stringz context s) the_module in
            L.set_linkage L.Linkage.Private global_str_val;
            let str_ptr = L.const_in_bounds_gep global_str_val [| L.const_int (L.i32_type context) 0; L.const_int (L.i32_type context) 0 |] in
            let len = L.const_int (L.i64_type context) (String.length s) in
            L.const_struct context [| str_ptr; len |]
        | SNull -> L.const_null (ltype_of_sast_ty sexpr_ty)
        | SArrayLit (elem_ty, selms) ->
            let ll_elem_ty = ltype_of_sast_ty elem_ty in
            let const_elems = List.map (fun se -> build_global_initializer elem_ty (Some se) builder_dummy) selms in
            L.const_array ll_elem_ty (Array.of_list const_elems)
        | SStructLit (struct_name, field_inits) ->
            let _ = ltype_of_sast_ty (TyStruct struct_name) in (* ll_s_ty not used *)
            (* Ensure field_inits are in the correct order as per struct definition *)
            let s_info = List.find (function STypeStruct(n,_) when n = struct_name -> true | _ -> false) sprogram.sp_types in
            let ordered_sfields = match s_info with STypeStruct(_,fs) -> fs | _ -> failwith "Struct not found for const init" in

            let const_fields_map = List.fold_left (fun acc (fname, se) -> StringMap.add fname (build_global_initializer (fst se) (Some se) builder_dummy) acc) StringMap.empty field_inits in
            let const_fields = List.map (fun (sf:sfield) ->
                try StringMap.find sf.name const_fields_map
                with Not_found ->
                    match sf.default_value with
                    | Some def_se -> build_global_initializer sf.field_type (Some def_se) builder_dummy
                    | None -> L.const_null (ltype_of_sast_ty sf.field_type) (* Or error if no default and not provided *)
            ) ordered_sfields in
            L.const_struct context (Array.of_list const_fields)

        | _ -> L.const_null (ltype_of_sast_ty var_ty) (* Placeholder for non-constant initializers - should be caught by semant or require global ctors *)
        )
  in

  Printf.eprintf "IRGen: Processing global variables...\n"; flush stderr; (* DEBUG *)
  List.iter (fun (gdecl : sglobal_decl) ->
    let dummy_builder = L.builder context in
    let init_val = build_global_initializer gdecl.var_type gdecl.initializer_expr dummy_builder in
    let global_val = L.define_global gdecl.name init_val the_module in
    global_vars_map := StringMap.add gdecl.name global_val !global_vars_map
  ) sprogram.sp_globals;
  Printf.eprintf "IRGen: Finished global variables.\n"; flush stderr; (* DEBUG *)

  (* --- Function Declarations (forward declarations) --- *)
  let function_decls : L.llvalue StringMap.t ref = ref StringMap.empty in

  let declare_function name params_sast_types return_sast_types =
    let param_ll_types = Array.of_list (List.map ltype_of_sast_ty params_sast_types) in
    let return_ll_type =
      match return_sast_types with
      | [] -> L.void_type context
      | [t] -> ltype_of_sast_ty t
      | ts -> ltype_of_sast_ty (TyTuple ts) (* Multiple returns*)
    in
    let func_ll_type = L.function_type return_ll_type param_ll_types in
    let func_ll_val = L.declare_function name func_ll_type the_module in
    function_decls := StringMap.add name func_ll_val !function_decls
  in

  (* Declare regular functions *)
  Printf.eprintf "IRGen: Declaring regular functions...\n"; flush stderr; (* DEBUG *)
  List.iter (fun (fdecl : sfunc_decl) ->
    let param_types = List.map (fun p -> p.param_type) fdecl.params in
    declare_function fdecl.name param_types fdecl.return_types
  ) sprogram.sp_funcs;
  Printf.eprintf "IRGen: Finished declaring regular functions.\n"; flush stderr; (* DEBUG *)

  (* Declare struct methods (mangled names) *)
  if sprogram.sp_methods <> [] then Printf.eprintf "IRGen: Declaring struct methods...\n"; flush stderr; (* DEBUG *)
  let mangle_method struct_name method_name = struct_name ^ "$" ^ method_name in
  List.iter (fun (mdecl : sstruct_func) ->
    let receiver_sast_ty = TyStruct mdecl.struct_name in
    let param_sast_types = receiver_sast_ty :: (List.map (fun p -> p.param_type) mdecl.params) in
    let mangled_name = mangle_method mdecl.struct_name mdecl.name in
    declare_function mangled_name param_sast_types mdecl.return_types
  ) sprogram.sp_methods;
  if sprogram.sp_methods <> [] then Printf.eprintf "IRGen: Finished declaring struct methods.\n"; flush stderr; (* DEBUG *)

  (* --- Built-in functions --- *)
  let printf_t    : L.lltype  = L.var_arg_function_type (L.i32_type context) [| L.pointer_type (L.i8_type context) |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in
  let exit_t      = L.function_type (L.void_type context) [| L.i32_type context |] in
  let exit_func   = L.declare_function "exit" exit_t the_module in
  (* let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in *)

  (* --- Function Body Generation ---  *)
  let current_func_return_type : L.lltype option ref = ref None in
  let loop_env : (L.llbasicblock * L.llbasicblock) list ref = ref [] in (* For break/continue: (continue_bb, break_bb) *)

  (* Recursive helper function to translate Sast.sexpr to Llvm.llvalue.  *)
  let rec build_expr builder (local_vars : L.llvalue StringMap.t) (current_func_llval: L.llvalue) ((sexpr_ty, sx) : sexpr) : L.llvalue =
    let rec build_lvalue_address sx_for_lval : L.llvalue =
        match sx_for_lval with
        | SIdentifier id -> (
            try StringMap.find id local_vars
            with Not_found ->
              try StringMap.find id !global_vars_map
              with Not_found -> failwith ("IRGen: Undeclared identifier (lvalue): " ^ id)
          )
        | SFieldAccess (struct_sexpr, field_name) ->
            let struct_ptr = build_lvalue_address (snd struct_sexpr) in (* Get address of struct *)
            let struct_sast_ty = fst struct_sexpr in
            let field_idx =
              match struct_sast_ty with
              | TyStruct s_name ->
                  let s_info = List.find (function STypeStruct(n,_) when n = s_name -> true | _ -> false) sprogram.sp_types in
                  let sfields = match s_info with STypeStruct(_,fs) -> fs | _ -> failwith ("Struct def not found for field access: " ^ s_name) in
                  (try
                    let (_field, index) = List.find (fun ((f:sfield), _) -> f.name = field_name) (List.mapi (fun i fld -> (fld, i)) sfields) in
                    index
                  with Not_found -> failwith ("Field " ^ field_name ^ " not found in struct " ^ s_name))
              | TyPrim _ | TyArray (_, _) | TyTuple _ | TyNull | TyUnit | TyError ->
                  failwith "Field access on non-struct type (lvalue) - Should have been caught by Semant"
            in
            L.build_struct_gep struct_ptr field_idx ("field_" ^ field_name ^ "_ptr") builder
        | SIndexAccess (coll_sexpr, idx_sexpr) ->
            let coll_ptr = build_lvalue_address (snd coll_sexpr) in (* Get address of collection (array) *)
            let idx_val = build_expr builder local_vars current_func_llval idx_sexpr in
            (match fst coll_sexpr with
            | TyArray _ -> L.build_gep coll_ptr [| L.const_int (L.i32_type context) 0; idx_val |] "elem_ptr" builder
            | TyTuple _ -> (* get element pointer on struct for tuple element *)
                (match snd idx_sexpr with
                  | SIntLit i -> L.build_struct_gep coll_ptr i "tuple_elem_ptr" builder
                  | _ -> failwith "Tuple index must be a constant integer literal for GEP"
                )
            | _ -> failwith "Index access on non-array/tuple type (lvalue)")
        | _ -> failwith "Expression is not a valid l-value"
    in

    match sx with
    | SIntLit i -> L.const_int (ltype_of_sast_ty sexpr_ty) i
    | SBoolLit b -> L.const_int (L.i1_type context) (if b then 1 else 0)
    | SCharLit c -> L.const_int (L.i8_type context) (Char.code c)
    | SFloatLit f -> L.const_float (ltype_of_sast_ty sexpr_ty) f
    | SStringLit s ->
        (* Create a global for the string data part *)
        let str_data_global = L.build_global_stringptr s ".str" builder in
        (* Allocate stack space for the String struct {i8*, i64} *)
        let string_struct_alloca = L.build_alloca string_ll_type "str_lit_obj" builder in
        (* Store data pointer *)
        let data_ptr_field = L.build_struct_gep string_struct_alloca 0 "str_data_ptr" builder in
        ignore(L.build_store str_data_global data_ptr_field builder);
        (* Store length *)
        let len_val = L.const_int (L.i64_type context) (String.length s) in
        let len_ptr_field = L.build_struct_gep string_struct_alloca 1 "str_len_ptr" builder in
        ignore(L.build_store len_val len_ptr_field builder);
        L.build_load string_struct_alloca "loaded_str_lit_obj" builder (* Return the struct by value *)

    | SNull -> L.const_null (ltype_of_sast_ty sexpr_ty)

    | SArrayLit (_ , selms) -> (* _elem_sast_ty not directly used here, sexpr_ty is the array type *)
      let array_ll_ty = ltype_of_sast_ty sexpr_ty in (* sexpr_ty is TyArray *)
      let array_alloca = L.build_alloca array_ll_ty "arr_lit_obj" builder in
      List.iteri (fun i selem ->
          let elem_val = build_expr builder local_vars current_func_llval selem in
          let elem_ptr = L.build_gep array_alloca [| L.const_int (L.i32_type context) 0; L.const_int (L.i32_type context) i |] ("elem_" ^ string_of_int i) builder in
          ignore(L.build_store elem_val elem_ptr builder)
      ) selms;
      L.build_load array_alloca "loaded_arr_lit_obj" builder

    | SStructLit (struct_name, field_inits_sast) ->
        let struct_ll_ty = ltype_of_sast_ty sexpr_ty in (* sexpr_ty is TyStruct *)
        let struct_alloca = L.build_alloca struct_ll_ty "struct_lit_obj" builder in

        let s_info = List.find (function STypeStruct(n,_) when n = struct_name -> true | _ -> false) sprogram.sp_types in
        let ordered_sfields = match s_info with STypeStruct(_,fs) -> fs | _ -> failwith ("Struct not found for literal: " ^ struct_name) in

        let field_vals_map = List.fold_left (fun acc (fname, se_field) ->
            StringMap.add fname (build_expr builder local_vars current_func_llval se_field) acc
        ) StringMap.empty field_inits_sast in

        List.iteri (fun i (sf:sfield) ->
            let field_val =
                try StringMap.find sf.name field_vals_map
                with Not_found ->
                    match sf.default_value with
                    | Some def_se -> build_expr builder local_vars current_func_llval def_se
                    | None -> failwith ("IRGen: No value provided for field " ^ sf.name ^ " in struct " ^ struct_name ^ " and no default value.")
            in
            let field_ptr = L.build_struct_gep struct_alloca i ("field_" ^ sf.name) builder in
            ignore(L.build_store field_val field_ptr builder)
        ) ordered_sfields;
        L.build_load struct_alloca "loaded_struct_lit_obj" builder

    | SIdentifier id ->
        let addr = try StringMap.find id local_vars
                    with Not_found ->
                      try StringMap.find id !global_vars_map
                      with Not_found -> failwith ("IRGen: Undeclared identifier (rvalue): " ^ id)
        in L.build_load addr id builder

    | SFieldAccess (struct_sexpr, field_name) ->
        let struct_addr = build_lvalue_address (snd struct_sexpr) in (* Get address of struct *)
        L.build_load struct_addr field_name builder (* Load the field value *)

    | SIndexAccess (coll_sexpr, _) ->
        let coll_addr = build_lvalue_address (snd coll_sexpr) in (* Get address of collection element *)
        L.build_load coll_addr "indexed_val" builder

    | SBinop (se1, op, se2) ->
        let v1 = build_expr builder local_vars current_func_llval se1 in
        let v2 = build_expr builder local_vars current_func_llval se2 in
        let (t1, _) = se1 in
        (match op with
          | A.Plus when Semant.is_integer t1  -> L.build_add v1 v2 "addtmp" builder
          | A.Plus (* Floats *)             -> L.build_fadd v1 v2 "faddtmp" builder
          | A.Minus when Semant.is_integer t1 -> L.build_sub v1 v2 "subtmp" builder
          | A.Minus (* Floats *)            -> L.build_fsub v1 v2 "fsubtmp" builder
          | A.Mult when Semant.is_integer t1  -> L.build_mul v1 v2 "multmp" builder
          | A.Mult (* Floats *)             -> L.build_fmul v1 v2 "fmultmp" builder
          | A.Div when Semant.is_integer t1   -> L.build_sdiv v1 v2 "divtmp" builder (* Assuming signed division for I types, U types would need udiv *)
          | A.Div (* Floats *)              -> L.build_fdiv v1 v2 "fdivtmp" builder
          | A.Mod                           -> L.build_srem v1 v2 "modtmp" builder (* Signed remainder *)
          | A.Lshift                        -> L.build_shl v1 v2 "lshifttmp" builder
          | A.Rshift                        -> L.build_ashr v1 v2 "rshifttmp" builder (* Arithmetic shift for signed, logical for unsigned *)
          | A.Bitxor                        -> L.build_xor v1 v2 "bitxortmp" builder
          | A.Bitor                         -> L.build_or v1 v2 "bitortmp" builder
          | A.Bitand                        -> L.build_and v1 v2 "bitandtmp" builder
          | A.Eq                            -> if Semant.is_numeric t1 && not (Semant.is_integer t1) then L.build_fcmp L.Fcmp.Oeq v1 v2 "feqtmp" builder else L.build_icmp L.Icmp.Eq v1 v2 "eqtmp" builder
          | A.Neq                           -> if Semant.is_numeric t1 && not (Semant.is_integer t1) then L.build_fcmp L.Fcmp.One v1 v2 "fneqtmp" builder else L.build_icmp L.Icmp.Ne v1 v2 "neqtmp" builder
          | A.Lt                            -> if Semant.is_numeric t1 && not (Semant.is_integer t1) then L.build_fcmp L.Fcmp.Olt v1 v2 "flttmp" builder else L.build_icmp L.Icmp.Slt v1 v2 "lttmp" builder
          | A.Le                            -> if Semant.is_numeric t1 && not (Semant.is_integer t1) then L.build_fcmp L.Fcmp.Ole v1 v2 "fletmp" builder else L.build_icmp L.Icmp.Sle v1 v2 "letmp" builder
          | A.Gt                            -> if Semant.is_numeric t1 && not (Semant.is_integer t1) then L.build_fcmp L.Fcmp.Ogt v1 v2 "fgttmp" builder else L.build_icmp L.Icmp.Sgt v1 v2 "gttmp" builder
          | A.Ge                            -> if Semant.is_numeric t1 && not (Semant.is_integer t1) then L.build_fcmp L.Fcmp.Oge v1 v2 "fgetmp" builder else L.build_icmp L.Icmp.Sge v1 v2 "getmp" builder
          | A.And                           -> L.build_and v1 v2 "andtmp" builder
          | A.Or                            -> L.build_or v1 v2 "ortmp" builder
        )
    | SUnaop (op, se) ->
        let v = build_expr builder local_vars current_func_llval se in
        (match op with
          | A.Neg    -> if Semant.is_integer (fst se) then L.build_neg v "negtmp" builder else L.build_fneg v "fnegtmp" builder
          | A.Not    -> L.build_not v "nottmp" builder (* LLVM 'not' is bitwise; for bool, xor with true (1) *)
          | A.Bitnot -> L.build_not v "bitnottmp" builder
          | A.Inc | A.Dec -> failwith "Inc/Dec should be handled by assignments or desugared earlier"
        )
    | SSimpleAssign (lhs_sx_node, rhs_se) ->
        let rhs_val = build_expr builder local_vars current_func_llval rhs_se in
        let lhs_addr = build_lvalue_address (snd lhs_sx_node) in
        ignore(L.build_store rhs_val lhs_addr builder);
        rhs_val
    | SCompoundAssign (lhs_sx_node, op, rhs_se) ->
        let lhs_addr = build_lvalue_address (snd lhs_sx_node) in
        let old_lhs_val = L.build_load lhs_addr "old_lhs_val" builder in
        let rhs_val = build_expr builder local_vars current_func_llval rhs_se in
        let (lhs_ty, _) = lhs_sx_node in
        let ast_binop = (* Map Ast.compound_op to Ast.biop *)
            match op with
            | A.PlusAssign -> A.Plus | A.MinusAssign -> A.Minus | A.TimesAssign -> A.Mult | A.DivAssign -> A.Div
            | A.ModAssign -> A.Mod | A.LshiftAssign -> A.Lshift | A.RshiftAssign -> A.Rshift
            | A.BitandAssign -> A.Bitand | A.BitxorAssign -> A.Bitxor | A.BitorAssign -> A.Bitor
        in
        let new_val =
            (match ast_binop with (* Similar to SBinop but with old_lhs_val and rhs_val *)
            | A.Plus when Semant.is_integer lhs_ty  -> L.build_add old_lhs_val rhs_val "addtmp" builder
            | A.Plus (* Floats *)                 -> L.build_fadd old_lhs_val rhs_val "faddtmp" builder
            | A.Minus when Semant.is_integer lhs_ty -> L.build_sub old_lhs_val rhs_val "subtmp" builder
            | A.Minus (* Floats *)                -> L.build_fsub old_lhs_val rhs_val "fsubtmp" builder
            | A.Mult when Semant.is_integer lhs_ty  -> L.build_mul old_lhs_val rhs_val "multmp" builder
            | A.Mult (* Floats *)                 -> L.build_fmul old_lhs_val rhs_val "fmultmp" builder
            | A.Div when Semant.is_integer lhs_ty   -> L.build_sdiv old_lhs_val rhs_val "divtmp" builder (* Assuming signed division for I types, U types would need udiv *)
            | A.Div (* Floats *)                  -> L.build_fdiv old_lhs_val rhs_val "fdivtmp" builder
            | A.Mod  (* Integer only, following SBinop for srem. Assumes lhs_ty is integer. *) -> L.build_srem old_lhs_val rhs_val "modtmp" builder
            | A.Lshift (* Assumes lhs_ty is integer *) -> L.build_shl old_lhs_val rhs_val "lshifttmp" builder
            | A.Rshift (* Assumes lhs_ty is integer *) -> L.build_ashr old_lhs_val rhs_val "rshifttmp" builder (* Following SBinop; arithmetic (signed) shift. Logical for unsigned might be needed. *)
            | A.Bitxor (* Assumes lhs_ty is integer *) -> L.build_xor old_lhs_val rhs_val "bitxortmp" builder
            | A.Bitor (* Assumes lhs_ty is integer *)  -> L.build_or old_lhs_val rhs_val "bitortmp" builder
            | A.Bitand (* Assumes lhs_ty is integer *) -> L.build_and old_lhs_val rhs_val "bitandtmp" builder
            | _ -> failwith ("Unsupported compound assignment op in IRGen: " ^ Semant.local_string_of_biop ast_binop)
            )
        in
        ignore(L.build_store new_val lhs_addr builder);
        new_val

    | SSequence (se1, se2) ->
        ignore(build_expr builder local_vars current_func_llval se1);
        build_expr builder local_vars current_func_llval se2

    | SFunctionCall (fname, args_sast) ->
        let args_ll = List.map (build_expr builder local_vars current_func_llval) args_sast in
        let callee_llval =
          match fname with
          |"printf" -> printf_func
          |"exit" -> exit_func
          | _ ->
            try StringMap.find fname !function_decls
            with Not_found -> failwith ("IRGen: Unknown function referenced: " ^ fname)
        in
        L.build_call callee_llval (Array.of_list args_ll) (if sexpr_ty = TyUnit then "" else "calltmp") builder

    | SMethodCall (receiver_sast, mname, args_sast) ->
        let receiver_ll = build_expr builder local_vars current_func_llval receiver_sast in
        let (receiver_ty, _) = receiver_sast in
        let struct_name = match receiver_ty with TyStruct n -> n | _ -> failwith "Method call on non-struct" in
        let mangled_name = mangle_method struct_name mname in
        let callee = StringMap.find mangled_name !function_decls in
        let args_ll = receiver_ll :: List.map (build_expr builder local_vars current_func_llval) args_sast in
        L.build_call callee (Array.of_list args_ll) (if sexpr_ty = TyUnit then "" else "methodcalltmp") builder

    | SCast (target_sast_ty, source_se) ->
        let source_val = build_expr builder local_vars current_func_llval source_se in
        let source_ll_ty = L.type_of source_val in
        let target_ll_ty = ltype_of_sast_ty target_sast_ty in
        let (source_sast_prim_ty, _) = source_se in

        (* Helper to check if a Sast.ty is a float type *)
        let is_float_sast_ty = function TyPrim (A.F32 | A.F64) -> true | _ -> false in
        (* Helper to check if a Sast.ty is an integer type *)
        let is_int_sast_ty = function TyPrim (A.U8|A.U16|A.U32|A.U64|A.I8|A.I16|A.I32|A.I64) -> true | _ -> false in
        (* Helper to check if a Sast.ty is a signed integer type *)
        let is_signed_int_sast_ty = function TyPrim (A.I8|A.I16|A.I32|A.I64) -> true | _ -> false in

        if source_ll_ty = target_ll_ty then source_val (* No-op cast *)
        else if is_float_sast_ty source_sast_prim_ty && is_int_sast_ty target_sast_ty then
            if is_signed_int_sast_ty target_sast_ty then L.build_fptosi source_val target_ll_ty "fptosi" builder
            else L.build_fptoui source_val target_ll_ty "fptoui" builder
        else if is_int_sast_ty source_sast_prim_ty && is_float_sast_ty target_sast_ty then
            if is_signed_int_sast_ty source_sast_prim_ty then L.build_sitofp source_val target_ll_ty "sitofp" builder
            else L.build_uitofp source_val target_ll_ty "uitofp" builder
        else if is_int_sast_ty source_sast_prim_ty && is_int_sast_ty target_sast_ty then
            (* Integer to Integer: sext, zext, or trunc *)
            let source_bits = L.integer_bitwidth source_ll_ty in
            let target_bits = L.integer_bitwidth target_ll_ty in
            if target_bits > source_bits then
                if is_signed_int_sast_ty source_sast_prim_ty then L.build_sext source_val target_ll_ty "sext" builder
                else L.build_zext source_val target_ll_ty "zext" builder
            else (* target_bits <= source_bits *)
                L.build_trunc source_val target_ll_ty "trunc" builder
        else if is_float_sast_ty source_sast_prim_ty && is_float_sast_ty target_sast_ty then
            (* Float to Float: fpext or fptrunc *)
              let source_bits = if source_ll_ty = L.float_type context then 32 else 64 in
              let target_bits = if target_ll_ty = L.float_type context then 32 else 64 in
              if target_bits > source_bits then L.build_fpext source_val target_ll_ty "fpext" builder
              else L.build_fptrunc source_val target_ll_ty "fptrunc" builder
        else
            L.build_bitcast source_val target_ll_ty "bitcast" builder (* Fallback, might be unsafe *)

  and build_stmt builder (local_vars : L.llvalue StringMap.t ref) (current_func_llval: L.llvalue) (sstmt : sstmt) : unit =
    match sstmt with
    | SExpr se -> ignore(build_expr builder !local_vars current_func_llval se)
    | SVarDecl vd ->
        let var_ll_type = ltype_of_sast_ty vd.var_type in
        let alloca = L.build_alloca var_ll_type vd.name builder in
        local_vars := StringMap.add vd.name alloca !local_vars;
        (match vd.initializer_expr with
        | Some ie ->
            let init_val = build_expr builder !local_vars current_func_llval ie in
            ignore(L.build_store init_val alloca builder)
        | None -> () (* No initializer, just allocated *)
        )
    | SBlock stmts ->
        (* OCaml maps don't have easy push/pop for true lexical scoping with shadowing.
            For now, assume all var decls in a function create unique stack slots.
            If a block introduces a new var that shadows, it will be a new alloca.
        *)
        let _ = List.fold_left (fun _ s -> build_stmt builder local_vars current_func_llval s) () stmts in ()

    | SIf (cond_se, then_stmts, else_opt) ->
        let cond_val = build_expr builder !local_vars current_func_llval cond_se in

        let then_bb = L.append_block context "then" current_func_llval in
        let merge_bb = L.append_block context "ifcont" current_func_llval in

        let else_bb_opt, else_builder_opt =
          match else_opt with
          | Some _ ->
              let bb = L.append_block context "else" current_func_llval in
              let b = L.builder_at_end context bb in
              (Some bb, Some b)
          | None -> (None, None)
        in

        let else_target_bb = match else_bb_opt with Some bb -> bb | None -> merge_bb in
        ignore(L.build_cond_br cond_val then_bb else_target_bb builder);

        (* Build then block *)
        L.position_at_end then_bb builder;
        List.iter (build_stmt builder local_vars current_func_llval) then_stmts;
        if Option.is_none (L.block_terminator (L.insertion_block builder)) then
          ignore(L.build_br merge_bb builder);

        (* Build else block if it exists *)
        (match else_opt, else_bb_opt, else_builder_opt with
        | Some else_s, Some actual_else_bb, Some else_b ->
            L.position_at_end actual_else_bb else_b; (* Use the separate builder for else *)
            build_stmt else_b local_vars current_func_llval else_s; (* build_stmt for the single else statement (which could be a block) *)
              if Option.is_none (L.block_terminator (L.insertion_block else_b)) then
                ignore(L.build_br merge_bb else_b);
        | _ -> ()
        );
        L.position_at_end merge_bb builder

    | SWhile (cond_se, body_stmts) ->
        let cond_bb = L.append_block context "while.cond" current_func_llval in
        let body_bb = L.append_block context "while.body" current_func_llval in
        let after_bb = L.append_block context "while.end" current_func_llval in

        loop_env := (cond_bb, after_bb) :: !loop_env; (* Push loop context for break/continue *)

        ignore(L.build_br cond_bb builder); (* Jump to condition check first *)

        L.position_at_end cond_bb builder;
        let cond_val = build_expr builder !local_vars current_func_llval cond_se in
        ignore(L.build_cond_br cond_val body_bb after_bb builder);

        L.position_at_end body_bb builder;
        List.iter (build_stmt builder local_vars current_func_llval) body_stmts;
        if Option.is_none (L.block_terminator (L.insertion_block builder)) then
            ignore(L.build_br cond_bb builder); (* Loop back to condition *)

        loop_env := List.tl !loop_env; (* Pop loop context *)
        L.position_at_end after_bb builder


    | SFor (init_stmt_opt, cond_se_opt, update_se_opt, body_stmts) ->
        let loop_cond_bb = L.append_block context "for.cond" current_func_llval in
        let loop_body_bb = L.append_block context "for.body" current_func_llval in
        let loop_update_bb = L.append_block context "for.update" current_func_llval in
        let loop_after_bb = L.append_block context "for.end" current_func_llval in

        loop_env := (loop_update_bb, loop_after_bb) :: !loop_env; (* For continue, jump to update; for break, jump to after *)

        (* Initializer *)
        Option.iter (build_stmt builder local_vars current_func_llval) init_stmt_opt;
        ignore(L.build_br loop_cond_bb builder); (* Jump to condition check *)

        (* Condition *)
        L.position_at_end loop_cond_bb builder;
        (match cond_se_opt with
        | Some cond_se ->
            let cond_val = build_expr builder !local_vars current_func_llval cond_se in
            ignore(L.build_cond_br cond_val loop_body_bb loop_after_bb builder)
        | None -> ignore(L.build_br loop_body_bb builder) (* No condition, loop indefinitely (until break) *)
        );

        (* Body *)
        L.position_at_end loop_body_bb builder;
        List.iter (build_stmt builder local_vars current_func_llval) body_stmts;
          if Option.is_none (L.block_terminator (L.insertion_block builder)) then
            ignore(L.build_br loop_update_bb builder); (* Jump to update *)

        (* Update *)
        L.position_at_end loop_update_bb builder;
        Option.iter (fun se -> ignore(build_expr builder !local_vars current_func_llval se)) update_se_opt;
        if Option.is_none (L.block_terminator (L.insertion_block builder)) then
            ignore(L.build_br loop_cond_bb builder); (* Jump back to condition *)

        loop_env := List.tl !loop_env; (* Pop loop context *)
        L.position_at_end loop_after_bb builder


    | SReturn se_list_opt ->
        (match se_list_opt, !current_func_return_type with
        | None, Some ret_ll_ty when L.classify_type ret_ll_ty = L.TypeKind.Void -> ignore(L.build_ret_void builder)
        | Some [se], Some _ ->
            let ret_val = build_expr builder !local_vars current_func_llval se in
            ignore(L.build_ret ret_val builder)
        | Some ses, Some ret_ll_ty when L.classify_type ret_ll_ty = L.TypeKind.Struct ->
            (* Multiple return values, pack into struct *)
            let ret_struct_alloca = L.build_alloca ret_ll_ty "ret_agg" builder in
            List.iteri (fun i se_ret ->
                let val_to_store = build_expr builder !local_vars current_func_llval se_ret in
                let field_ptr = L.build_struct_gep ret_struct_alloca i ("ret_field_" ^ string_of_int i) builder in
                ignore(L.build_store val_to_store field_ptr builder)
            ) ses;
            let loaded_ret_struct = L.build_load ret_struct_alloca "loaded_ret_agg" builder in
            ignore(L.build_ret loaded_ret_struct builder)
        | _, None -> failwith "IRGen: current_func_return_type not set"
        | _ -> failwith "IRGen: Return statement mismatch with function signature"
        )
    | SBreak ->
        (match !loop_env with
        | (_, break_bb) :: _ -> ignore(L.build_br break_bb builder)
        | [] -> failwith "IRGen: break statement outside of a loop")
    | SContinue ->
        (match !loop_env with
        | (continue_bb, _) :: _ -> ignore(L.build_br continue_bb builder)
        | [] -> failwith "IRGen: continue statement outside of a loop")
  in

  let build_function_body (f_llval: L.llvalue) (param_sast_decls: sparam list) (body_sast_stmts: sstmt list) (return_sast_types: ty list) =
    Printf.eprintf "  [DEBUG] build_function_body: Function has %d params according to LLVM.\n" (Array.length (L.params f_llval)); flush stderr;
    let builder = L.builder_at_end context (L.entry_block f_llval) in
    Printf.eprintf "  [DEBUG] build_function_body: Builder created at entry block.\n"; flush stderr;
    let local_vars_map : L.llvalue StringMap.t ref = ref StringMap.empty in

    (* Set current function's return type for SReturn handling *)
    Printf.eprintf "  [DEBUG] build_function_body: Setting current_func_return_type.\n"; flush stderr;
    current_func_return_type := Some(
      match return_sast_types with
      | [] -> L.void_type context
      | [t] -> ltype_of_sast_ty t
      | ts -> ltype_of_sast_ty (TyTuple ts)
    );
    Printf.eprintf "  [DEBUG] build_function_body: current_func_return_type set.\n"; flush stderr;

    (* Allocate space for params and store their values *)
    Printf.eprintf "  [DEBUG] build_function_body: Allocating parameters (count: %d)...\n" (List.length param_sast_decls); flush stderr;
    List.iter2(fun (sparam: sparam) llparam ->
      Printf.eprintf "    [DEBUG] Param: %s\n" sparam.name; flush stderr;
      Printf.eprintf "      [DEBUG] sparam.name: %s, sparam.param_type: %s\n" sparam.name (Sast.string_of_ty sparam.param_type); flush stderr;
      Printf.eprintf "      [DEBUG] llparam (raw): %s\n" (L.string_of_llvalue llparam); flush stderr;

      L.set_value_name sparam.name llparam;
      Printf.eprintf "      [DEBUG] Set value name for llparam.\n"; flush stderr;

      let ll_param_type = ltype_of_sast_ty sparam.param_type in
      Printf.eprintf "      [DEBUG] ll_param_type for alloca: %s\n" (L.string_of_lltype ll_param_type); flush stderr;

      let alloca = L.build_alloca ll_param_type sparam.name builder in
      Printf.eprintf "      [DEBUG] Alloca created: %s (type: %s)\n" (L.string_of_llvalue alloca) (L.string_of_lltype (L.type_of alloca)); flush stderr;

      ignore(L.build_store llparam alloca builder);
      Printf.eprintf "      [DEBUG] Stored llparam into alloca.\n"; flush stderr;

      local_vars_map := StringMap.add sparam.name alloca !local_vars_map;
      Printf.eprintf "      [DEBUG] Added alloca to local_vars_map.\n"; flush stderr;
    ) param_sast_decls (Array.to_list (L.params f_llval));
    Printf.eprintf "  [DEBUG] build_function_body: Finished allocating parameters.\n"; flush stderr;

    (* Build statements *)
    List.iter (build_stmt builder local_vars_map f_llval) body_sast_stmts;

    (* Add implicit return for void functions if last block isn't terminated *)
    if (match return_sast_types with [] -> true | [TyUnit] -> true | _ -> false) then
      if Option.is_none (L.block_terminator (L.insertion_block builder)) then
        ignore(L.build_ret_void builder)
    else if Option.is_none (L.block_terminator (L.insertion_block builder)) then
      (* This case should ideally be caught by semantic analysis (non-void function must return) *)
      ignore(L.build_unreachable builder)
  in

  (* Build regular functions *)
  Printf.eprintf "IRGen: Building regular function bodies...\n"; flush stderr; (* DEBUG *)
  List.iter (fun (fdecl : sfunc_decl) ->
    Printf.eprintf "IRGen: Building body for function %s...\n" fdecl.name; flush stderr; (* DEBUG *)
    let f_llval = StringMap.find fdecl.name !function_decls in
    build_function_body f_llval fdecl.params fdecl.body fdecl.return_types;
    Printf.eprintf "IRGen: Finished body for function %s.\n" fdecl.name; flush stderr; (* DEBUG *)
  ) sprogram.sp_funcs;
  Printf.eprintf "IRGen: Finished building regular function bodies.\n"; flush stderr; (* DEBUG *)


  (* Build struct methods *)
  List.iter (fun (mdecl : sstruct_func) ->
    let mangled_name = mangle_method mdecl.struct_name mdecl.name in
    let m_llval = StringMap.find mangled_name !function_decls in

    (* Create sparam list including the receiver *)
    let receiver_sparam = { name = mdecl.receiver_name; param_type = TyStruct mdecl.struct_name } in
    let all_sparams = receiver_sparam :: mdecl.params in

    build_function_body m_llval all_sparams mdecl.body mdecl.return_types
  ) sprogram.sp_methods;

  (* Verify the module before returning it *)
  Printf.eprintf "IRGen: About to verify module...\n"; flush stderr; (* DEBUG *)
  Llvm_analysis.assert_valid_module  the_module;
  Printf.eprintf "IRGen: Module verified successfully.\n"; flush stderr; (* DEBUG *)

  the_module
;;
