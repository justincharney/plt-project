(* irgen.ml *)

module L = Llvm
module A = Ast
open Sast
open Semant (* For is_integer, local_string_of_biop etc. *)


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
    Printf.eprintf "  [DEBUG] ltype_of_sast_ty: %s\n" (Sast.string_of_ty t); flush stderr;
    match t with
    | TyPrim p ->
      (
        match p with
          | A.I8 | A.U8     -> L.i8_type      context
          | A.I16 | A.U16   -> L.i16_type     context
          | A.I32 | A.U32   -> L.i32_type     context
          | A.I64 | A.U64   -> L.i64_type     context
          | A.F32         -> L.float_type   context
          | A.F64         -> L.double_type  context
          | A.Bool        -> L.i1_type      context
          | A.String      -> string_ll_type
          | A.Error       -> failwith "TyError should not reach IR gen"
      )
    | TyArray(elt_t, n) -> L.array_type (ltype_of_sast_ty elt_t) n
    | TyStruct name -> (
      Printf.eprintf "    [DEBUG] ltype_of_sast_ty for TyStruct %s: looking in ll_struct_types\n" name; flush stderr;
      try StringMap.find name !ll_struct_types
        with Not_found ->
          Printf.eprintf "    [DEBUG] ltype_of_sast_ty for TyStruct %s: not in ll_struct_types, looking in ll_alias_types\n" name; flush stderr;
          try StringMap.find name !ll_alias_types
          with Not_found ->
            Printf.eprintf "    [ERROR] ltype_of_sast_ty for TyStruct %s: Not found in either map.\n" name; flush stderr;
            StringMap.iter (fun k _ -> Printf.eprintf "      ll_struct_types has: %s\n" k) !ll_struct_types;
            StringMap.iter (fun k _ -> Printf.eprintf "      ll_alias_types has: %s\n" k) !ll_alias_types;
            failwith ("Unknown struct type in ltype_of_sast_ty: " ^ name)
    )
    | TyTuple []        -> L.void_type context (* Should ideally be TyUnit *)
    | TyTuple [ty_elt]  -> ltype_of_sast_ty ty_elt
    | TyTuple ts        -> L.struct_type context (Array.of_list (List.map ltype_of_sast_ty ts))
    | TyNull            -> L.pointer_type (L.i8_type context)
    | TyUnit            -> L.void_type context
    | TyError           -> failwith "TyError should not reach IR generation (ltype_of_sast_ty)"
  in

  let process_type_declarations () =
    let struct_defs = List.filter_map (function STypeStruct(n,f) -> Some (n,f) | _ -> None) sprogram.sp_types in
    let alias_defs = List.filter_map (function STypeAlias(n,t) -> Some (n,t) | _ -> None) sprogram.sp_types in

    Printf.eprintf "  [DEBUG] process_type_declarations: Defining struct type names (pass 1)\n"; flush stderr;
    List.iter (fun (name, _) ->
      Printf.eprintf "    [DEBUG] Defining struct type name: %s\n" name; flush stderr;
      let ll_ty = L.named_struct_type context name in
      ll_struct_types := StringMap.add name ll_ty !ll_struct_types
    ) struct_defs;

    Printf.eprintf "  [DEBUG] process_type_declarations: Setting struct type bodies (pass 2)\n"; flush stderr;
    List.iter (fun (name, current_sfields : string * Sast.sfield list) -> (* Explicit type for the tuple's second element *)
      Printf.eprintf "    [DEBUG] Setting body for struct type: %s\n" name; flush stderr;
      let ll_ty = StringMap.find name !ll_struct_types in
      let field_ll_types = List.map (fun (f : Sast.sfield) -> (* Explicit type for f *)
        Printf.eprintf "      [DEBUG] Field %s type: %s\n" f.name (Sast.string_of_ty f.field_type); flush stderr;
        ltype_of_sast_ty f.field_type
      ) current_sfields in (* Use the explicitly typed list *)
      L.struct_set_body ll_ty (Array.of_list field_ll_types) false
    ) struct_defs;

    Printf.eprintf "  [DEBUG] process_type_declarations: Processing alias types\n"; flush stderr;
    List.iter (fun (name, sast_ty) ->
        Printf.eprintf "    [DEBUG] Defining alias type: %s = %s\n" name (Sast.string_of_ty sast_ty); flush stderr;
        let ll_ty = ltype_of_sast_ty sast_ty in
        ll_alias_types := StringMap.add name ll_ty !ll_alias_types
    ) alias_defs
  in
  Printf.eprintf "IRGen: Processing type declarations...\n"; flush stderr;
  process_type_declarations ();
  Printf.eprintf "IRGen: Finished type declarations.\n"; flush stderr;

  let global_vars_map : L.llvalue StringMap.t ref = ref StringMap.empty in
  let rec build_global_initializer (var_name_for_debug: string) (var_ty: ty) (init_expr_opt: sexpr option) (_builder_dummy : L.llbuilder) : L.llvalue =
    Printf.eprintf "    [DEBUG] build_global_initializer for '%s', var_ty: %s\n" var_name_for_debug (Sast.string_of_ty var_ty); flush stderr;
    match init_expr_opt with
    | None ->
        Printf.eprintf "      [DEBUG] No initializer for '%s', creating const_null for type %s.\n" var_name_for_debug (Sast.string_of_ty var_ty); flush stderr;
        let ll_var_ty = ltype_of_sast_ty var_ty in
        L.const_null ll_var_ty
    | Some (sexpr_ty, sx) ->
        Printf.eprintf "      [DEBUG] Initializer for '%s': sexpr_ty: %s, sx: %s\n" var_name_for_debug (Sast.string_of_ty sexpr_ty) (Sast.string_of_sexpr_node sx); flush stderr;
        (match sx with
        | SIntLit i ->
            Printf.eprintf "        [DEBUG] SIntLit %d\n" i; flush stderr;
            (match sexpr_ty with
                | TyPrim (A.I8 | A.U8)   -> L.const_int (L.i8_type context) i
                | TyPrim (A.I16 | A.U16) -> L.const_int (L.i16_type context) i
                | TyPrim (A.I32 | A.U32) -> L.const_int (L.i32_type context) i
                | TyPrim (A.I64 | A.U64) -> L.const_int (L.i64_type context) i
                | _ -> L.const_int (L.i32_type context) i (* Should be caught by semant *)
            )
        | SBoolLit b  ->
            Printf.eprintf "        [DEBUG] SBoolLit %b\n" b; flush stderr;
            L.const_int (L.i1_type context) (if b then 1 else 0)
        | SFloatLit f ->
            Printf.eprintf "        [DEBUG] SFloatLit %f\n" f; flush stderr;
            (match sexpr_ty with
                | TyPrim A.F32 -> L.const_float (L.float_type context) f
                | TyPrim A.F64 -> L.const_float (L.double_type context) f
                | _ -> L.const_float (L.double_type context) f (* Should be caught by semant *)
            )
        | SStringLit s ->
            Printf.eprintf "        [DEBUG] SStringLit \"%s\"\n" s; flush stderr;
            let global_str_val = L.define_global (".str_const_" ^ var_name_for_debug) (L.const_stringz context s) the_module in
            L.set_linkage L.Linkage.Internal global_str_val;
            let str_ptr = L.const_in_bounds_gep global_str_val [| L.const_int (L.i32_type context) 0; L.const_int (L.i32_type context) 0 |] in
            let len = L.const_int (L.i64_type context) (String.length s) in
            (* Use const_named_struct with the pre-defined string_ll_type (%String) *)
            L.const_named_struct string_ll_type [| str_ptr; len |]
        | SNull ->
            Printf.eprintf "        [DEBUG] SNull\n"; flush stderr;
            let ll_sexpr_ty = ltype_of_sast_ty sexpr_ty in
            L.const_null ll_sexpr_ty
        | SArrayLit (elem_ty, selms) ->
            Printf.eprintf "        [DEBUG] SArrayLit, elem_ty: %s\n" (Sast.string_of_ty elem_ty); flush stderr;
            let ll_elem_ty = ltype_of_sast_ty elem_ty in
            let const_elems = List.mapi (fun i se ->
                Printf.eprintf "          [DEBUG] SArrayLit elem %d\n" i; flush stderr;
                build_global_initializer (var_name_for_debug ^ "_arr_elem" ^ string_of_int i) elem_ty (Some se) _builder_dummy
            ) selms in
            L.const_array ll_elem_ty (Array.of_list const_elems)
        | SStructLit (struct_name_from_lit, field_inits) ->
          (* struct_name_from_lit is the name used in the literal, e.g., "Vector" *)
          (* sexpr_ty is the resolved type of this literal, e.g., TyStruct "Point" *)
          let canonical_struct_name =
            match sexpr_ty with
            | TyStruct csn -> csn
            | _ -> failwith ("IRGen Error: SStructLit's expression type is not TyStruct: " ^ (Sast.string_of_ty sexpr_ty) ^ " for literal " ^ struct_name_from_lit)
          in
          Printf.eprintf "        [DEBUG] SStructLit: literal name '%s', canonical name for lookup: '%s'\n" struct_name_from_lit canonical_struct_name; flush stderr;
          Printf.eprintf "          [DEBUG] Attempting ltype_of_sast_ty for TyStruct %s (canonical)\n" canonical_struct_name; flush stderr;
          let _ = ltype_of_sast_ty (TyStruct canonical_struct_name) in (* Ensure struct type is known, using canonical name. This also prints debug info from ltype_of_sast_ty *)
          Printf.eprintf "          [DEBUG] ltype_of_sast_ty for TyStruct %s (canonical) OK.\n" canonical_struct_name; flush stderr;

          let s_info =
            try List.find (function STypeStruct(n,_) when n = canonical_struct_name -> true | _ -> false) sprogram.sp_types
            with Not_found ->
              Printf.eprintf "          [ERROR] Struct definition for canonical name '%s' (from literal '%s') not found in sprogram.sp_types during SStructLit processing.\n" canonical_struct_name struct_name_from_lit; flush stderr;
              List.iter (function STypeStruct(n,_) -> Printf.eprintf "            Available STypeStruct: %s\n" n | STypeAlias(n,_) -> Printf.eprintf "            Available STypeAlias: %s\n" n ) sprogram.sp_types;
              failwith ("Struct " ^ canonical_struct_name ^ " not found for const init")
          in
          Printf.eprintf "          [DEBUG] Found s_info for struct '%s'.\n" canonical_struct_name; flush stderr;
          let ordered_sfields = match s_info with STypeStruct(_,fs) -> fs | _ -> failwith ("Struct " ^ canonical_struct_name ^ " not found for const init (should be impossible here after find)") in

          Printf.eprintf "          [DEBUG] Building const_fields_map for struct '%s'.\n" canonical_struct_name; flush stderr;
          let const_fields_map = List.fold_left (fun acc (fname, se) ->
              Printf.eprintf "            [DEBUG] SStructLit field init: %s\n" fname; flush stderr;
              StringMap.add fname (build_global_initializer (var_name_for_debug ^ "_" ^ fname) (fst se) (Some se) _builder_dummy) acc
          ) StringMap.empty field_inits in

          Printf.eprintf "          [DEBUG] Mapping ordered_sfields for struct '%s'.\n" canonical_struct_name; flush stderr;
          let const_fields = List.map (fun (sf:sfield) ->
              Printf.eprintf "            [DEBUG] SStructLit ordered field: %s (type: %s)\n" sf.name (Sast.string_of_ty sf.field_type); flush stderr;
              try
                let f_val = StringMap.find sf.name const_fields_map in
                Printf.eprintf "              [DEBUG] Found provided initializer for field %s.\n" sf.name; flush stderr;
                f_val
              with Not_found ->
                  Printf.eprintf "              [DEBUG] No initializer provided for field %s. Checking for default.\n" sf.name; flush stderr;
                  match sf.default_value with
                  | Some def_se ->
                      Printf.eprintf "                [DEBUG] Using default value for field %s.\n" sf.name; flush stderr;
                      build_global_initializer (var_name_for_debug ^ "_" ^ sf.name ^ "_default") sf.field_type (Some def_se) _builder_dummy
                  | None ->
                      Printf.eprintf "                [DEBUG] No default value for field %s. Using const_null.\n" sf.name; flush stderr;
                      L.const_null (ltype_of_sast_ty sf.field_type)
          ) ordered_sfields in
          (* Get the LLVM type for the NAMED struct e.g. %Point from "Point" *)
          let named_ll_struct_type = ltype_of_sast_ty (TyStruct canonical_struct_name) in
          let result_struct = L.const_named_struct named_ll_struct_type (Array.of_list const_fields) in
          Printf.eprintf "      [DEBUG] build_global_initializer: SStructLit for '%s' (canonical '%s') processed successfully.\n" struct_name_from_lit canonical_struct_name; flush stderr;
          result_struct
        | _ -> Printf.eprintf "      [DEBUG] Unhandled SX node for global init: %s. Using const_null for var_ty %s.\n" (Sast.string_of_sexpr_node sx) (Sast.string_of_ty var_ty); flush stderr;  L.const_null (ltype_of_sast_ty var_ty)
        )
  in
  Printf.eprintf "IRGen: Processing global variables...\n"; flush stderr;
  List.iter (fun (gdecl : sglobal_decl) ->
    Printf.eprintf "  [DEBUG] Processing global var: %s of type %s\n" gdecl.name (Sast.string_of_ty gdecl.var_type); flush stderr;
    try
      let dummy_builder = L.builder context in (* Dummy builder, not used for const generation but might be if build_expr was used *)
      Printf.eprintf "    [DEBUG] Calling build_global_initializer for %s\n" gdecl.name; flush stderr;
      let init_val = build_global_initializer gdecl.name gdecl.var_type gdecl.initializer_expr dummy_builder in
      Printf.eprintf "    [DEBUG] build_global_initializer for %s returned. Defining global.\n" gdecl.name; flush stderr;
      let global_val = L.define_global gdecl.name init_val the_module in
      Printf.eprintf "    [DEBUG] Global %s defined. Adding to global_vars_map.\n" gdecl.name; flush stderr;
      global_vars_map := StringMap.add gdecl.name global_val !global_vars_map;
      Printf.eprintf "    [DEBUG] Global %s added to map.\n" gdecl.name; flush stderr;
    with
    | Not_found as e ->
        Printf.eprintf "  [FATAL ERROR] Not_found exception during processing of global variable '%s'.\n" gdecl.name;
        Printf.eprintf "    Exception details: %s\n" (Printexc.to_string e);
        Printexc.print_backtrace stderr;
        flush stderr;
        raise e
    | Failure msg as e ->
        Printf.eprintf "  [FATAL ERROR] Failure exception during processing of global variable '%s': %s\n" gdecl.name msg;
        Printexc.print_backtrace stderr;
        flush stderr;
        raise e
    | e ->
        Printf.eprintf "  [FATAL ERROR] Unexpected exception during processing of global variable '%s'.\n" gdecl.name;
        Printf.eprintf "    Exception details: %s\n" (Printexc.to_string e);
        Printexc.print_backtrace stderr;
        flush stderr;
        raise e
  ) sprogram.sp_globals;
  Printf.eprintf "IRGen: Finished global variables.\n"; flush stderr;

  let function_decls : L.llvalue StringMap.t ref = ref StringMap.empty in

  (* Modified: Use L.define_function for functions defined in this module *)
  let define_pat_function name params_sast_types return_sast_types =
    let param_ll_types = Array.of_list (List.map ltype_of_sast_ty params_sast_types) in
    let return_ll_type =
      match return_sast_types with
      | [] | [TyUnit]-> L.void_type context
      | [t] -> ltype_of_sast_ty t
      | ts -> ltype_of_sast_ty (TyTuple ts)
    in
    let func_ll_type = L.function_type return_ll_type param_ll_types in
    let func_ll_val = L.define_function name func_ll_type the_module in (* USE L.define_function *)
    function_decls := StringMap.add name func_ll_val !function_decls
  in

  Printf.eprintf "IRGen: Defining P.A.T. functions...\n"; flush stderr;
  List.iter (fun (fdecl : sfunc_decl) ->
    let param_types = List.map (fun p -> p.param_type) fdecl.params in
    define_pat_function fdecl.name param_types fdecl.return_types
  ) sprogram.sp_funcs;
  Printf.eprintf "IRGen: Finished defining P.A.T. functions.\n"; flush stderr;

  if sprogram.sp_methods <> [] then Printf.eprintf "IRGen: Defining P.A.T. struct methods...\n"; flush stderr;
  let mangle_method struct_name method_name = struct_name ^ "$" ^ method_name in
  List.iter (fun (mdecl : sstruct_func) ->
    let receiver_sast_ty = TyStruct mdecl.struct_name in
    let param_sast_types = receiver_sast_ty :: (List.map (fun p -> p.param_type) mdecl.params) in
    let mangled_name = mangle_method mdecl.struct_name mdecl.name in
    define_pat_function mangled_name param_sast_types mdecl.return_types
  ) sprogram.sp_methods;
  if sprogram.sp_methods <> [] then Printf.eprintf "IRGen: Finished defining P.A.T. struct methods.\n"; flush stderr;

  let printf_t    : L.lltype  = L.var_arg_function_type (L.i32_type context) [| L.pointer_type (L.i8_type context) |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in (* Built-ins are declared *)
  let exit_t      = L.function_type (L.void_type context) [| L.i32_type context |] in
  let exit_func   = L.declare_function "exit" exit_t the_module in (* Built-ins are declared *)

  let current_func_return_type : L.lltype option ref = ref None in
  let loop_env : (L.llbasicblock * L.llbasicblock) list ref = ref [] in

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
            let struct_ptr = build_lvalue_address (snd struct_sexpr) in
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
            let coll_ptr = build_lvalue_address (snd coll_sexpr) in
            let idx_val = build_expr builder local_vars current_func_llval idx_sexpr in
            (match fst coll_sexpr with
            | TyArray _ -> L.build_gep coll_ptr [| L.const_int (L.i32_type context) 0; idx_val |] "elem_ptr" builder
            | TyTuple _ ->
                (match snd idx_sexpr with
                  | SIntLit i -> L.build_struct_gep coll_ptr i "tuple_elem_ptr" builder
                  | _ -> failwith "Tuple index must be a constant integer literal for GEP")
            | _ -> failwith "Index access on non-array/tuple type (lvalue)")
        | _ -> failwith "Expression is not a valid l-value"
    in

    match sx with
    | SIntLit i -> L.const_int (ltype_of_sast_ty sexpr_ty) i
    | SBoolLit b -> L.const_int (L.i1_type context) (if b then 1 else 0)
    | SCharLit c -> L.const_int (L.i8_type context) (Char.code c)
    | SFloatLit f -> L.const_float (ltype_of_sast_ty sexpr_ty) f
    | SStringLit s ->
        let str_data_global = L.build_global_stringptr s ".str" builder in
        let string_struct_alloca = L.build_alloca string_ll_type "str_lit_obj" builder in
        let data_ptr_field = L.build_struct_gep string_struct_alloca 0 "str_data_ptr" builder in
        ignore(L.build_store str_data_global data_ptr_field builder);
        let len_val = L.const_int (L.i64_type context) (String.length s) in
        let len_ptr_field = L.build_struct_gep string_struct_alloca 1 "str_len_ptr" builder in
        ignore(L.build_store len_val len_ptr_field builder);
        L.build_load string_struct_alloca "loaded_str_lit_obj" builder

    | SNull -> L.const_null (ltype_of_sast_ty sexpr_ty)

    | SArrayLit (_elem_sast_ty, selms) ->
      let array_ll_ty = ltype_of_sast_ty sexpr_ty in
      let array_alloca = L.build_alloca array_ll_ty "arr_lit_obj" builder in
      List.iteri (fun i selem ->
          let elem_val = build_expr builder local_vars current_func_llval selem in
          let elem_ptr = L.build_gep array_alloca [| L.const_int (L.i32_type context) 0; L.const_int (L.i32_type context) i |] ("elem_" ^ string_of_int i) builder in
          ignore(L.build_store elem_val elem_ptr builder)
      ) selms;
      L.build_load array_alloca "loaded_arr_lit_obj" builder

    | SStructLit (struct_name, field_inits_sast) ->
        let struct_ll_ty = ltype_of_sast_ty sexpr_ty in
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
                    | None -> failwith ("IRGen: No value for field " ^ sf.name ^ " in " ^ struct_name)
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

    | SFieldAccess (_, field_name_str) as sx_field_access ->
      (* sx_field_access is the SFieldAccess node itself *)
      let specific_field_addr = build_lvalue_address sx_field_access in
      L.build_load specific_field_addr field_name_str builder


    | SIndexAccess (_, _idx_sexpr_node) as sx_index_access -> (* Use the whole sx for build_lvalue_address *)
        (* sx_index_access is the SIndexAccess node itself *)
        let specific_elem_addr = build_lvalue_address sx_index_access in (* This will get e.g. %Point* for tuple[0] *)
        L.build_load specific_elem_addr "indexed_val" builder (* Load from e.g. %Point* giving %Point *)

    | SBinop (se1, op, se2) ->
        let v1 = build_expr builder local_vars current_func_llval se1 in
        let v2 = build_expr builder local_vars current_func_llval se2 in
        Printf.eprintf "  [DEBUG] SBinop: op '%s'\n" (Sast._sast_string_of_biop op);
        Printf.eprintf "    v1 LLVM Value: %s\n" (L.string_of_llvalue v1);
        Printf.eprintf "    v1 LLVM Type:  %s\n" (L.string_of_lltype (L.type_of v1));
        Printf.eprintf "    v2 LLVM Value: %s\n" (L.string_of_llvalue v2);
        Printf.eprintf "    v2 LLVM Type:  %s\n" (L.string_of_lltype (L.type_of v2));
        flush stderr;
        let (t1, _) = se1 in
        let (t2, _) = se2 in
        (match op with
          | A.Plus    when Semant.is_string  t1 -> failwith "String concatenation via '+' not yet implemented in IRgen"
          | A.Plus    when Semant.is_integer t1 -> L.build_add  v1 v2 "addtmp"  builder
          | A.Plus                              -> L.build_fadd v1 v2 "faddtmp" builder
          | A.Minus   when Semant.is_integer t1 -> L.build_sub  v1 v2 "subtmp"  builder
          | A.Minus                             -> L.build_fsub v1 v2 "fsubtmp" builder
          | A.Mult ->
            (if Semant.is_integer t1 && Semant.is_integer t2 then
              L.build_mul v1 v2 "multmp" builder
            else if Semant.is_numeric t1 && Semant.is_numeric t2 then
              let v1_float = if Semant.is_integer t1 then L.build_sitofp v1 (L.float_type context) "v1tof" builder else v1 in
              let v2_float = if Semant.is_integer t2 then L.build_sitofp v2 (L.float_type context) "v2tofp" builder else v2 in
              L.build_fmul v1_float v2_float "fmultmp" builder
            else
              failwith ("Type error in multiplication: " ^ Sast.string_of_ty t1 ^ " * " ^ Sast.string_of_ty t2))
          | A.Div     when Semant.is_integer t1 -> L.build_sdiv v1 v2 "divtmp"  builder
          | A.Div                               -> L.build_fdiv v1 v2 "fdivtmp" builder
          | A.Mod     when Semant.is_integer t1 -> L.build_srem v1 v2 "modtmp"  builder
          | A.Lshift  when Semant.is_integer t1 -> L.build_shl  v1 v2 "lshifttmp" builder
          | A.Rshift  when Semant.is_integer t1 -> L.build_ashr v1 v2 "rshifttmp" builder
          | A.Bitxor  when Semant.is_integer t1 -> L.build_xor  v1 v2 "bitxortmp" builder
          | A.Bitor   when Semant.is_integer t1 -> L.build_or   v1 v2 "bitortmp"  builder
          | A.Bitand  when Semant.is_integer t1 -> L.build_and  v1 v2 "bitandtmp" builder
          | A.Eq      when Semant.is_integer t1 || Semant.is_boolean t1 -> L.build_icmp L.Icmp.Eq  v1 v2 "eqtmp"  builder
          | A.Eq                                -> L.build_fcmp L.Fcmp.Oeq v1 v2 "feqtmp" builder
          | A.Neq     when Semant.is_integer t1 || Semant.is_boolean t1 -> L.build_icmp L.Icmp.Ne  v1 v2 "neqtmp" builder
          | A.Neq                               -> L.build_fcmp L.Fcmp.One v1 v2 "fneqtmp"builder
          | A.Lt      when Semant.is_integer t1 -> L.build_icmp L.Icmp.Slt v1 v2 "lttmp"  builder
          | A.Lt                                -> L.build_fcmp L.Fcmp.Olt v1 v2 "flttmp" builder
          | A.Le      when Semant.is_integer t1 -> L.build_icmp L.Icmp.Sle v1 v2 "letmp"  builder
          | A.Le                                -> L.build_fcmp L.Fcmp.Ole v1 v2 "fletmp" builder
          | A.Gt      when Semant.is_integer t1 -> L.build_icmp L.Icmp.Sgt v1 v2 "gttmp"  builder
          | A.Gt                                -> L.build_fcmp L.Fcmp.Ogt v1 v2 "fgttmp" builder
          | A.Ge      when Semant.is_integer t1 -> L.build_icmp L.Icmp.Sge v1 v2 "getmp"  builder
          | A.Ge                                -> L.build_fcmp L.Fcmp.Oge v1 v2 "fgetmp" builder
          | A.And     when Semant.is_boolean t1 -> L.build_and  v1 v2 "andtmp"  builder
          | A.Or      when Semant.is_boolean t1 -> L.build_or   v1 v2 "ortmp"   builder
          | _ -> failwith ("Unsupported binary operator or type mismatch: " ^ local_string_of_biop op ^ " on " ^ Sast.string_of_ty t1)
        )
    | SUnaop (op, se) ->
        let v = build_expr builder local_vars current_func_llval se in
        (match op with
          | A.Neg    -> if Semant.is_integer (fst se) then L.build_neg v "negtmp" builder else L.build_fneg v "fnegtmp" builder
          | A.Not    -> L.build_xor v (L.const_int (L.i1_type context) 1) "nottmp" builder
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
        let ast_binop =
            match op with
            | A.PlusAssign -> A.Plus | A.MinusAssign -> A.Minus | A.TimesAssign -> A.Mult | A.DivAssign -> A.Div
            | A.ModAssign -> A.Mod | A.LshiftAssign -> A.Lshift | A.RshiftAssign -> A.Rshift
            | A.BitandAssign -> A.Bitand | A.BitxorAssign -> A.Bitxor | A.BitorAssign -> A.Bitor
        in
        let new_val =
            (match ast_binop with
            | A.Plus   when Semant.is_integer lhs_ty -> L.build_add  old_lhs_val rhs_val "addtmp"  builder
            | A.Plus                                 -> L.build_fadd old_lhs_val rhs_val "faddtmp" builder
            | A.Minus  when Semant.is_integer lhs_ty -> L.build_sub  old_lhs_val rhs_val "subtmp"  builder
            | A.Minus                                -> L.build_fsub old_lhs_val rhs_val "fsubtmp" builder
            | A.Mult   when Semant.is_integer lhs_ty -> L.build_mul  old_lhs_val rhs_val "multmp"  builder
            | A.Mult                                 -> L.build_fmul old_lhs_val rhs_val "fmultmp" builder
            | A.Div    when Semant.is_integer lhs_ty -> L.build_sdiv old_lhs_val rhs_val "divtmp"  builder
            | A.Div                                  -> L.build_fdiv old_lhs_val rhs_val "fdivtmp" builder
            | A.Mod    when Semant.is_integer lhs_ty -> L.build_srem old_lhs_val rhs_val "modtmp"  builder
            | A.Lshift when Semant.is_integer lhs_ty -> L.build_shl  old_lhs_val rhs_val "lshifttmp" builder
            | A.Rshift when Semant.is_integer lhs_ty -> L.build_ashr old_lhs_val rhs_val "rshifttmp" builder
            | A.Bitxor when Semant.is_integer lhs_ty -> L.build_xor  old_lhs_val rhs_val "bitxortmp" builder
            | A.Bitor  when Semant.is_integer lhs_ty -> L.build_or   old_lhs_val rhs_val "bitortmp"  builder
            | A.Bitand when Semant.is_integer lhs_ty -> L.build_and  old_lhs_val rhs_val "bitandtmp" builder
            | A.And    when Semant.is_boolean lhs_ty -> L.build_and  old_lhs_val rhs_val "andtmp_compound" builder
            | A.Or     when Semant.is_boolean lhs_ty -> L.build_or   old_lhs_val rhs_val "ortmp_compound"  builder
            | _ -> failwith ("Unsupported compound assignment op in IRGen for underlying op: " ^ Semant.local_string_of_biop ast_binop)
            )
        in
        ignore(L.build_store new_val lhs_addr builder);
        new_val

    | SSequence (se1, se2) ->
        ignore(build_expr builder local_vars current_func_llval se1);
        build_expr builder local_vars current_func_llval se2

    | SFunctionCall ("len", [arg_se]) ->
          (match fst arg_se with
          | TyPrim A.String ->
              let llvm_val =
                  build_expr builder local_vars current_func_llval arg_se
              in
              let len64_bit =
                  L.build_extractvalue llvm_val 1 "len64bits" builder
              in
              L.build_trunc len64_bit (L.i32_type context) "len32bits" builder

          | TyArray (_,length) ->
              L.const_int (L.i32_type context) length
          | _ -> failwith ("len() not supported"))

    | SFunctionCall ("cap", [arg_se]) ->
          (match fst arg_se with
          | TyArray (_,capacity) ->
              L.const_int (L.i32_type context) capacity
          | _ -> failwith ("cap() not supported"))

    | SFunctionCall ("assert", [arg_se]) ->
        let cond_result =
            build_expr builder local_vars current_func_llval arg_se
        in
        let pass_result =
            L.append_block context "assert_passed" current_func_llval
        in
        let fail_result =
            L.append_block context "assert_failed" current_func_llval
        in
        ignore (L.build_cond_br cond_result pass_result fail_result builder);
        L.position_at_end fail_result builder;
        ignore (L.build_call exit_func [| L.const_int (L.i32_type context) 1 |] "" builder);
        ignore (L.build_unreachable builder);
        L.position_at_end pass_result builder;
        L.const_null (L.pointer_type (L.i8_type context))

    | SFunctionCall (fname, sast_args) when fname="print_fancy"->
        (match sast_args with
        [s_string;f_num;b_num;i_bool;u_bool] ->
          let f_num_expr = build_expr builder local_vars current_func_llval f_num
          and b_num_expr = build_expr builder local_vars current_func_llval b_num
          and s_expr = build_expr builder local_vars current_func_llval s_string in
          let format_string_expr = match (i_bool,u_bool) with
              ((_,SBoolLit b1),(_,SBoolLit b2)) when b1 && b2 -> StringMap.find "fancy_fmt_both" local_vars
            | ((_,SBoolLit b1),(_,SBoolLit b2)) when b1 && (not b2)-> StringMap.find "fancy_fmt_italic" local_vars
            | ((_,SBoolLit b1),(_,SBoolLit b2)) when (not b1) && b2-> StringMap.find "fancy_fmt_under" local_vars
            | ((_,SBoolLit _),(_,SBoolLit _)) -> StringMap.find "fancy_fmt_none" local_vars
            | _ -> raise (Failure "invalid call to print_fancy")
          in (*ignore(L.build_call printf_func [|format_string_expr|] "printf_fancy" builder);*)
          let actual_call = L.build_call printf_func (Array.of_list [format_string_expr;f_num_expr;b_num_expr;s_expr]) "printf_fancy" builder in
          (*ignore(L.build_call printf_func [|(StringMap.find "clear_format_str" local_vars)|] "printf_fancy" builder);*)
          actual_call
        | _ -> raise (Failure "Invalid call to print_fancy"))

    | SFunctionCall (fname, args_sast) ->
        let args_ll = List.map (build_expr builder local_vars current_func_llval) args_sast in
        let callee_llval, ret_name_suffix =
          match fname with
          | "printf" -> (printf_func, "printf_call")
          | "print_int" -> (printf_func, "printf_call")
          | "print_float" -> (printf_func,"printf_call")
          | "exit" -> (exit_func, "")
          | _ ->
            try (StringMap.find fname !function_decls, "calltmp")
            with Not_found -> failwith ("IRGen: Unknown function referenced: " ^ fname)
        in

        (* Prepare arguments for calling C's printf *)
        let final_args_ll =
          if fname = "printf" then
            match args_sast with
            | (_, _) :: rest_sast_args ->
              let format_ll_val = List.hd args_ll in (* LLVM value for the format string struct *)
              let format_string_ptr = L.build_extractvalue format_ll_val 0 "fmt_str_ptr" builder in
              let variadic_args_ll = List.mapi (fun i (arg_sast_ty, _) ->
                let arg_ll_val = List.nth args_ll (i + 1) in (* Get corresponding LLVM value *)
                match arg_sast_ty with
                | TyPrim A.F32 -> L.build_fpext arg_ll_val (L.double_type context) "fpext_for_printf" builder
                | TyPrim A.String -> L.build_extractvalue arg_ll_val 0 ("arg_str_ptr_" ^ string_of_int i) builder
                | _ -> arg_ll_val
              ) rest_sast_args in
              format_string_ptr :: variadic_args_ll
            | [] -> failwith "IRGen: printf called with no arguments"
          else if fname = "print_int" then
            match args_sast with
            | [(_, _)] ->
              let int_val_ll = List.hd args_ll in
              let fmt_str_ptr = StringMap.find "int_format_str" local_vars in
              [fmt_str_ptr; int_val_ll]
            | _ -> failwith "IRGen: print_int expects exactly one integer argument"
          else if fname = "print_float" then
            match args_sast with
            | [(_, _)] ->
              let float_val_ll = List.hd args_ll in
              let float_val_double = L.build_fpext float_val_ll (L.double_type context) "fpext_for_printf" builder in
              let fmt_str_ptr = StringMap.find "float_format_str" local_vars in
              [fmt_str_ptr; float_val_double]
            | _ -> failwith "IRGen: print_float expects exactly one float argument"
          else args_ll
        in
        let call_instr = L.build_call callee_llval (Array.of_list final_args_ll)
                           (if sexpr_ty = TyUnit || fname = "exit" then "" else ret_name_suffix) builder in
        if fname = "exit" then ignore (L.build_unreachable builder);
        call_instr


    | SMethodCall (receiver_sast, mname, args_sast) ->
        let receiver_ll = build_expr builder local_vars current_func_llval receiver_sast in
        let (receiver_ty, _) = receiver_sast in
        let struct_name = match receiver_ty with TyStruct n -> n | _ -> failwith "Method call on non-struct" in
        let mangled_name = mangle_method struct_name mname in
        let callee = StringMap.find mangled_name !function_decls in
        let args_ll = receiver_ll :: List.map (build_expr builder local_vars current_func_llval) args_sast in
        L.build_call callee (Array.of_list args_ll) (if sexpr_ty = TyUnit then "" else "methodcalltmp") builder

    | SCast (target_sast_ty, source_se) ->
        Printf.eprintf "\n  [DEBUG] SCast: Processing new cast invocation.\n";
        Printf.eprintf "    Target SAST Type (target_sast_ty): %s\n" (Sast.string_of_ty target_sast_ty);
        Printf.eprintf "    Source SAST Expr (fst source_se): %s\n" (Sast.string_of_ty (fst source_se));
        Printf.eprintf "    Source SAST Expr Node (snd source_se): %s\n" (Sast.string_of_sexpr_node (snd source_se));
        flush stderr;

        let source_val = build_expr builder local_vars current_func_llval source_se in
        let source_ll_ty = L.type_of source_val in
        let target_ll_ty = ltype_of_sast_ty target_sast_ty in
        let (source_sast_ty_from_expr, _) = source_se in (* This is the SAST type of the source expression *)

        Printf.eprintf "    LLVM source_val: %s (LLVM Type: %s)\n" (L.string_of_llvalue source_val) (L.string_of_lltype source_ll_ty);
        Printf.eprintf "    LLVM target_ll_ty: %s\n" (L.string_of_lltype target_ll_ty);
        Printf.eprintf "    SAST source_sast_ty_from_expr (used for conditions): %s\n" (Sast.string_of_ty source_sast_ty_from_expr);
        flush stderr;

        (* Local helper definitions for clarity within SCast *)
        let is_float_sast_ty_local = function TyPrim (A.F32 | A.F64) -> true | _ -> false in
        let is_int_sast_ty_local = function TyPrim (A.U8|A.U16|A.U32|A.U64|A.I8|A.I16|A.I32|A.I64) -> true | _ -> false in
        let is_signed_int_sast_ty_local = function TyPrim (A.I8|A.I16|A.I32|A.I64) -> true | _ -> false in

        Printf.eprintf "    Condition checks before branching:\n";
        Printf.eprintf "      is_float_sast_ty_local(source_sast_ty_from_expr = %s)? %b\n" (Sast.string_of_ty source_sast_ty_from_expr) (is_float_sast_ty_local source_sast_ty_from_expr);
        Printf.eprintf "      is_int_sast_ty_local(source_sast_ty_from_expr   = %s)? %b\n" (Sast.string_of_ty source_sast_ty_from_expr) (is_int_sast_ty_local source_sast_ty_from_expr);
        Printf.eprintf "      is_float_sast_ty_local(target_sast_ty    = %s)? %b\n" (Sast.string_of_ty target_sast_ty) (is_float_sast_ty_local target_sast_ty);
        Printf.eprintf "      is_int_sast_ty_local(target_sast_ty      = %s)? %b\n" (Sast.string_of_ty target_sast_ty) (is_int_sast_ty_local target_sast_ty);
        flush stderr;

        if source_ll_ty = target_ll_ty then (
            Printf.eprintf "    [DEBUG] SCast: Path taken: No-op cast (source_ll_ty equals target_ll_ty)\n"; flush stderr;
            source_val
        )
        else if is_float_sast_ty_local source_sast_ty_from_expr && is_int_sast_ty_local target_sast_ty then (
            Printf.eprintf "    [DEBUG] SCast: Path taken: float_to_int\n"; flush stderr;
            if is_signed_int_sast_ty_local target_sast_ty then (
                let cast_instr = L.build_fptosi source_val target_ll_ty "fptosi" builder in
                Printf.eprintf "      [DEBUG] SCast Details: float to int (signed) -> fptosi. Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                cast_instr
            ) else (
                let cast_instr = L.build_fptoui source_val target_ll_ty "fptoui" builder in
                Printf.eprintf "      [DEBUG] SCast Details: float to int (unsigned) -> fptoui. Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                cast_instr
            )
        )
        else if is_int_sast_ty_local source_sast_ty_from_expr && is_float_sast_ty_local target_sast_ty then (
            Printf.eprintf "    [DEBUG] SCast: Path taken: int_to_float\n"; flush stderr;
            if is_signed_int_sast_ty_local source_sast_ty_from_expr then (
                let cast_instr = L.build_sitofp source_val target_ll_ty "sitofp" builder in
                Printf.eprintf "      [DEBUG] SCast Details: int to float (signed) -> sitofp. Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                cast_instr
            ) else (
                let cast_instr = L.build_uitofp source_val target_ll_ty "uitofp" builder in
                Printf.eprintf "      [DEBUG] SCast Details: int to float (unsigned) -> uitofp. Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                cast_instr
            )
        )
        else if is_int_sast_ty_local source_sast_ty_from_expr && is_int_sast_ty_local target_sast_ty then (
            Printf.eprintf "    [DEBUG] SCast: Path taken: int_to_int\n"; flush stderr;
            let source_bits = L.integer_bitwidth source_ll_ty in
            let target_bits = L.integer_bitwidth target_ll_ty in
            if target_bits > source_bits then (
                if is_signed_int_sast_ty_local source_sast_ty_from_expr then (
                    let cast_instr = L.build_sext source_val target_ll_ty "sext" builder in
                    Printf.eprintf "      [DEBUG] SCast Details: int to int (sext). Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                    cast_instr
                ) else (
                    let cast_instr = L.build_zext source_val target_ll_ty "zext" builder in
                    Printf.eprintf "      [DEBUG] SCast Details: int to int (zext). Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                    cast_instr
                )
            ) else ( (* target_bits <= source_bits *)
                let cast_instr = L.build_trunc source_val target_ll_ty "trunc" builder in
                Printf.eprintf "      [DEBUG] SCast Details: int to int (trunc). Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                cast_instr
            )
        )
        else if is_float_sast_ty_local source_sast_ty_from_expr && is_float_sast_ty_local target_sast_ty then (
             Printf.eprintf "    [DEBUG] SCast: Path taken: float_to_float\n"; flush stderr;
             let source_bits = if source_ll_ty = L.float_type context then 32 else 64 in
             let target_bits = if target_ll_ty = L.float_type context then 32 else 64 in
             if target_bits > source_bits then (
                let cast_instr = L.build_fpext source_val target_ll_ty "fpext" builder in
                Printf.eprintf "      [DEBUG] SCast Details: float to float (fpext). Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                cast_instr
             ) else ( (* target_bits <= source_bits *)
                let cast_instr = L.build_fptrunc source_val target_ll_ty "fptrunc" builder in
                Printf.eprintf "      [DEBUG] SCast Details: float to float (fptrunc). Result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
                cast_instr
             )
        ) else (
            Printf.eprintf "    [DEBUG] SCast: Path taken: Fallback to L.build_bitcast from %s to %s\n" (L.string_of_lltype source_ll_ty) (L.string_of_lltype target_ll_ty); flush stderr;
            let cast_instr = L.build_bitcast source_val target_ll_ty "bitcast" builder in
            Printf.eprintf "      [DEBUG] SCast Details: bitcast result LLVM Value: %s, LLVM Type: %s\n" (L.string_of_llvalue cast_instr) (L.string_of_lltype (L.type_of cast_instr)); flush stderr;
            cast_instr
        )

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
        | None -> ()
        )
    | SBlock stmts ->
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

        L.position_at_end then_bb builder;
        List.iter (build_stmt builder local_vars current_func_llval) then_stmts;
        if Option.is_none (L.block_terminator (L.insertion_block builder)) then
          ignore(L.build_br merge_bb builder);

        (match else_opt, else_bb_opt, else_builder_opt with
        | Some else_s, Some actual_else_bb, Some else_b ->
            L.position_at_end actual_else_bb else_b;
            build_stmt else_b local_vars current_func_llval else_s;
              if Option.is_none (L.block_terminator (L.insertion_block else_b)) then
                ignore(L.build_br merge_bb else_b);
        | _ -> ()
        );
        L.position_at_end merge_bb builder

    | SWhile (cond_se, body_stmts) ->
        let cond_bb = L.append_block context "while.cond" current_func_llval in
        let body_bb = L.append_block context "while.body" current_func_llval in
        let after_bb = L.append_block context "while.end" current_func_llval in
        loop_env := (cond_bb, after_bb) :: !loop_env;
        ignore(L.build_br cond_bb builder);
        L.position_at_end cond_bb builder;
        let cond_val = build_expr builder !local_vars current_func_llval cond_se in
        ignore(L.build_cond_br cond_val body_bb after_bb builder);
        L.position_at_end body_bb builder;
        List.iter (build_stmt builder local_vars current_func_llval) body_stmts;
        if Option.is_none (L.block_terminator (L.insertion_block builder)) then
            ignore(L.build_br cond_bb builder);
        loop_env := List.tl !loop_env;
        L.position_at_end after_bb builder

    | SFor (init_stmt_opt, cond_se_opt, update_se_opt, body_stmts) ->
        let loop_cond_bb = L.append_block context "for.cond" current_func_llval in
        let loop_body_bb = L.append_block context "for.body" current_func_llval in
        let loop_update_bb = L.append_block context "for.update" current_func_llval in
        let loop_after_bb = L.append_block context "for.end" current_func_llval in
        loop_env := (loop_update_bb, loop_after_bb) :: !loop_env;
        Option.iter (build_stmt builder local_vars current_func_llval) init_stmt_opt;
        ignore(L.build_br loop_cond_bb builder);
        L.position_at_end loop_cond_bb builder;
        (match cond_se_opt with
        | Some cond_se ->
            let cond_val = build_expr builder !local_vars current_func_llval cond_se in
            ignore(L.build_cond_br cond_val loop_body_bb loop_after_bb builder)
        | None -> ignore(L.build_br loop_body_bb builder)
        );
        L.position_at_end loop_body_bb builder;
        List.iter (build_stmt builder local_vars current_func_llval) body_stmts;
          if Option.is_none (L.block_terminator (L.insertion_block builder)) then
            ignore(L.build_br loop_update_bb builder);
        L.position_at_end loop_update_bb builder;
        Option.iter (fun se -> ignore(build_expr builder !local_vars current_func_llval se)) update_se_opt;
        if Option.is_none (L.block_terminator (L.insertion_block builder)) then
            ignore(L.build_br loop_cond_bb builder);
        loop_env := List.tl !loop_env;
        L.position_at_end loop_after_bb builder

    | SReturn se_list_opt ->
        (match se_list_opt, !current_func_return_type with
        | None, Some ret_ll_ty when L.classify_type ret_ll_ty = L.TypeKind.Void -> ignore(L.build_ret_void builder)
        | Some [se], Some _ ->
            let ret_val = build_expr builder !local_vars current_func_llval se in
            ignore(L.build_ret ret_val builder)
        | Some ses, Some ret_ll_ty when L.classify_type ret_ll_ty = L.TypeKind.Struct ->
            let ret_struct_alloca = L.build_alloca ret_ll_ty "ret_agg" builder in
            List.iteri (fun i se_ret ->
                let val_to_store = build_expr builder !local_vars current_func_llval se_ret in
                let field_ptr = L.build_struct_gep ret_struct_alloca i ("ret_field_" ^ string_of_int i) builder in
                ignore(L.build_store val_to_store field_ptr builder)
            ) ses;
            let loaded_ret_struct = L.build_load ret_struct_alloca "loaded_ret_agg" builder in
            ignore(L.build_ret loaded_ret_struct builder)
        | _, None -> failwith "IRGen: current_func_return_type not set"
        | Some _, Some ret_ll_ty -> failwith ("IRGen: Return statement mismatch. Expected single/void, got multi for LL type: " ^ (L.string_of_lltype ret_ll_ty))
        | None, Some ret_ll_ty -> failwith ("IRGen: Return statement mismatch. Expected value for non-void LL type: " ^ (L.string_of_lltype ret_ll_ty))
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

  let build_function_body (func_name: string) (f_llval: L.llvalue) (param_sast_decls: sparam list) (body_sast_stmts: sstmt list) (return_sast_types: ty list) =
    Printf.eprintf "  [DEBUG] build_function_body for %s: Function has %d params according to LLVM.\n" func_name (Array.length (L.params f_llval)); flush stderr;
    let builder = L.builder_at_end context (L.entry_block f_llval) in
    Printf.eprintf "  [DEBUG] build_function_body for %s: Builder created at entry block.\n" func_name; flush stderr;
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%lf\n" "fmt" builder
    and both_format_str = L.build_global_stringptr "\x1b[38;5;%d;48;5;%d;3;4m%s\x1b[0m" "fmt" builder
    and italic_format_str = L.build_global_stringptr "\x1b[38;5;%d;48;5;%d;3m%s\x1b[0m" "fmt" builder
    and underline_format_str = L.build_global_stringptr "\x1b[38;5;%d;48;5;%d;4m%s\x1b[0m" "fmt" builder
    and none_format_str = L.build_global_stringptr "\x1b[38;5;%d;48;5;%dm%s\x1b[0m" "fmt" builder in
    let tmp_map1 = (StringMap.add "fancy_fmt_both" both_format_str StringMap.empty) in
    let tmp_map2 = (StringMap.add "fancy_fmt_italic" italic_format_str tmp_map1) in
    let tmp_map3 = (StringMap.add "fancy_fmt_under" underline_format_str tmp_map2) in
    let tmp_map4 = (StringMap.add "fancy_fmt_none" none_format_str tmp_map3) in
    let local_vars_map : L.llvalue StringMap.t ref = ref (StringMap.add "float_format_str" float_format_str (StringMap.add "int_format_str" int_format_str tmp_map4)) in

    Printf.eprintf "  [DEBUG] build_function_body for %s: Setting current_func_return_type.\n" func_name; flush stderr;
    current_func_return_type := Some(
      match return_sast_types with
      | [] | [TyUnit] -> L.void_type context
      | [t] -> ltype_of_sast_ty t
      | ts -> ltype_of_sast_ty (TyTuple ts)
    );
    Printf.eprintf "  [DEBUG] build_function_body for %s: current_func_return_type set.\n" func_name; flush stderr;

    Printf.eprintf "  [DEBUG] build_function_body for %s: Allocating parameters (SAST count: %d)...\n" func_name (List.length param_sast_decls); flush stderr;
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
    Printf.eprintf "  [DEBUG] build_function_body for %s: Finished allocating parameters.\n" func_name; flush stderr;

    List.iter (build_stmt builder local_vars_map f_llval) body_sast_stmts;
    Printf.eprintf "  [DEBUG] build_function_body for %s: Finished building statements.\n" func_name; flush stderr;


    if Option.is_none (L.block_terminator (L.insertion_block builder)) then
      (match !current_func_return_type with
        | Some ll_ret_ty when L.classify_type ll_ret_ty = L.TypeKind.Void ->
            Printf.eprintf "  [DEBUG] build_function_body for %s: Adding implicit ret void.\n" func_name; flush stderr;
            ignore(L.build_ret_void builder)
        | Some _ ->
            Printf.eprintf "  [WARNING] build_function_body for %s: Non-void function might be missing a return. Adding unreachable.\n" func_name; flush stderr;
            ignore(L.build_unreachable builder)
        | None -> failwith ("IRGen internal error: current_func_return_type not set for " ^ func_name)
      );
    Printf.eprintf "  [DEBUG] build_function_body for %s: Terminator check complete.\n" func_name; flush stderr;
  in

  Printf.eprintf "IRGen: Building regular function bodies...\n"; flush stderr;
  List.iter (fun (fdecl : sfunc_decl) ->
    Printf.eprintf "IRGen: Building body for function %s...\n" fdecl.name; flush stderr;
    let f_llval = StringMap.find fdecl.name !function_decls in
    if fdecl.name = "main" then L.set_linkage L.Linkage.External f_llval;
    build_function_body fdecl.name f_llval fdecl.params fdecl.body fdecl.return_types;
    Printf.eprintf "IRGen: Finished body for function %s.\n" fdecl.name; flush stderr;
  ) sprogram.sp_funcs;
  Printf.eprintf "IRGen: Finished building regular function bodies.\n"; flush stderr;


  List.iter (fun (mdecl : sstruct_func) ->
    let mangled_name = mangle_method mdecl.struct_name mdecl.name in
    let m_llval = StringMap.find mangled_name !function_decls in
    let receiver_sparam = { name = mdecl.receiver_name; param_type = TyStruct mdecl.struct_name } in
    let all_sparams = receiver_sparam :: mdecl.params in
    build_function_body mangled_name m_llval all_sparams mdecl.body mdecl.return_types
  ) sprogram.sp_methods;

  Printf.eprintf "IRGen: About to verify module...\n"; flush stderr;
  (try Llvm_analysis.assert_valid_module the_module
   with Failure msg -> Printf.eprintf "LLVM Module Verification Failed: %s\n" msg; failwith "Module verification failed");
  Printf.eprintf "IRGen: Module verified successfully.\n"; flush stderr;

  the_module
;;
