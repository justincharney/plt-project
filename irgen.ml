(* irgen.ml *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (sprogram : sprogram) =
  let context    = L.global_context () in
  let the_module = L.create_module context "P.A.T." in

  let slice_def_list,alias_def_list = 
    List.fold_right (fun spt (s_acc,a_acc) ->
      match spt with
        STypeStruct (n, sfields) -> ((STypeStruct (n,sfields))::s_acc, a_acc)
      | STypeAlias (n, t) ->  (s_acc, (STypeAlias (n,t))::a_acc)
    ) sprogram.sp_types ([],[]) in

  let string_struct = L.named_struct_type context "String" in 
  L.struct_set_body string_struct [| L.pointer_type (L.i8_type context); (L.i64_type context) |] false;

  (* Return the LLVM type for a P.A.T type *)
  let rec ltype_of_typ : L.lltype = function
    | TyPrim p ->
      (
        match p with
          | I8     -> L.i8_type      context
          | I16    -> L.i16_type     context
          | I32    -> L.i32_type     context
          | I64    -> L.i64_type     context
          | U8     -> L.i8_type      context
          | U16    -> L.i16_type     context
          | U32    -> L.i32_type     context
          | U64    -> L.i64_type     context
          | F32    -> L.float_type   context
          | F64    -> L.double_type  context
          | Bool   -> L.i1_type      context
          | String -> string_struct
      )
    | TyArray(t, n) -> L.array_type (ltype_of_typ t) n
    (* | SSlice t -> 
        let ptr = L.pointer_type (ltype_of_typ t) in
        L.struct_type context [| ptr; L.i64_type context |] *)
    | TyStruct name ->
        try StringMap.find name struct_types
        with Not_found -> failwith ("Unknown type: " ^ name)
    | TyUnit -> L.void_type context
    | TyError -> string_struct
    | _ -> raise Failure "type not supported in IR"

  and
  (* Type handling *)
  struct_types : L.llvalue StringMap.t = 
    let global_struct m (n, tl) = 
      let name_struct = L.named_struct_type context n in 
      ignore(L.struct_set_body name_struct (Array.of_list (List.map (fun t->ltype_of_typ t.field_type) tl)) false);
      StringMap.add n name_struct m in 
    List.fold_left global_struct StringMap.empty slice_def_list in 

  let alias_types : L.llvalue StringMap.t = 
    let global_alias m (n,t) = StringMap.add n t m in 
    List.fold_left global_alias StringMap.empty alias_def_list in 

  let global_vars : L.llvalue StringMap.t = 
    let global_var m decl = 
      let init = L.const_null (ltype_of_typ decl.var_type) in 
      let global_val = L.define_global decl.name init the_module in 
      L.set_global_constant decl.is_const global_val;
      StringMap.add decl.name global_val m in 
    List.fold_left global_var StringMap.empty sprogram.sp_globals in 

  (* Built-in functions: make(), len(), cap(), printf(), sprintf(), assert(), exit(), append() *)
  let printf_t    : L.lltype  = L.var_arg_function_type (L.i32_type context) [| L.pointer_type (L.i8_type context) |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in
  let exit_t      = L.function_type (L.void_type context) [| L.i32_type context |] in
  let exit_func   = L.declare_function "exit" exit_t the_module in
  (* let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in *)

  let function_decls : (L.llvalue * sfunc) StringMap.t = 
    let func_decl m fdecl = 
      let ftype = L.function_type (ltype_of_typ (List.hd fdecl.return_types)) (Array.of_list (List.map (fun s->ltype_of_typ s.param_type) fdecl.params)) in
      StringMap.add fdecl.name (L.define_function fdecl.name ftype the_module, fdecl) m in
    List.fold_left func_decl StringMap.empty sprogram.sp_funcs in

  let build_function_body fdecl = 
    let (the_function,_) = StringMap.find fdecl.name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in 
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

    let local_vars : L.llvalue StringMap.t = 
      let add_formal m sp p = L.set_value_name sp.name p; 
        let local = L.build_alloca (ltype_of_typ sp.param_type) sp.name builder in 
        ignore(l.build_store p local builder);
        StringMap.add sp.name local m in 
      List.fold_left2 add_formal StringMap.empty fdecl.params (Array.to_list (L.params the_function)) in

    let add_local_var m v_decl = 
      let l_var = L.build_alloca (ltype_of_typ v_decl.var_type) v_decl.name in 
      StringMap.add v_decl.name l_var m in 

    let lookup n m = try StringMap.find n m 
      with Not_found -> StringMap.find n global_var in
    
    let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore(instr builder) in

    let rec build_stmt builder = function in

    let func_builder = build_stmt builder fdecl.sf_body in
    add_terminal func_builder (L.build_ret (L.const_int (L.i32_type context) 0))
  in
  List.iter build_function_body sprogram.sp_functions;

  the_module
