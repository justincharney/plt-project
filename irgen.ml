(* irgen.ml *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (sprogram : sprogram) =
  let context    = L.global_context () in
  let the_module = L.create_module context "P.A.T." in

  (* Return the LLVM type for a P.A.T type *)
  let rec ltype_of_typ = function
    | SPrimitive p ->
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
          | String -> L.pointer_type (L.i8_type context)
      )
    | SArray(t, n) -> L.array_type (ltype_of_typ t) n
    (* | SSlice t -> 
        let ptr = L.pointer_type (ltype_of_typ t) in
        L.struct_type context [| ptr; L.i64_type context |] *)
    | STypeName name ->
        try StringMap.find name struct_types
        with Not_found -> failwith ("Unknown type: " ^ name)
  and
  (* Type handling *)
  struct_types : L.llvalue StringMap.t = 
    let global_struct m (n, tl) = 
      let name_struct = L.named_struct_type context n in 
      ignore(L.struct_set_body name_struct (Array.of_list (List.map (fun (_,t)->ltype_of_typ t) tl)) false);
      StringMap.add n name_struct m in 
    List.fold_left global_struct StringMap.empty sprogram.sp_structs in 

  (* Built-in functions: make(), len(), cap(), printf(), sprintf(), assert(), exit(), append() *)
  let printf_t    : L.lltype  = L.var_arg_function_type (L.i32_type context) [| L.pointer_type (L.i8_type context) |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in
  let exit_t      = L.function_type (L.void_type context) [| L.i32_type context |] in
  let exit_func   = L.declare_function "exit" exit_t the_module in

  let function_decls : (L.llvalue * sfunc) StringMap.t = 
    let func_decl m fdecl = 
      let ftype = L.function_type (ltype_of_typ fdecl.sf_return_types) (Array.of_list (List.map (fun s->ltype_of_typ s.sp_type) fdecl.sf_params)) in
      StringMap.add fdecl.sf_name (L.define_function fdecl.sf_name ftype the_module, fdecl) m in
    List.fold_left func_decl StringMap.empty sprogram.sp_functions in

  let build_function_body fdecl = 
    let (the_function,_) = StringMap.find fdecl.sf_name function_decls in 
    let builder = L.builder_at_end context (L.entry_block the_function) in 
    
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
