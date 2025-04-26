(* irgen.ml *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (sprogram : sprogram) =
  let context    = L.global_context () in
  let the_module = L.create_module context "P.A.T." in
  let builder    = L.builder context in

  (* -------------------------------------------------------------------------------- *)
  (* START: KEVIN REVIEW *)

  (* Type handling *)
  let struct_types  = Hashtbl.create 10 in
  let struct_fields = Hashtbl.create 10 in

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
    | SSlice t -> 
        let ptr = L.pointer_type (ltype_of_typ t) in
        L.struct_type context [| ptr; L.i64_type context |]
    | STypeName name ->
        try Hashtbl.find struct_types name 
        with Not_found -> failwith ("Unknown type: " ^ name)
  in

  (* Built-in functions: make(), len(), cap(), printf(), sprintf(), assert(), exit(), append() *)
  let printf_t    = L.var_arg_function_type (L.i32_type context) [| L.pointer_type (L.i8_type context) |]
  let printf_func = L.declare_function "printf" printf_t the_module
  let malloc_t    = L.function_type (L.pointer_type (L.i8_type context)) [| L.i64_type context |]
  let malloc_func = L.declare_function "malloc" malloc_t the_module
  let exit_t      = L.function_type (L.void_type context) [| L.i32_type context |]
  let exit_func   = L.declare_function "exit" exit_t the_module

  (* let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in *)

  (* END: KEVIN REVIEW *)
  (* -------------------------------------------------------------------------------- *)

  the_module
