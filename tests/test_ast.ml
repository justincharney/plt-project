open OUnit2
open Ast


let ast_construction_tests = "test suite for AST construction and structure" >::: [

  "create and verify literals" >:: (fun _ ->
    assert_equal (IntLit 10) (IntLit 10);
    assert_equal (FloatLit 3.14) (FloatLit 3.14);
    assert_equal (BoolLit true) (BoolLit true);
    assert_equal (StringLit "hello") (StringLit "hello");
    assert_equal (CharLit 'a') (CharLit 'a');
    assert_equal Null Null;

    (* Verify content with pattern matching *)
    (match IntLit 42 with IntLit i -> assert_equal 42 i | _ -> assert_failure "Expected IntLit");
    (match StringLit "test" with StringLit s -> assert_equal "test" s | _ -> assert_failure "Expected StringLit");
  );

  "create and verify type expressions" >:: (fun _ ->
    let t_bool = Primitive Bool in
    let t_str_arr = Array (Primitive String, 10) in
    let t_i32_slice = Slice (Primitive I32) in
    let t_struct = Struct "MyStruct" in
    let t_alias = TypeName "MyIntAlias" in
    let t_error = Primitive Error in

    assert_equal (Primitive Bool) t_bool;
    assert_equal (Array (Primitive String, 10)) t_str_arr;
    assert_equal (Slice (Primitive I32)) t_i32_slice;
    assert_equal (Struct "MyStruct") t_struct;
    assert_equal (TypeName "MyIntAlias") t_alias;
    assert_equal (Primitive Error) t_error;

    (* Verify structure *)
    (match t_str_arr with
     | Array (Primitive String, sz) -> assert_equal 10 sz
     | _ -> assert_failure "Expected Array(Primitive String, 10)");
    (match t_i32_slice with
     | Slice (Primitive I32) -> ()
     | _ -> assert_failure "Expected Slice(Primitive I32)");
  );

  "create and verify basic expressions" >:: (fun _ ->
    let ident = Identifier "x" in
    let binop = Binop (IntLit 1, Plus, IntLit 2) in
    let unaop = Unaop (Neg, Identifier "y") in
    let assign = SimpleAssign (Identifier "x", IntLit 5) in
    let comp_assign = CompoundAssign (Identifier "count", PlusAssign, IntLit 1) in
    let field_acc = FieldAccess (Identifier "myStructVar", "fieldName") in
    let index_acc = IndexAccess (Identifier "myArray", IntLit 0) in
    let slice_expr = SliceExpr (Identifier "mySlice", Some (IntLit 1), Some (IntLit 5)) in
    let slice_expr_no_end = SliceExpr (Identifier "mySlice", Some (IntLit 2), None) in
    let slice_expr_no_start = SliceExpr (Identifier "mySlice", None, Some (IntLit 3)) in
    let func_call = FunctionCall ("myFunc", [IntLit 1; BoolLit false]) in
    let method_call = MethodCall (Identifier "myObj", "doThing", [StringLit "arg"]) in
    let cast_expr = Cast (Primitive I16, Identifier "long_val") in
    let make_expr = Make (Slice (Primitive I32), IntLit 10, None) in
    let make_expr_cap = Make (Slice (Primitive U8), IntLit 5, Some (IntLit 10)) in
    let seq_expr = Sequence (SimpleAssign (Identifier "a", IntLit 1), Identifier "a") in
    let arr_lit = ArrayLit (Primitive I32, [IntLit 1; IntLit 2]) in
    let struct_lit = StructLit ("Point", [("x", IntLit 0); ("y", IntLit 0)]) in
    let slice_lit = SliceLit (Primitive F32, [FloatLit 1.0; FloatLit 2.0]) in


    (* Verify identifiers *)
    (match ident with Identifier name -> assert_equal "x" name | _ -> assert_failure "Expected Identifier");

    (* Verify Binop *)
    (match binop with
     | Binop (IntLit l, Plus, IntLit r) -> assert_equal 1 l; assert_equal 2 r
     | _ -> assert_failure "Expected Binop(IntLit 1, Plus, IntLit 2)");

    (* Verify Unaop *)
     (match unaop with
     | Unaop (Neg, Identifier name) -> assert_equal "y" name
     | _ -> assert_failure "Expected Unaop(Neg, Identifier \"y\")");

    (* Verify Assignment *)
    (match assign with
      | SimpleAssign(Identifier i, IntLit v) -> assert_equal "x" i; assert_equal 5 v
      | _ -> assert_failure "Expected SimpleAssign" );
    (match comp_assign with
      | CompoundAssign(Identifier i, PlusAssign, IntLit v) -> assert_equal "count" i; assert_equal 1 v
      | _ -> assert_failure "Expected CompoundAssign");

    (* Verify Access *)
    (match field_acc with
      | FieldAccess(Identifier s, f) -> assert_equal "myStructVar" s; assert_equal "fieldName" f
      | _ -> assert_failure "Expected FieldAccess");
    (match index_acc with
      | IndexAccess(Identifier a, IntLit idx) -> assert_equal "myArray" a; assert_equal 0 idx
      | _ -> assert_failure "Expected IndexAccess");

    (* Verify Slice Expression *)
     (match slice_expr with
      | SliceExpr(Identifier s, Some (IntLit start), Some (IntLit end_)) ->
          assert_equal "mySlice" s; assert_equal 1 start; assert_equal 5 end_
      | _ -> assert_failure "Expected SliceExpr with start and end");
     (match slice_expr_no_end with
      | SliceExpr(Identifier s, Some (IntLit start), None) ->
          assert_equal "mySlice" s; assert_equal 2 start
      | _ -> assert_failure "Expected SliceExpr with start only");
    (match slice_expr_no_start with
      | SliceExpr(Identifier s, None, Some (IntLit end_)) ->
          assert_equal "mySlice" s; assert_equal 3 end_
      | _ -> assert_failure "Expected SliceExpr with end only");

    (* Verify Calls *)
    (match func_call with
      | FunctionCall(name, args) -> assert_equal "myFunc" name; assert_equal 2 (List.length args)
      | _ -> assert_failure "Expected FunctionCall");
    (match method_call with
      | MethodCall(Identifier obj, name, args) -> assert_equal "myObj" obj; assert_equal "doThing" name; assert_equal 1 (List.length args)
      | _ -> assert_failure "Expected MethodCall");

    (* Verify Cast *)
    (match cast_expr with
      | Cast(Primitive I16, Identifier v) -> assert_equal "long_val" v
      | _ -> assert_failure "Expected Cast");

    (* Verify Make *)
    (match make_expr with
      | Make(Slice(Primitive I32), IntLit len, None) -> assert_equal 10 len
      | _ -> assert_failure "Expected Make slice without capacity");
     (match make_expr_cap with
      | Make(Slice(Primitive U8), IntLit len, Some(IntLit cap)) -> assert_equal 5 len; assert_equal 10 cap
      | _ -> assert_failure "Expected Make slice with capacity");

    (* Verify Sequence *)
    (match seq_expr with
      | Sequence(SimpleAssign _, Identifier i) -> assert_equal "a" i
      | _ -> assert_failure "Expected Sequence");

    (* Verify Literals *)
    (match arr_lit with
      | ArrayLit(Primitive I32, elems) -> assert_equal 2 (List.length elems)
      | _ -> assert_failure "Expected ArrayLit");
    (match struct_lit with
      | StructLit("Point", fields) -> assert_equal 2 (List.length fields); assert_equal "x" (fst (List.hd fields))
      | _ -> assert_failure "Expected StructLit");
    (match slice_lit with
      | SliceLit(Primitive F32, elems) -> assert_equal 2 (List.length elems)
      | _ -> assert_failure "Expected SliceLit");
  );

  "create and verify statements" >:: (fun _ ->
    let expr_stmt = Expr (FunctionCall ("doSideEffect", [])) in
    let block_stmt = Block [Expr (IntLit 1); Expr (IntLit 2)] in
    let if_stmt = IfStmt (BoolLit true, [Expr (IntLit 1)], Some (Block [Expr (IntLit 0)])) in
    let if_no_else = IfStmt (Identifier "cond", [Expr (IntLit 1)], None) in
    let for_stmt = ForStmt (None, Some (BoolLit true), None, [Expr (IntLit 1)]) in
    let for_full = ForStmt(
        Some (Expr (SimpleAssign(Identifier "i", IntLit 0))),
        Some (Binop(Identifier "i", Lt, IntLit 10)),
        Some (CompoundAssign(Identifier "i", PlusAssign, IntLit 1)),
        [Expr (Identifier "i")]
      ) in
    let while_stmt = WhileStmt (Identifier "running", [Expr (IntLit 1)]) in
    let ret_stmt = Return (Some [Identifier "result"]) in
    let ret_empty = Return None in
    let break_stmt = Break in
    let cont_stmt = Continue in
    let var_decl1 = VarDecl { is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=Some (IntLit 10)} in (* var x i32 = 10 *)
    let var_decl2 = VarDecl { is_const=false; name="y"; var_type=None; initializer_expr=Some (FloatLit 1.0)} in (* y := 1.0 *)
    let const_decl = VarDecl { is_const=true; name="MAX_VAL"; var_type=Some (Primitive U64); initializer_expr=Some (IntLit 100)} in (* const MAX_VAL u64 = 100 *)
    let var_decl_no_init = VarDecl { is_const=false; name="z"; var_type=Some (Primitive Bool); initializer_expr=None} in (* var z bool *)

    (match for_full with
      | ForStmt (
          Some (Expr (SimpleAssign(Identifier i_init, IntLit v_init))),
          Some (Binop(Identifier i_cond, Lt, IntLit v_cond)),
          Some (CompoundAssign(Identifier i_upd, PlusAssign, IntLit v_upd)),
          [Expr (Identifier i_body)]
        ) ->
          assert_equal "i" i_init; assert_equal 0 v_init;
          assert_equal "i" i_cond; assert_equal 10 v_cond;
          assert_equal "i" i_upd; assert_equal 1 v_upd;
          assert_equal "i" i_body
      | _ -> assert_failure "Expected structure for for_full not matched");

    (* Verify specific statement structures *)
    (match expr_stmt with Expr(FunctionCall (n, _)) -> assert_equal "doSideEffect" n | _ -> assert_failure "Expected Expr statement");
    (match block_stmt with Block stmts -> assert_equal 2 (List.length stmts) | _ -> assert_failure "Expected Block");
    (match if_stmt with IfStmt(BoolLit b, _, Some (Block _)) -> assert_equal true b | _ -> assert_failure "Expected IfStmt");
    (match if_no_else with IfStmt(Identifier i, _, None) -> assert_equal "cond" i | _ -> assert_failure "Expected IfStmt with no else");
    (match for_stmt with ForStmt(None, Some(BoolLit b), None, _) -> assert_equal true b | _ -> assert_failure "Expected ForStmt");
    (match while_stmt with WhileStmt(Identifier i, _) -> assert_equal "running" i | _ -> assert_failure "Expected WhileStmt");
    (match ret_stmt with Return (Some [Identifier i]) -> assert_equal "result" i | _ -> assert_failure "Expected Return statement");
    (match ret_empty with Return None -> () | _ -> assert_failure "Expected empty Return");
    assert_equal Break break_stmt;
    assert_equal Continue cont_stmt;

    (* Verify VarDecl fields *)
    (match var_decl1 with
         | VarDecl r ->
            assert_equal false r.is_const; assert_equal "x" r.name;
            assert_equal (Some (Primitive I32)) r.var_type;
            assert_equal (Some (IntLit 10)) r.initializer_expr
         | _ -> assert_failure "Expected var_decl1 to be VarDecl");

        (match var_decl2 with
         | VarDecl r ->
            assert_equal false r.is_const; assert_equal "y" r.name;
            assert_equal None r.var_type;
            assert_equal (Some (FloatLit 1.0)) r.initializer_expr
         | _ -> assert_failure "Expected var_decl2 to be VarDecl");

        (match const_decl with
         | VarDecl r ->
            assert_equal true r.is_const; assert_equal "MAX_VAL" r.name;
            assert_equal (Some (Primitive U64)) r.var_type;
            assert_equal (Some (IntLit 100)) r.initializer_expr
         | _ -> assert_failure "Expected const_decl to be VarDecl");

        (match var_decl_no_init with
         | VarDecl r ->
            assert_equal false r.is_const; assert_equal "z" r.name;
            assert_equal (Some (Primitive Bool)) r.var_type;
            assert_equal None r.initializer_expr
         | _ -> assert_failure "Expected var_decl_no_init to be VarDecl");
  );

  "create and verify top-level declarations" >:: (fun _ ->
    let simple_field = { name="id"; field_type=Primitive I32; modifier=None; default_value=None } in
    let mod_field = { name="status"; field_type=Primitive String; modifier=Some Mutable; default_value=Some (StringLit "init") } in

    (* Test type_decl constructors *)
    let struct_decl: type_decl = TypeStruct ("MyStruct", [simple_field; mod_field]) in
    let alias_decl: type_decl = TypeAlias ("UserID", Primitive I32) in

    (* Test param record *)
    let simple_param = { name="input"; param_type=Primitive String } in

    (* Test func_decl record *)
    let function_decl: func_decl = {
      name="process";
      params=[simple_param];
      return_types=[Primitive Bool];
      body=[Return (Some [BoolLit true])]
    } in

     (* Test struct_func record *)
    let struct_func_decl: struct_func = {
      name = "getStatus";
      struct_name = "MyStruct";
      params = [];
      return_types = [Primitive String];
      body = [Return (Some [FieldAccess(Identifier "self", "status")])] (* Assuming 'self' convention *)
    } in

    (* Test global_decl record *)
    let global_var: global_decl = {
      is_const=true;
      name="PI";
      var_type=Some (Primitive F32);
      initializer_expr=Some (FloatLit 3.14159)
    } in

    (* Verify Type Decls *)
    (match struct_decl with
     | TypeStruct(name, fields) -> assert_equal "MyStruct" name; assert_equal 2 (List.length fields);
        let f1 = List.nth fields 0 in
        let f2 = List.nth fields 1 in
        assert_equal "id" f1.name; assert_equal None f1.modifier; assert_equal None f1.default_value;
        assert_equal "status" f2.name; assert_equal (Some Mutable) f2.modifier; assert_equal (Some (StringLit "init")) f2.default_value
     | _ -> assert_failure "Expected TypeStruct");
    (match alias_decl with
     | TypeAlias(name, Primitive I32) -> assert_equal "UserID" name
     | _ -> assert_failure "Expected TypeAlias");

    (* Verify Function Decl *)
    assert_equal "process" function_decl.name;
    assert_equal 1 (List.length function_decl.params);
    assert_equal "input" (List.hd function_decl.params).name;
    assert_equal (Primitive String) (List.hd function_decl.params).param_type;
    assert_equal [Primitive Bool] function_decl.return_types;
    assert_equal 1 (List.length function_decl.body);

    (* Verify Struct Function Decl *)
    assert_equal "getStatus" struct_func_decl.name;
    assert_equal "MyStruct" struct_func_decl.struct_name;
    assert_equal 0 (List.length struct_func_decl.params);
    assert_equal [Primitive String] struct_func_decl.return_types;
    assert_equal 1 (List.length struct_func_decl.body);

    (* Verify Global Var Decl *)
    assert_equal true global_var.is_const;
    assert_equal "PI" global_var.name;
    assert_equal (Some (Primitive F32)) global_var.var_type;
    (match global_var.initializer_expr with Some (FloatLit f) -> assert_equal 3.14159 f | _ -> assert_failure "Expected FloatLit initializer");
  );

  "create and verify program structure" >:: (fun _ ->
    let program_struct: program = {
      package_name = "main";
      imports = ["fmt"; "math"];
      type_declarations = [TypeAlias ("Count", Primitive I32)];
      global_vars = [{
        is_const=true;
        name="Version";
        var_type=Some(Primitive String);
        initializer_expr=Some(StringLit "1.0")
      }];
      functions = [{
        name="main";
        params=[];
        return_types=[];
        body=[]
      }];
      struct_functions = [{ (* Added struct func *)
          name = "helper";
          struct_name = "Util";
          params = []; return_types = []; body = []
      }];
    } in

    assert_equal "main" program_struct.package_name;
    assert_equal ["fmt"; "math"] program_struct.imports;
    assert_equal 1 (List.length program_struct.type_declarations);
    assert_equal 1 (List.length program_struct.global_vars);
    assert_equal "Version" (List.hd program_struct.global_vars).name;
    assert_equal 1 (List.length program_struct.functions);
    assert_equal "main" (List.hd program_struct.functions).name;
    assert_equal 1 (List.length program_struct.struct_functions);
    assert_equal "helper" (List.hd program_struct.struct_functions).name;
  )

]

(* Run test *)
let () = run_test_tt_main ast_construction_tests
