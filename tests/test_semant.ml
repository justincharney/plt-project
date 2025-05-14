open OUnit2
open Ast
open Semant (* The module we are testing *)

(* Note: Tests in this file must have statements in reverse order compared to
   natural code flow, because our semantic analyzer processes statements
   in source code order (after reversing the AST's reversed order). *)

(* Helper to create a minimal valid program structure *)
let make_program ?(imports=[]) ?(types=[]) ?(globals=[]) ?(functions=[]) ?(struct_funcs=[]) () =
  {
    package_name = "main";
    imports = imports;
    type_declarations = types;
    global_vars = globals;
    functions = functions;
    struct_functions = struct_funcs;
  }

(* Helper function to run the semantic check and assert success *)
let run_semant_test_pass program _ =
  try
    let _ = Semant.check_program program in (* We only care that it doesn't raise *)
    assert_bool "Semantic check passed as expected." true
  with
  | Semantic_error msg -> assert_failure ("Semantic check failed unexpectedly: " ^ msg)
  | ex -> assert_failure ("An unexpected exception occurred: " ^ Printexc.to_string ex)

let run_semant_test_fail program _ =
  try
    let _ = Semant.check_program program in
    assert_failure "Expected the semantic checker to raise Semantic_error"
  with
  | Semantic_error _ -> () (* any message is fine *)
  | ex -> assert_failure (* wrong kind of exn *)
    ("Expected Semantic_error but got "
    ^ Printexc.to_string ex)

(* --- Test Cases --- *)

let basic_tests = "Basic Valid Programs" >::: [
  "empty main" >:: run_semant_test_pass (make_program ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }] ());

  "global variable declaration (inferred I32)" >:: run_semant_test_pass (make_program
    ~globals:[{ is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 10) }]
    ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
    ()
  );

  "global variable declaration (explicit I32)" >:: run_semant_test_pass (make_program
    ~globals:[{ is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=Some (Cast(Primitive I32, IntLit 10)) }]
    ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
    ()
  );

  "simple function call" >:: run_semant_test_pass (make_program
    ~functions:[
      { name="foo"; params=[]; return_types=[]; body=[] };
      { name="main"; params=[]; return_types=[]; body=[Expr (FunctionCall("foo", []))] }
    ]
    ()
  );

   "simple return I32" >:: run_semant_test_pass (make_program
    ~functions:[{ name="main"; params=[]; return_types=[Primitive I32]; body=[Return (Some [Cast(Primitive I32, IntLit 42)])] }]
    ()
   );

   "simple return U32" >:: run_semant_test_pass (make_program
    ~functions:[{ name="main"; params=[]; return_types=[Primitive U32]; body=[Return (Some [Cast(Primitive U32, IntLit 42)])] }]
    ()
  );
]

let type_tests = "Type Checking and Resolution" >::: [
    "valid assignment I32" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "x", Cast(Primitive I32, IntLit 5)));
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive I32);
            initializer_expr=None};
        ]}]
        ()
    );

    "valid inferred assignment (Float)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "x", FloatLit 2.71));
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some (FloatLit 3.14)};
        ]}]
        ()
    );

    "type alias usage I32" >:: run_semant_test_pass (make_program
        ~types:[TypeAlias("MyInt", Primitive I32)]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "x", Cast(Primitive I32, IntLit 2)));
            VarDecl {is_const=false; name="x"; var_type=Some (TypeName "MyInt"); initializer_expr=Some (Cast(Primitive I32, IntLit 1))};
        ]}]
        ()
    );

    "array literal type check Bool" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (ArrayLit (Primitive Bool, [BoolLit true; BoolLit false]))
        ]}]
        ()
    );

    "slice literal type check U8 (char)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SliceLit (Primitive U8, [CharLit 'a'; CharLit 'b'])) (* Using CharLit for U8 *)
        ]}]
        ()
    );

     "slice literal type check I32" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SliceLit (Primitive I32, [Cast(Primitive I32, IntLit 1); Cast(Primitive I32, IntLit 2)]))
        ]}]
        ()
    );

    "valid cast I32 to I64" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Cast(Primitive I64, Identifier "x"));
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 10)}; (* x is I32 *)
        ]}]
        ()
    );

    "null assignment to slice" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "s", Null));
            VarDecl {is_const=false; name="s"; var_type=Some(Slice(Primitive I32)); initializer_expr=None};
        ]}]
        ()
     );
]

let expression_tests = "Expression Checking" >::: [
    "valid binary operation (+) I32 (default)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(IntLit 1, Plus, IntLit 2))
        ]}]
        ()
    );
     "valid binary operation (+) I32 (explicit)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(Cast(Primitive I32, IntLit 1), Plus, Cast(Primitive I32, IntLit 2)))
        ]}]
        ()
    );
    "valid binary operation (+) U32 (explicit)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(Cast(Primitive U32, IntLit 1), Plus, Cast(Primitive U32, IntLit 2)))
        ]}]
        ()
    );
    "valid binary operation (==) Float" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(FloatLit 1.0, Eq, FloatLit 2.0))
        ]}]
        ()
    );
    "valid binary operation (&&) Bool" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(BoolLit true, And, BoolLit false))
        ]}]
        ()
    );
    "valid unary operation (-) I32 (default)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Neg, IntLit 5))
        ]}]
        ()
    );
    "valid unary operation (-) U32 (explicit)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Neg, Cast(Primitive U32, IntLit 5)))
        ]}]
        ()
    );
    "valid unary operation (!) Bool" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Not, BoolLit true))
        ]}]
        ()
    );
    "valid compound assignment I32 (default)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (CompoundAssign(Identifier "x", PlusAssign, IntLit 5));
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 10)};
        ]}]
        ()
    );
     "valid compound assignment I32 (explicit)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (CompoundAssign(Identifier "x", PlusAssign, Cast(Primitive I32, IntLit 5)));
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=Some(Cast(Primitive I32, IntLit 10))};
        ]}]
        ()
    );
     "valid compound assignment U32 (explicit)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (CompoundAssign(Identifier "x", PlusAssign, Cast(Primitive U32, IntLit 5)));
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive U32); initializer_expr=Some(Cast(Primitive U32, IntLit 10))};
        ]}]
        ()
    );
    "valid sequence" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Sequence(IntLit 1, IntLit 2)) (* Now I32 sequence, still valid *)
        ]}]
        ()
    );
    "valid index access I32 array" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (IndexAccess(Identifier "a", IntLit 0)); (* Index is I32, allowed *)
            VarDecl {is_const=false; name="a"; var_type=None; initializer_expr=Some(ArrayLit(Primitive I32, [Cast(Primitive I32, IntLit 1)]))};
        ]}]
        ()
    );
     "valid slice expression I32 array" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SliceExpr(Identifier "a", Some (IntLit 0), Some (IntLit 1))); (* Indices are I32, allowed *)
            VarDecl {is_const=false; name="a"; var_type=None; initializer_expr=Some(ArrayLit(Primitive I32, [Cast(Primitive I32, IntLit 1); Cast(Primitive I32, IntLit 2)]))};
        ]}]
        ()
    );
    "valid make" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr(Make(Slice(Primitive U8), IntLit 10, Some (IntLit 20))) (* Length/Cap are I32, allowed *)
        ]}]
        ()
    );
]

let statement_tests = "Statement Checking" >::: [
    "valid if statement" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            IfStmt(BoolLit true, [Expr(IntLit 1)], Some (Block [Expr(IntLit 2)])) (* Uses I32 now, fine *)
        ]}]
        ()
    );
    "valid while statement" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            WhileStmt(BoolLit false, [Expr(IntLit 1)]) (* Uses I32 now, fine *)
        ]}]
        ()
    );
    "valid for statement (full)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            ForStmt(
                Some (VarDecl {is_const=false; name="i"; var_type=None; initializer_expr=Some(IntLit 0)}), (* i is I32 *)
                Some (Binop(Identifier "i", Lt, IntLit 10)),
                Some (CompoundAssign(Identifier "i", PlusAssign, IntLit 1)),
                [Expr(Identifier "i")]
            )
        ]}]
        ()
    );
    "valid for statement (condition only)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            ForStmt( None, Some (BoolLit true), None, [Break] )
        ]}]
        ()
    );
    "valid return (multiple values I32)" >:: run_semant_test_pass (make_program
        ~functions:[{
            name="swap";
            params=[{name="a"; param_type=Primitive I32}; {name="b"; param_type=Primitive I32}];
            return_types=[Primitive I32; Primitive I32];
            body=[Return(Some [Identifier "b"; Identifier "a"])]
        }; { name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
     "valid block scope" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             (* x is not visible here *)
            VarDecl {is_const=false; name="y"; var_type=None; initializer_expr=Some(IntLit 2)}; (* y is I32 *)
             Block [
                VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)} (* x is I32 *)
            ];
        ]}]
        ()
    );
]

let struct_method_tests = "Structs and Methods" >::: [
    "define simple struct" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "struct literal creation I32" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Point", [
                {name="x"; field_type=Primitive I32; modifier=None; default_value=None};
                {name="y"; field_type=Primitive I32; modifier=None; default_value=None}
            ])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("Point", [("x", Cast(Primitive I32, IntLit 1)); ("y", Cast(Primitive I32, IntLit 2))]))
        ]}]
        ()
    );
    "struct field access I32" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (FieldAccess(Identifier "p", "x"));
            VarDecl {is_const=false; name="p"; var_type=None; initializer_expr=Some(StructLit("Point", [("x", Cast(Primitive I32, IntLit 1))]))};
        ]}]
        ()
    );
    "simple method definition I32" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Counter", [{name="val"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~struct_funcs:[{
            name="get"; struct_name="Counter"; params=[]; return_types=[Primitive I32];
            body=[Return(Some [FieldAccess(Identifier "self", "val")])] (* Assuming self convention *)
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "simple method call I32" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Counter", [{name="val"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~struct_funcs:[{
            name="get"; struct_name="Counter"; params=[]; return_types=[Primitive I32];
            body=[Return(Some [FieldAccess(Identifier "self", "val")])]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (MethodCall(Identifier "c", "get", []));
            VarDecl {is_const=false; name="c"; var_type=None; initializer_expr=Some(StructLit("Counter", [("val", Cast(Primitive I32, IntLit 5))]))};
        ]}]
        ()
    );
     "method with params I32" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Adder", [])]
        ~struct_funcs:[{
            name="add"; struct_name="Adder";
            params=[{name="a"; param_type=Primitive I32}; {name="b"; param_type=Primitive I32}];
            return_types=[Primitive I32];
            body=[Return(Some [Binop(Identifier "a", Plus, Identifier "b")])]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (MethodCall(Identifier "ad", "add", [Cast(Primitive I32, IntLit 3); Cast(Primitive I32, IntLit 4)]));
            VarDecl {is_const=false; name="ad"; var_type=None; initializer_expr=Some(StructLit("Adder", []))};
        ]}]
        ()
    );
     "null assignment to struct" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "p", Null));
            VarDecl {is_const=false; name="p"; var_type=Some(Struct("Point")); initializer_expr=None};
        ]}]
        ()
     );
]

(* --- Failure Cases --- *)

let error_tests_declarations = "Error Cases: Declarations" >::: [
    "duplicate global var" >:: run_semant_test_fail (make_program
        ~globals:[
            {is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 1)};
            {is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 2)}
        ]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "duplicate function" >:: run_semant_test_fail (make_program
        ~functions:[
            { name="foo"; params=[]; return_types=[]; body=[] };
            { name="foo"; params=[]; return_types=[]; body=[] };
            { name="main"; params=[]; return_types=[]; body=[] }
        ]
        ()
    );
    "duplicate type alias" >:: run_semant_test_fail (make_program
        ~types:[TypeAlias("MyInt", Primitive I32); TypeAlias("MyInt", Primitive U32)]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "duplicate struct" >:: run_semant_test_fail (make_program
        ~types:[
            TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}]);
            TypeStruct("Point", [{name="y"; field_type=Primitive F32; modifier=None; default_value=None}])
        ]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "duplicate method" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
        ~struct_funcs:[
            {name="doIt"; struct_name="Thing"; params=[]; return_types=[]; body=[]};
            {name="doIt"; struct_name="Thing"; params=[]; return_types=[]; body=[]}
        ]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
     "variable declaration no type no init" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=None}
        ]}]
        ()
    );
     "duplicate var in scope" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 2)}
        ]}]
        ()
    );
]

let error_tests_types = "Error Cases: Types" >::: [
    "assignment type mismatch (I32 vs Bool)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "x", BoolLit true));
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=None};
        ]}]
        ()
    );
     "assignment type mismatch (I32 vs U32)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "x", Cast(Primitive U32, IntLit 5))); (* Cast to U32 to force mismatch *)
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=None};
        ]}]
        ()
    );
    "declaration init type mismatch (String vs I32)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive String); initializer_expr=Some(IntLit 10)}
        ]}]
        ()
    );
     "declaration init type mismatch (I32 vs U32)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=Some(Cast(Primitive U32, IntLit 10))} (* Cast init to U32 *)
        ]}]
        ()
    );
    "binop type mismatch (+ I32 vs Bool)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(IntLit 1, Plus, BoolLit true))
        ]}]
        ()
    );
     "binop type mismatch (== I32 vs String)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(IntLit 1, Eq, StringLit "a"))
        ]}]
        ()
    );
     "binop type mismatch (&& I32 vs Bool)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(IntLit 1, And, BoolLit true))
        ]}]
        ()
    );
    "unary type mismatch (- Bool)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Neg, BoolLit true))
        ]}]
        ()
    );
     "unary type mismatch (! I32)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Not, IntLit 0))
        ]}]
        ()
    );
    "if condition not bool" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            IfStmt(IntLit 1, [], None)
        ]}]
        ()
    );
    "while condition not bool" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            WhileStmt(IntLit 0, [])
        ]}]
        ()
    );
    "for condition not bool" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            ForStmt(None, Some (IntLit 1), None, [])
        ]}]
        ()
    );
    "return type mismatch (I32 expected, Bool given)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[Primitive I32]; body=[
            Return(Some [BoolLit true])
        ]}]
        ()
    );
    "return type mismatch (I32 expected, U32 given)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[Primitive I32]; body=[
            Return(Some [Cast(Primitive U32, IntLit 1)]) (* Cast to U32 *)
        ]}]
        ()
    );
    "return wrong number of values" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[Primitive I32]; body=[
            Return(Some [IntLit 1; IntLit 2])
        ]}]
        ()
    );
     "return value from void func" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Return(Some [IntLit 1])
        ]}]
        ()
    );
     "return nothing from non-void func" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[Primitive Bool]; body=[
            Return(None)
        ]}]
        ()
    );
     "array literal element type mismatch (I32 vs Bool)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (ArrayLit(Primitive I32, [Cast(Primitive I32, IntLit 1); BoolLit true])) (* Still fails *)
        ]}]
        ()
    );
     "array literal element type mismatch (I32 vs U32)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (ArrayLit(Primitive I32, [Cast(Primitive U32, IntLit 1); Cast(Primitive I32, IntLit 2)])) (* Cast first element to U32 *)
        ]}]
        ()
    );
     "slice literal element type mismatch (F32 vs String)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SliceLit(Primitive F32, [FloatLit 1.0; StringLit "no"]))
        ]}]
        ()
    );
     "invalid cast (I32 to Bool)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             Expr (Cast(Primitive Bool, IntLit 1))
        ]}]
        ()
    );
    "null assignment to non-nullable (I32)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             Expr (SimpleAssign(Identifier "x", Null));
             VarDecl {is_const=false; name="x"; var_type=Some(Primitive I32); initializer_expr=None};
        ]}]
        ()
     );
]

let error_tests_scope = "Error Cases: Scope and Usage" >::: [
    "use undeclared variable" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "x", IntLit 1))
        ]}]
        ()
    );
    "call undeclared function" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (FunctionCall("no_such_func", []))
        ]}]
        ()
    );
    "call variable as function" >:: run_semant_test_fail (make_program
        ~globals:[{is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 1)}]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (FunctionCall("x", []))
        ]}]
        ()
    );
    "function wrong number of args" >:: run_semant_test_fail (make_program
        ~functions:[
            { name="foo"; params=[{name="a"; param_type=Primitive I32}]; return_types=[]; body=[] };
            { name="main"; params=[]; return_types=[]; body=[Expr (FunctionCall("foo", []))] }
        ]
        ()
    );
     "function wrong arg type (I32 expected, Bool given)" >:: run_semant_test_fail (make_program
        ~functions:[
            { name="foo"; params=[{name="a"; param_type=Primitive I32}]; return_types=[]; body=[] };
            { name="main"; params=[]; return_types=[]; body=[Expr (FunctionCall("foo", [BoolLit true]))] }
        ]
        ()
     );
     "function wrong arg type (I32 expected, U32 given)" >:: run_semant_test_fail (make_program
        ~functions:[
            { name="foo"; params=[{name="a"; param_type=Primitive I32}]; return_types=[]; body=[] };
            { name="main"; params=[]; return_types=[]; body=[Expr (FunctionCall("foo", [Cast(Primitive U32, IntLit 1)]))] } (* Cast arg to U32 *)
        ]
        ()
     );
     "assign to non-lvalue" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(IntLit 5, IntLit 10))
        ]}]
        ()
    );
     "index non-array/slice" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (IndexAccess(Identifier "x", IntLit 0));
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
        ]}]
        ()
    );
     "index with non-integer" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (IndexAccess(Identifier "a", BoolLit true));
            VarDecl {is_const=false; name="a"; var_type=None; initializer_expr=Some(ArrayLit(Primitive I32, [Cast(Primitive I32, IntLit 1)]))};
        ]}]
        ()
    );
      "slice non-array/slice" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SliceExpr(Identifier "x", Some(IntLit 0), None));
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
        ]}]
        ()
    );
      "slice with non-integer index" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SliceExpr(Identifier "a", Some(FloatLit 0.0), None));
            VarDecl {is_const=false; name="a"; var_type=None; initializer_expr=Some(ArrayLit(Primitive I32, [Cast(Primitive I32, IntLit 1)]))};
        ]}]
        ()
    );
     "make len non-integer" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr(Make(Slice(Primitive U8), StringLit "10", None))
        ]}]
        ()
    );
     "make cap non-integer" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr(Make(Slice(Primitive U8), IntLit 10, Some(BoolLit true)))
        ]}]
        ()
    );
    "use undeclared type alias" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=Some(TypeName "NoSuchType"); initializer_expr=None}
        ]}]
        ()
    );
    "use undeclared struct type in var decl" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="p"; var_type=Some(Struct "NoSuchStruct"); initializer_expr=None}
        ]}]
        ()
    );
]

let error_tests_structs = "Error Cases: Structs and Methods" >::: [
    "struct literal unknown struct" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("NoSuchStruct", []))
        ]}]
        ()
    );
    "struct literal unknown field" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("Point", [("y", Cast(Primitive I32, IntLit 1))]))
        ]}]
        ()
    );
    "struct literal field type mismatch (I32 vs Bool)" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("Point", [("x", BoolLit true)]))
        ]}]
        ()
    );
     "struct literal field type mismatch (I32 vs U32)" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("Point", [("x", Cast(Primitive U32, IntLit 1))])) (* Cast value to U32 *)
        ]}]
        ()
    );
    "field access on non-struct" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (FieldAccess(Identifier "x", "some_field"));
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
        ]}]
        ()
    );
    "field access unknown field" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (FieldAccess(Identifier "p", "y"));
            VarDecl {is_const=false; name="p"; var_type=None; initializer_expr=Some(StructLit("Point", [("x", Cast(Primitive I32, IntLit 1))]))};
        ]}]
        ()
    );
     "method call on non-struct" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (MethodCall(Identifier "x", "doThing", []));
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
        ]}]
        ()
    );
     "method call unknown method" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (MethodCall(Identifier "t", "noSuchMethod", []));
            VarDecl {is_const=false; name="t"; var_type=None; initializer_expr=Some(StructLit("Thing", []))};
        ]}]
        ()
    );
    "method call wrong number of args" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
         ~struct_funcs:[{
            name="doIt"; struct_name="Thing"; params=[{name="a"; param_type=Primitive I32}]; return_types=[];
            body=[]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (MethodCall(Identifier "t", "doIt", [])); (* Missing arg 'a' *)
            VarDecl {is_const=false; name="t"; var_type=None; initializer_expr=Some(StructLit("Thing", []))};
        ]}]
        ()
    );
     "method call wrong arg type (I32 expected, Bool given)" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
         ~struct_funcs:[{
            name="doIt"; struct_name="Thing"; params=[{name="a"; param_type=Primitive I32}]; return_types=[];
            body=[]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (MethodCall(Identifier "t", "doIt", [BoolLit true])); (* Wrong type for 'a' *)
            VarDecl {is_const=false; name="t"; var_type=None; initializer_expr=Some(StructLit("Thing", []))};
        ]}]
        ()
    );
     "method call wrong arg type (I32 expected, U32 given)" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
         ~struct_funcs:[{
            name="doIt"; struct_name="Thing"; params=[{name="a"; param_type=Primitive I32}]; return_types=[];
            body=[]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (MethodCall(Identifier "t", "doIt", [Cast(Primitive U32, IntLit 1)])); (* Cast arg to U32 *)
            VarDecl {is_const=false; name="t"; var_type=None; initializer_expr=Some(StructLit("Thing", []))};
        ]}]
        ()
    );
     "method definition for unknown struct" >:: run_semant_test_fail (make_program
         ~struct_funcs:[{
            name="doIt"; struct_name="NoSuchStruct"; params=[]; return_types=[];
            body=[]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
]


(* Add more test categories as needed: Control Flow, Arrays/Slices, etc. *)

let suite = "Semantic Analyzer Test Suite" >::: [
    basic_tests;
    type_tests;
    expression_tests;
    statement_tests;
    struct_method_tests;
    error_tests_declarations;
    error_tests_types;
    error_tests_scope;
    error_tests_structs;
  ]

let () =
  run_test_tt_main suite
