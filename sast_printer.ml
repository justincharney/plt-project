open Sast
open Ast (* For string_of_biop, etc., and type_modifier *)

(* --- Operator printing helpers --- *)
let string_of_biop = function
  | Plus -> "+" | Minus -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | And -> "&&" | Or -> "||"
  | Lshift -> "<<" | Rshift -> ">>" | Bitand -> "&" | Bitor -> "|" | Bitxor -> "^"

let string_of_unop = function
  | Neg -> "-" | Not -> "!" | Bitnot -> "~" | Inc -> "++" | Dec -> "--"

let string_of_compound_op = function
  | PlusAssign -> "+=" | MinusAssign -> "-=" | TimesAssign -> "*=" | DivAssign -> "/=" | ModAssign -> "%="
  | LshiftAssign -> "<<=" | RshiftAssign -> ">>=" | BitandAssign -> "&=" | BitxorAssign -> "^=" | BitorAssign -> "|="

let string_of_modifier_opt = function
  | None -> ""
  | Some Private -> "private "
  | Some Mutable -> "mut "
  | Some Final -> "final "
  | Some Late -> "late "

(* --- Main SAST Printing Functions --- *)

let rec string_of_sexpr (typ, sx) =
  let sx_str = match sx with
    | SIntLit l -> string_of_int l
    | SBoolLit b -> string_of_bool b
    | SCharLit c -> Printf.sprintf "'%c'" c (* Note: Doesn't handle escapes like '\n' well visually *)
    | SFloatLit f -> string_of_float f
    | SStringLit s -> Printf.sprintf "\"%s\"" (String.escaped s) (* Use String.escaped *)
    | SNull -> "null"
    | SArrayLit (t, exprs) -> Printf.sprintf "%s{%s}" (string_of_ty t) (String.concat ", " (List.map string_of_sexpr exprs))
    (* | SSliceLit (t, exprs) -> Printf.sprintf "[]%s{%s}" (string_of_ty t) (String.concat ", " (List.map string_of_sexpr exprs)) *)
    | SStructLit (name, fields) ->
        let field_strs = List.map (fun (fname, sexpr) -> Printf.sprintf "%s: %s" fname (string_of_sexpr sexpr)) fields in
        Printf.sprintf "%s{%s}" name (String.concat ", " field_strs)
    | SIdentifier id -> id
    | SFieldAccess (sexpr, field) -> Printf.sprintf "%s.%s" (string_of_sexpr sexpr) field
    | SIndexAccess (sexpr, index) -> Printf.sprintf "%s[%s]" (string_of_sexpr sexpr) (string_of_sexpr index)
    (* | SSliceExpr (sexpr, start_opt, end_opt) ->
        let start_str = match start_opt with None -> "" | Some s -> string_of_sexpr s in
        let end_str = match end_opt with None -> "" | Some e -> string_of_sexpr e in
        Printf.sprintf "%s[%s:%s]" (string_of_sexpr sexpr) start_str end_str *)
    | SBinop (e1, op, e2) -> Printf.sprintf "(%s %s %s)" (string_of_sexpr e1) (string_of_biop op) (string_of_sexpr e2)
    | SUnaop (op, e) -> Printf.sprintf "(%s%s)" (string_of_unop op) (string_of_sexpr e)
    | SSimpleAssign (lval, rval) -> Printf.sprintf "(%s = %s)" (string_of_sexpr lval) (string_of_sexpr rval)
    | SCompoundAssign (lval, op, rval) -> Printf.sprintf "(%s %s %s)" (string_of_sexpr lval) (string_of_compound_op op) (string_of_sexpr rval)
    | SSequence (e1, e2) -> Printf.sprintf "(%s; %s)" (string_of_sexpr e1) (string_of_sexpr e2)
    | SFunctionCall (name, args) -> Printf.sprintf "%s(%s)" name (String.concat ", " (List.map string_of_sexpr args))
    | SMethodCall (obj, name, args) -> Printf.sprintf "%s.%s(%s)" (string_of_sexpr obj) name (String.concat ", " (List.map string_of_sexpr args))
    | SMake (t, len, cap_opt) ->
        let cap_str = match cap_opt with None -> "" | Some c -> ", " ^ string_of_sexpr c in
        Printf.sprintf "make(%s, %s%s)" (string_of_ty t) (string_of_sexpr len) cap_str
    | SCast (t, e) -> Printf.sprintf "%s(%s)" (string_of_ty t) (string_of_sexpr e)
  in
  Printf.sprintf "%s : %s" sx_str (string_of_ty typ)

let rec string_of_sstmt ?(indent=0) stmt =
  let indent_str = String.make indent ' ' in
  match stmt with
  | SExpr sexpr -> indent_str ^ string_of_sexpr sexpr ^ ";"
  | SVarDecl vd ->
      let const_str = if vd.is_const then "const " else "var " in
      let type_str = string_of_ty vd.var_type in
      let init_str = match vd.initializer_expr with
        | None -> ""
        | Some ie -> " = " ^ string_of_sexpr ie
      in
      Printf.sprintf "%s%s%s : %s%s;" indent_str const_str vd.name type_str init_str
  | SBlock stmts ->
      let stmts_str = List.map (string_of_sstmt ~indent:(indent + 2)) stmts in
      Printf.sprintf "%s{\n%s\n%s}" indent_str (String.concat "\n" stmts_str) indent_str
  | SIf (cond, then_stmts, else_opt) ->
      let then_block_str = String.concat "\n" (List.map (string_of_sstmt ~indent:(indent + 2)) then_stmts) in
      let else_str = match else_opt with
        | None -> ""
        | Some else_stmt -> (* Now handles both else block and else if cleanly *)
             (* Check if it's an SBlock for correct brace placement *)
             let else_body_str = string_of_sstmt ~indent else_stmt in (* Render the else part *)
             Printf.sprintf "\n%selse %s" indent_str else_body_str
      in
      Printf.sprintf "%sif (%s) {\n%s\n%s}%s" indent_str (string_of_sexpr cond) then_block_str indent_str else_str
  | SWhile (cond, body) ->
      let body_str = String.concat "\n" (List.map (string_of_sstmt ~indent:(indent + 2)) body) in
      Printf.sprintf "%swhile (%s) {\n%s\n%s}" indent_str (string_of_sexpr cond) body_str indent_str
  | SFor (init, cond, step, body) ->
        let init_str = match init with
          | None -> ";"
          | Some (SVarDecl vd) -> (* Handle SVarDecl specifically, using 'vd' *)
              let const_str = if vd.is_const then "const " else "var " in
              (* In SAST, var decls should always have a type after checking *)
              let type_str = string_of_ty vd.var_type in
              let init_val_str = match vd.initializer_expr with
                | None -> failwith "SAST Printing Error: VarDecl in for loop init missing initializer" (* Semantic check should prevent this *)
                | Some ie -> " = " ^ string_of_sexpr ie
              in
              Printf.sprintf "%s%s : %s%s" const_str vd.name type_str init_val_str (* No semicolon here *)
          | Some s -> (* General case for other statement types (e.g., SExpr) *)
               let full_stmt_str = string_of_sstmt ~indent:0 s in
               (* Remove trailing semicolon if present *)
               if String.length full_stmt_str > 0 && full_stmt_str.[String.length full_stmt_str - 1] = ';' then
                  String.sub full_stmt_str 0 (String.length full_stmt_str - 1)
               else
                  full_stmt_str (* Should not happen if string_of_sstmt is consistent *)
        in
        let cond_str = match cond with None -> ";" | Some e -> string_of_sexpr e ^ ";" in
        let step_str = match step with None -> "" | Some e -> string_of_sexpr e in
        let body_str = String.concat "\n" (List.map (string_of_sstmt ~indent:(indent + 2)) body) in
        Printf.sprintf "%sfor (%s %s %s) {\n%s\n%s}" indent_str init_str cond_str step_str body_str indent_str
  | SReturn expr_opts ->
      let exprs_str = match expr_opts with
        | None -> ""
        | Some exprs -> " " ^ String.concat ", " (List.map string_of_sexpr exprs)
      in
      indent_str ^ "return" ^ exprs_str ^ ";"
  | SBreak -> indent_str ^ "break;"
  | SContinue -> indent_str ^ "continue;"

let string_of_sparam (p : sparam) =
  Printf.sprintf "%s: %s" p.name (string_of_ty p.param_type)

let string_of_sfield (f : sfield) =
    let mod_str = string_of_modifier_opt f.modifier in
    let def_str = match f.default_value with
      | None -> ""
      | Some e -> " = " ^ string_of_sexpr e (* This should be correct now *)
    in
    Printf.sprintf "%s%s: %s%s;" mod_str f.name (string_of_ty f.field_type) def_str

let string_of_stype_decl = function
  | STypeStruct (name, fields) ->
      (* Apply indent to each field string *)
      let fields_str = List.map (fun f -> "  " ^ string_of_sfield f) fields in
      Printf.sprintf "type %s struct {\n%s\n}" name (String.concat "\n" fields_str)
  | STypeAlias (name, t) ->
      Printf.sprintf "type %s = %s;" name (string_of_ty t)

let string_of_sglobal_decl (g : sglobal_decl) =
  let const_str = if g.is_const then "const " else "" in (* Assuming global var decl doesn't use 'var' keyword *)
  let init_str = match g.initializer_expr with
    | None -> ";" (* Globals might not require initializers depending on language rules *)
    | Some e -> " = " ^ string_of_sexpr e ^ ";"
  in
  Printf.sprintf "%s%s: %s%s" const_str g.name (string_of_ty g.var_type) init_str

let string_of_sfunc_decl (f : sfunc_decl) =
  let params_str = String.concat ", " (List.map string_of_sparam f.params) in
  let return_str = match f.return_types with
    | [] -> ""
    | [TyUnit] -> "" (* Don't print unit return type *)
    | [t] -> " " ^ string_of_ty t
    | ts -> Printf.sprintf " (%s)" (String.concat ", " (List.map string_of_ty ts))
  in
  let body_str = String.concat "\n" (List.map (string_of_sstmt ~indent:2) f.body) in
  Printf.sprintf "func %s(%s)%s {\n%s\n}" f.name params_str return_str body_str

let string_of_sstruct_func (sf : sstruct_func) =
   let params_str = String.concat ", " (List.map string_of_sparam sf.params) in
   let return_str = match sf.return_types with
     | [] -> ""
     | [TyUnit] -> "" (* Don't print unit return type *)
     | [t] -> " " ^ string_of_ty t
     | ts -> Printf.sprintf " (%s)" (String.concat ", " (List.map string_of_ty ts))
   in
   let body_str = String.concat "\n" (List.map (string_of_sstmt ~indent:2) sf.body) in
   Printf.sprintf "func (%s) %s(%s)%s {\n%s\n}" sf.struct_name sf.name params_str return_str body_str

let string_of_sprogram p =
  let pkg_str = "package " ^ p.sp_package ^ ";\n" in
  let imports_str = String.concat "\n" (List.map (fun s -> "import \"" ^ s ^ "\";") p.sp_imports) ^ if List.length p.sp_imports > 0 then "\n\n" else "\n" in
  let types_str = String.concat "\n\n" (List.map string_of_stype_decl p.sp_types) ^ if List.length p.sp_types > 0 then "\n\n" else "" in
  let globals_str = String.concat "\n" (List.map string_of_sglobal_decl p.sp_globals) ^ if List.length p.sp_globals > 0 then "\n\n" else "" in
  let funcs_str = String.concat "\n\n" (List.map string_of_sfunc_decl p.sp_funcs) ^ if List.length p.sp_funcs > 0 then "\n\n" else "" in
  let methods_str = String.concat "\n\n" (List.map string_of_sstruct_func p.sp_methods) ^ if List.length p.sp_methods > 0 then "\n\n" else "" in
  pkg_str ^ imports_str ^ types_str ^ globals_str ^ funcs_str ^ methods_str
