(* pat_sast.ml *)

open Ast
open Semantic

(* ------------------------------------------------------------------ *)
(* Helpers for printing primitive types, operators, and type_expr       *)
(* ------------------------------------------------------------------ *)

let string_of_primitive = function
  | Bool   -> "bool"
  | String -> "string"
  | U8     -> "u8"
  | U16    -> "u16"
  | U32    -> "u32"
  | U64    -> "u64"
  | I8     -> "i8"
  | I16    -> "i16"
  | I32    -> "i32"
  | I64    -> "i64"
  | F32    -> "f32"
  | F64    -> "f64"
  | Error  -> "error"

let rec string_of_type_expr = function
  | Primitive p -> string_of_primitive p
  | Array(te,n) -> Printf.sprintf "[%d]%s" n (string_of_type_expr te)
  | Slice te     -> Printf.sprintf "[]%s"   (string_of_type_expr te)
  | TypeName s   -> s

let string_of_biop = function
  | Plus    -> "+"   | Minus   -> "-"
  | Mult    -> "*"   | Div     -> "/"
  | Mod     -> "%"   | Lshift  -> "<<"
  | Rshift  -> ">>"  | Bitxor  -> "^"
  | Bitor   -> "|"   | Bitand  -> "&"
  | Eq      -> "=="  | Neq     -> "!="
  | Lt      -> "<"   | Le      -> "<="
  | Gt      -> ">"   | Ge      -> ">="
  | And     -> "&&"  | Or      -> "||"

let string_of_unop = function
  | Neg    -> "-"  | Not    -> "!"
  | Bitnot -> "~"  | Inc    -> "++"
  | Dec    -> "--"

let string_of_assign_op = function
  | RegAssign      -> "="
  | DeclAssign     -> ":="
  | PlusAssign     -> "+="
  | MinusAssign    -> "-="
  | TimesAssign    -> "*="
  | DivAssign      -> "/="
  | ModAssign      -> "%="
  | LshiftAssign   -> "<<="
  | RshiftAssign   -> ">>="
  | BitandAssign   -> "&="
  | BitxorAssign   -> "^="
  | BitorAssign    -> "|="

(* ------------------------------------------------------------------ *)
(* Pretty-printing S A S T  e x p r e s s i o n s                     *)
(* ------------------------------------------------------------------ *)

let rec string_of_sexpr (t, sx) : string =
  Printf.sprintf "(%s : %s)"
    (string_of_type_expr t)
    (string_of_sx sx)

and string_of_sx = function
  | SIntLit i         -> string_of_int i
  | SBoolLit true     -> "true"
  | SBoolLit false    -> "false"
  | SCharLit c        -> "'" ^ String.make 1 c ^ "'"
  | SFloatLit f       -> string_of_float f
  | SStringLit s      -> "\"" ^ String.escaped s ^ "\""
  | SNull             -> "null"
  | SId id            -> id

  | SBinop (l, op, r) ->
      Printf.sprintf "(%s %s %s)"
        (string_of_sexpr l)
        (string_of_biop op)
        (string_of_sexpr r)

  | SUnaop (op, e) ->
      Printf.sprintf "(%s%s)"
        (string_of_unop op)
        (string_of_sexpr e)

  | SAssignment (op, lhs, rhs) ->
      Printf.sprintf "(%s %s %s)"
        (string_of_sexpr lhs)
        (string_of_assign_op op)
        (string_of_sexpr rhs)

  | SCast (te, e) ->
      Printf.sprintf "%s(%s)"
        (string_of_type_expr te)
        (string_of_sexpr e)

  | SArrayLit (te, elems) ->
      let body = elems |> List.map string_of_sexpr |> String.concat ", " in
      Printf.sprintf "[%s]{%s}"
        (string_of_type_expr te) body

  | SSliceLit (te, elems) ->
      let body = elems |> List.map string_of_sexpr |> String.concat ", " in
      Printf.sprintf "[]%s{%s}"
        (string_of_type_expr te) body

  | SStructLit (te, fields) ->
      let nm = string_of_type_expr te in
      let body =
        fields
        |> List.map (fun (fn, e) ->
             fn ^ ": " ^ string_of_sexpr e)
        |> String.concat ", "
      in
      Printf.sprintf "%s{ %s }" nm body

  | SFieldAccess (e, f) ->
      Printf.sprintf "%s.%s" (string_of_sexpr e) f

  | SIndexAccess (a, i) ->
      Printf.sprintf "%s[%s]"
        (string_of_sexpr a)
        (string_of_sexpr i)

  | SSliceExpr (arr, st, Some ed) ->
      Printf.sprintf "%s[%s:%s]"
        (string_of_sexpr arr)
        (string_of_sexpr st)
        (string_of_sexpr ed)

  | SSliceExpr (arr, st, None) ->
      Printf.sprintf "%s[%s:]"
        (string_of_sexpr arr)
        (string_of_sexpr st)

  | SFunctionCall (nm, args) ->
      let body = args |> List.map string_of_sexpr |> String.concat ", " in
      Printf.sprintf "%s(%s)" nm body

  | SMethodCall (recv, m, args) ->
      let body = args |> List.map string_of_sexpr |> String.concat ", " in
      Printf.sprintf "%s.%s(%s)"
        (string_of_sexpr recv) m body

  | SContinue -> "continue"
  | SBreak    -> "break"

(* ------------------------------------------------------------------ *)
(* Pretty-printing  S A S T  s t a t e m e n t s                       *)
(* ------------------------------------------------------------------ *)

let indent n = String.make (2*n) ' '

let rec string_of_sstmt indent_level stmt : string =
  let ind = indent indent_level in
  match stmt with
  | SExpr e ->
      ind ^ string_of_sexpr e ^ ";\n"

  | SVarDecl (is_const, name, t_opt, e) ->
      let kw = if is_const then "const" else "var" in
      let decl =
        match t_opt with
        | Some te ->
            Printf.sprintf "%s %s %s = %s"
              kw (string_of_type_expr te) name (string_of_sexpr e)
        | None ->
            Printf.sprintf "%s %s := %s"
              kw name (string_of_sexpr e)
      in
      ind ^ decl ^ ";\n"

  | SIf (cond, then_bs, else_bs) ->
      let hdr  = ind ^ "if (" ^ string_of_sexpr cond ^ ") {\n" in
      let then_str =
        then_bs
        |> List.map (string_of_sstmt (indent_level+1))
        |> String.concat ""
      in
      let mid  = ind ^ "} else {\n" in
      let else_str =
        else_bs
        |> List.map (string_of_sstmt (indent_level+1))
        |> String.concat ""
      in
      mid ^ then_str ^ mid ^ else_str ^ ind ^ "}\n"

  | SFor (init_o, cond_o, upd_o, body) ->
      let sinit =
        match init_o with
        | Some s -> String.trim (string_of_sstmt 0 s)
        | None   -> ""
      in
      let scond = Option.fold ~none:"" ~some:string_of_sexpr cond_o in
      let supd  = Option.fold ~none:"" ~some:string_of_sexpr upd_o in
      let hdr =
        ind ^ Printf.sprintf "for (%s; %s; %s) {\n" sinit scond supd
      in
      let body_str =
        body
        |> List.map (string_of_sstmt (indent_level+1))
        |> String.concat ""
      in
      hdr ^ body_str ^ ind ^ "}\n"

  | SWhile (cond, body) ->
      let hdr = ind ^ "while (" ^ string_of_sexpr cond ^ ") {\n" in
      let body_str =
        body
        |> List.map (string_of_sstmt (indent_level+1))
        |> String.concat ""
      in
      hdr ^ body_str ^ ind ^ "}\n"

  | SReturn exprs ->
      let body = exprs |> List.map string_of_sexpr |> String.concat ", " in
      ind ^ "return " ^ body ^ ";\n"

(* ------------------------------------------------------------------ *)
(* Pretty-printing  S A S T   f u n c t i o n s                        *)
(* ------------------------------------------------------------------ *)

let string_of_sparam p =
  let base = Printf.sprintf "%s: %s"
    p.sp_name (string_of_type_expr p.sp_type)
  in
  if p.sp_is_variadic then base ^ "..." else base

let pp_function f =
  let params = f.sf_params |> List.map string_of_sparam |> String.concat ", " in
  let rets =
    match f.sf_return_types with
    | [] -> ""
    | [t] -> " -> " ^ string_of_type_expr t
    | ts  ->
        let ts_str = ts |> List.map string_of_type_expr |> String.concat ", " in
        " -> (" ^ ts_str ^ ")"
  in
  Printf.printf "func %s(%s)%s {\n" f.sf_name params rets;
  List.iter (fun s -> Printf.printf "%s" (string_of_sstmt 1 s)) f.sf_body;
  Printf.printf "}\n\n"

let pp_struct_function m =
  let params = m.ss_params |> List.map string_of_sparam |> String.concat ", " in
  let rets =
    match m.ss_return_types with
    | [] -> ""
    | [t] -> " -> " ^ string_of_type_expr t
    | ts  ->
        let ts_str = ts |> List.map string_of_type_expr |> String.concat ", " in
        " -> (" ^ ts_str ^ ")"
  in
  Printf.printf "method %s::%s(%s)%s {\n"
    m.ss_struct m.ss_name params rets;
  List.iter (fun s -> Printf.printf "%s" (string_of_sstmt 1 s)) m.ss_body;
  Printf.printf "}\n\n"

(* ------------------------------------------------------------------ *)
(* Top-level  p r i n t   s p r o g r a m                              *)
(* ------------------------------------------------------------------ *)

let pp_program prog =
  (* Package & imports *)
  Printf.printf "package %s\n" prog.sp_package;
  List.iter (Printf.printf "import \"%s\"\n") prog.sp_imports;
  Printf.printf "\n";

  (* Type aliases *)
  List.iter (fun (n, te) ->
    Printf.printf "alias %s = %s\n" n (string_of_type_expr te)
  ) prog.sp_type_aliases;
  Printf.printf "\n";

  (* Struct definitions *)
  List.iter (fun (n, fields) ->
    let fs =
      fields
      |> List.map (fun (fn, ft) ->
           fn ^ ": " ^ string_of_type_expr ft)
      |> String.concat "; "
    in
    Printf.printf "struct %s { %s }\n" n fs
  ) prog.sp_structs;
  Printf.printf "\n";

  (* Global variables *)
  List.iter (fun (n, te) ->
    Printf.printf "global %s: %s\n" n (string_of_type_expr te)
  ) prog.sp_globals;
  Printf.printf "\n";

  (* Free functions *)
  List.iter pp_function prog.sp_functions;

  (* Struct methods *)
  List.iter pp_struct_function prog.sp_struct_functions
