

open Ast
open Ir

type scope = (string, string) Hashtbl.t (* 源码名 -> IR唯一名, e.g., "x" -> "x_1" *)

(* IR 生成过程中的上下文环境 *)
type env = {
  mutable temp_count: int;
  mutable label_count: int;
  
  loop_start_label: string option; (* 当前循环的开始标签 (用于 continue) *)
  loop_end_label: string option;   (* 当前循环的结束标签 (用于 break) *)

  scopes: scope list;
  var_count: int ref;
}

(* 创建一个新的 IR 生成环境 *)
let create_env () = {
  temp_count = 0;
  label_count = 0;
  loop_start_label = None;
  loop_end_label = None;
  scopes = [Hashtbl.create 10]; (* 包含一个空的全局作用域 *)
  var_count = ref 0;             (* 变量计数器从 0 开始 *)
}

(* 进入一个新作用域 *)
let enter_scope env =
  { env with scopes = (Hashtbl.create 10) :: env.scopes }

(* 退出一个作用域 *)
let exit_scope env =
  match env.scopes with
  | _ :: t -> { env with scopes = t }
  | [] -> failwith "Internal error: exiting global scope"

(* 生成一个新的唯一变量名 *)
let new_unique_var env (name: string) : string =
  let count = !(env.var_count) in
  env.var_count := count + 1;
  name ^ "_" ^ (string_of_int count)

(* 在当前作用域声明一个新变量 *)
let declare_var env (name: string) : env * string =
  let unique_name = new_unique_var env name in
  (match env.scopes with
  | current_scope :: _ -> Hashtbl.add current_scope name unique_name
  | [] -> failwith "Internal error: no scope to declare variable in");
  (env, unique_name)

(* 查找一个变量的唯一名 *)
let rec lookup_var_unique_name scopes (name: string) : string =
  match scopes with
  | [] -> failwith ("Semantic error should have caught this: undeclared variable " ^ name)
  | current_scope :: parent_scopes ->
    match Hashtbl.find_opt current_scope name with
    | Some unique_name -> unique_name
    | None -> lookup_var_unique_name parent_scopes name

(* 生成一个新的临时变量名 *)
let new_temp env =
  let i = env.temp_count in
  env.temp_count <- i + 1;
  "t" ^ string_of_int i

(* 生成一个新的标签名 *)
let new_label env =
  let i = env.label_count in
  env.label_count <- i + 1;
  "L" ^ string_of_int i

(*
 * `translate_expr` 函数:
 * 将一个 AST 表达式翻译成一个 IR 指令列表和一个存放最终结果的操作数。
 *
 * 返回: (instruction list * operand)
 *)
let rec translate_expr (env: env) (expr: Ast.expr) : instruction list * operand =
  match expr with
  | ConstInt n -> ([], IntLiteral n)
  
  | Var s ->
    (* 在使用变量时，查找它的唯一 IR 名称 *)
    let unique_name = lookup_var_unique_name env.scopes s in
    ([], Var unique_name)

  | UnaryOp (op, e) ->
    let (instrs, opnd) = translate_expr env e in
    let res_temp = new_temp env in
    (instrs @ [UnOp (res_temp, op, opnd)], Var res_temp)

  | BinOp (Or, e1, e2) ->
    let res_temp = new_temp env in
    let l_true = new_label env in
    let l_end = new_label env in
    let (instrs1, opnd1) = translate_expr env e1 in
    let (instrs2, opnd2) = translate_expr env e2 in
    let full_instrs = 
        instrs1 @ [IfNotZero (opnd1, l_true)]
        @ instrs2 @ [IfNotZero (opnd2, l_true)]
        @ [Assign (res_temp, IntLiteral 0)]
        @ [Goto l_end]
        @ [Label l_true]
        @ [Assign (res_temp, IntLiteral 1)]
        @ [Label l_end]
    in
    (full_instrs, Var res_temp)

  | BinOp (And, e1, e2) ->
    let res_temp = new_temp env in
    let l_false = new_label env in
    let l_end = new_label env in
    let (instrs1, opnd1) = translate_expr env e1 in
    let (instrs2, opnd2) = translate_expr env e2 in
    let full_instrs = 
        instrs1 @ [IfZero (opnd1, l_false)]
        @ instrs2 @ [IfZero (opnd2, l_false)]
        @ [Assign (res_temp, IntLiteral 1)]
        @ [Goto l_end]
        @ [Label l_false]
        @ [Assign (res_temp, IntLiteral 0)]
        @ [Label l_end]
    in
    (full_instrs, Var res_temp)

  | BinOp (op, e1, e2) ->
    let (instrs1, opnd1) = translate_expr env e1 in
    let (instrs2, opnd2) = translate_expr env e2 in
    let res_temp = new_temp env in
    (instrs1 @ instrs2 @ [BinOp (res_temp, opnd1, op, opnd2)], Var res_temp)
  
  | Call (fname, args) ->
    let (arg_instrs, arg_opnds) =
        List.fold_right (fun arg (acc_i, acc_o) ->
            let (i, o) = translate_expr env arg in
            (i @ acc_i, o :: acc_o)
        ) args ([], [])
    in
    let param_instrs = List.mapi (fun i opnd -> Param (i, opnd)) arg_opnds in
    let res_temp = new_temp env in
    let call_instr = Call (Some res_temp, fname, List.length args) in
    (arg_instrs @ param_instrs @ [call_instr], Var res_temp)

(* 一个获取新索引的辅助函数 *)
and new_temp_idx env = let i = env.temp_count in env.temp_count <- i + 1; i

(* 一个专门翻译条件并生成跳转的函数 *)
(* 它接收一个表达式、一个为真时跳转的标签、一个为假时跳转的标签 *)
and translate_cond (env: env) (expr: Ast.expr) (l_true: string) (l_false: string) : instruction list =
  match expr with
  | BinOp (op, e1, e2) when op = Eq || op = Neq || op = Lt || op = Gt || op = Leq || op = Geq ->
    let (instrs1, opnd1) = translate_expr env e1 in
    let (instrs2, opnd2) = translate_expr env e2 in
    instrs1 @ instrs2 @ [CondGoto (opnd1, opnd2, op, l_true); Goto l_false]
  | UnaryOp (Unot, e) -> translate_cond env e l_false l_true
  | BinOp (And, e1, e2) ->
    let l_mid = new_label env in
    let instrs1 = translate_cond env e1 l_mid l_false in
    let instrs2 = translate_cond env e2 l_true l_false in
    instrs1 @ [Label l_mid] @ instrs2
  | BinOp (Or, e1, e2) ->
    let l_mid = new_label env in
    let instrs1 = translate_cond env e1 l_true l_mid in
    let instrs2 = translate_cond env e2 l_true l_false in
    instrs1 @ [Label l_mid] @ instrs2
  | _ ->
    let (instrs, opnd) = translate_expr env expr in
    instrs @ [IfNotZero (opnd, l_true); Goto l_false]


(*
 * `translate_stmt` 函数:
 * 将一个 AST 语句翻译成一个 IR 指令列表。
 *
 * 返回: instruction list
 *)
and translate_stmt (env: env) (stmt: Ast.stmt) : env * instruction list =
  match stmt with
  | Empty -> (env, [])
  
  | ExprStmt e ->
    (match e with
     | Call (fname, args) ->
       let (arg_instrs, arg_opnds) = List.fold_right (fun arg (acc_i, acc_o) ->
           let (i, o) = translate_expr env arg in (i @ acc_i, o :: acc_o)
         ) args ([], [])
       in
       let param_instrs = List.mapi (fun i opnd -> Param (i, opnd)) arg_opnds in
       let call_instr = Call (None, fname, List.length args) in
       (env, arg_instrs @ param_instrs @ [call_instr])
     | _ -> (env, fst (translate_expr env e)))
     
  | VarDecl (vname, init_expr) ->
    (* 1. 在当前作用域声明新变量，获得唯一名和新环境 *)
    let (new_env, unique_name) = declare_var env vname in
    (* 2. 使用新环境翻译初始化表达式 *)
    let (instrs, opnd) = translate_expr new_env init_expr in
    (* 3. 返回新环境和赋值指令 *)
    (new_env, instrs @ [Assign (unique_name, opnd)])

  | Assign (vname, e) ->
    (* 1. 查找变量的唯一名 *)
    let unique_name = lookup_var_unique_name env.scopes vname in
    (* 2. 翻译右侧表达式，并生成优化后的 IR *)
    (match e with
     | BinOp (op, e1, e2) ->
       let (instrs1, opnd1) = translate_expr env e1 in
       let (instrs2, opnd2) = translate_expr env e2 in
       (env, instrs1 @ instrs2 @ [BinOp (unique_name, opnd1, op, opnd2)])
     | UnaryOp (op, e1) ->
       let (instrs1, opnd1) = translate_expr env e1 in
       (env, instrs1 @ [UnOp (unique_name, op, opnd1)])
     | _ ->
       let (instrs, opnd) = translate_expr env e in
       (env, instrs @ [Assign (unique_name, opnd)]))
  
  (* in translate_stmt *)
| Block stmts ->
  let inner_env = enter_scope env in
  
  (* 使用高效的列表构建方法 *)
  let final_env, rev_instrs_list = List.fold_left
    (fun (e, rev_acc) s ->
      let (e', new_instrs) = translate_stmt e s in
      (* 使用 :: (cons) 操作符，高效地将新指令列表添加到头部 *)
      (e', new_instrs :: rev_acc)
    ) (inner_env, []) stmts
  in

  (* 1. 反转列表顺序，2. 拍平列表的列表 *)
  let block_instrs = List.concat (List.rev rev_instrs_list) in

  (exit_scope final_env, [ScopeBegin] @ block_instrs @ [ScopeEnd])

  | If (cond, then_s, else_s_opt) ->
    let l_then = new_label env in
    let l_else = new_label env in
    let l_end = new_label env in
    (match else_s_opt with
    | Some else_s ->
        let cond_instrs = translate_cond env cond l_then l_else in
        let _, then_instrs = translate_stmt env then_s in
        let _, else_instrs = translate_stmt env else_s in
        (env, cond_instrs @ [Label l_then] @ then_instrs @ [Goto l_end] @ [Label l_else] @ else_instrs @ [Label l_end])
    | None ->
        let cond_instrs = translate_cond env cond l_then l_end in
        let _, then_instrs = translate_stmt env then_s in
        (env, cond_instrs @ [Label l_then] @ then_instrs @ [Label l_end]))

  | While (cond, body) ->
    let l_start = new_label env in
    let l_body = new_label env in
    let l_end = new_label env in
    let loop_env = { env with loop_start_label = Some l_start; loop_end_label = Some l_end } in
    let cond_instrs = translate_cond loop_env cond l_body l_end in
    let _, body_instrs = translate_stmt loop_env body in
    (env, [Label l_start] @ cond_instrs @ [Label l_body] @ body_instrs @ [Goto l_start] @ [Label l_end])

  | Break -> (match env.loop_end_label with Some l -> (env, [Goto l]) | None -> failwith "break outside loop")
  | Continue -> (match env.loop_start_label with Some l -> (env, [Goto l]) | None -> failwith "continue outside loop")

   | Return (Some e) ->
    (match e with
     (* case 1: return a + b (或 a-b, a*b ...) *)
     | BinOp (op, e1, e2) ->
       let (instrs1, opnd1) = translate_expr env e1 in
       let (instrs2, opnd2) = translate_expr env e2 in
       (* 一个临时的名字来存放 BinOp 的结果，即使它可能被 codegen 优化掉 *)
       let res_temp = new_temp env in
       let binop_instr = BinOp (res_temp, opnd1, op, opnd2) in
       let return_instr = Return (Some (Var res_temp)) in
       (env, instrs1 @ instrs2 @ [binop_instr; return_instr])

     (* case 2: return -x (或 !x ...) *)
     | UnaryOp (op, e1) ->
       let (instrs1, opnd1) = translate_expr env e1 in
       let res_temp = new_temp env in
       let unop_instr = UnOp (res_temp, op, opnd1) in
       let return_instr = Return (Some (Var res_temp)) in
       (env, instrs1 @ [unop_instr; return_instr])

     (* 后备 case: return x 或 return foo() 等 *)
     | _ ->
       let (instrs, opnd) = translate_expr env e in
       (env, instrs @ [Return (Some opnd)])
    )

  | Return None -> (env, [Return None])


(* 翻译一个函数定义 *)
let translate_func_def (func: Ast.func_def) : ir_func =
  let Ast.FuncDef (_, name, params_ast, body) = func in
  let env = create_env () in
  
  (* 将参数加入到初始作用域 *)
  let env_with_params, param_names = List.fold_left
    (fun (e, names_acc) (Ast.Param pname) ->
      let (e', unique_name) = declare_var e pname in
      (e', names_acc @ [unique_name])
    ) (env, []) params_ast
  in
  let arg_instrs = List.mapi (fun i pname -> Arg (i, pname)) param_names in

  (* 翻译函数体 *)
  let _, body_instrs = translate_stmt env_with_params body in

  (* 确保函数末尾有返回 *)
  let final_instrs =
    match List.rev body_instrs with
    | (Return _) :: _ -> body_instrs
    | ScopeEnd :: (Return _) :: _ -> body_instrs
    | _ -> body_instrs @ [Return None]
  in

  { name; params = param_names; instructions = arg_instrs @ final_instrs }

(* 主入口函数: 翻译整个程序 *)
let generate_ir (prog: Ast.program) : ir_program =
  let Ast.Program funcs = prog in
  List.map translate_func_def funcs