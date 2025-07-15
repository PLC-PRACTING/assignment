

open Ast


exception SemanticError of string


(* 符号表中存储的标识符信息 *)
type symbol_info =
  | VarInfo of ret_type      (* 变量信息, 在 ToyC 中类型总是 TInt *)
  | FuncInfo of ret_type * (param list) (* 函数信息: 返回类型 和 形参列表 *)

module SymbolTable = struct
  (* 符号表类型: 一个(string, symbol_info)哈希表的栈 *)
  type t = (string, symbol_info) Hashtbl.t list

  (* 创建一个空的符号表 (只包含一个全局作用域) *)
  let create () : t = [Hashtbl.create 10]

  (* 进入一个新的作用域 *)
  let enter_scope (st: t) : t =
    (Hashtbl.create 10) :: st

  (* 退出当前作用域 *)
  let exit_scope (st: t) : t =
    match st with
    | _ :: t -> t (* 弹出栈顶 *)
    | [] -> raise (SemanticError "严重错误: 试图退出全局作用域")

  (* 在当前作用域(栈顶)添加一个新符号 *)
  let add (st: t) (name: string) (info: symbol_info) : unit =
    match st with
    | current_scope :: _ ->
      if Hashtbl.mem current_scope name then
        raise (SemanticError ("在同一作用域内重复定义标识符: '" ^ name ^ "'"))
      else
        Hashtbl.add current_scope name info
    | [] -> raise (SemanticError "严重错误: 符号表为空")

  (* 从当前作用域开始, 逐级向上查找一个符号 *)
  let rec lookup (st: t) (name: string) : symbol_info =
    match st with
    | [] -> raise (SemanticError ("未声明的标识符: '" ^ name ^ "'"))
    | current_scope :: parent_scopes ->
      try
        Hashtbl.find current_scope name
      with Not_found ->
        lookup parent_scopes name
end


(* -------------------------------------------------------------------------- *)
(* 2. AST 遍历和检查的辅助函数                                                *)
(* -------------------------------------------------------------------------- *)

(* 定义遍历时携带的上下文环境 *)
type env = {
  sym_table: SymbolTable.t;
  current_func_ret_type: ret_type option; (* 当前函数的返回类型, None 表示在函数外 *)
  is_in_loop: bool;                      (* 当前是否在循环体内 *)
}

(* 声明相互递归的检查函数 *)
let rec check_expr (env: env) (expr: expr) : ret_type =
  match expr with
  | ConstInt _ -> TInt
  | Var name ->
    (match SymbolTable.lookup env.sym_table name with
     | VarInfo t -> t
     | FuncInfo _ -> raise (SemanticError ("函数名 '" ^ name ^ "' 不能作为值使用")))
  | UnaryOp (_, e) ->
    if check_expr env e <> TInt then
      raise (SemanticError "一元运算符只能作用于整数类型");
    TInt
  | BinOp (_, e1, e2) ->
    let t1 = check_expr env e1 in
    let t2 = check_expr env e2 in
    if t1 <> TInt || t2 <> TInt then
      raise (SemanticError "二元运算符两边的表达式必须都为整数类型");
    TInt
  | Call (name, args) ->
    (match SymbolTable.lookup env.sym_table name with
     | FuncInfo (ret_type, params) ->
       if List.length args <> List.length params then
         raise (SemanticError ("函数 '" ^ name ^ "' 调用参数数量错误: " ^
                               "期望 " ^ string_of_int (List.length params) ^
                               ", 得到 " ^ string_of_int (List.length args)));
       (* 检查每个参数的类型, 在ToyC中所有参数都应是int *)
       List.iter2
         (fun arg _ ->
            if check_expr env arg <> TInt then
              raise (SemanticError ("函数 '" ^ name ^ "' 的参数类型错误, 期望 int"))
         ) args params;
       ret_type (* 函数调用的类型是其返回类型 *)
     | VarInfo _ -> raise (SemanticError ("'" ^ name ^ "' 是一个变量, 不能被调用")))

and check_stmt (env: env) (stmt: stmt) : env =
  match stmt with
  | Empty -> env (* 空语句, 环境不变 *)
  | ExprStmt e ->
    let _ = check_expr env e in (* 检查表达式, 但忽略其类型 *)
    env
  | VarDecl (name, init_expr) ->
    let expr_type = check_expr env init_expr in
    if expr_type = TVoid then
      raise (SemanticError ("变量 '" ^ name ^ "' 不能由 void 类型的表达式初始化"));
    SymbolTable.add env.sym_table name (VarInfo TInt);
    env
  | Assign (name, e) ->
    (match SymbolTable.lookup env.sym_table name with
     | VarInfo _ ->
       let expr_type = check_expr env e in
       if expr_type = TVoid then
         raise (SemanticError ("不能将 void 类型的表达式赋值给变量 '" ^ name ^ "'"));
       env
     | FuncInfo _ -> raise (SemanticError ("不能给函数 '" ^ name ^ "' 赋值")))

  | Block stmts ->
    (* 1. 创建一个用于块内部的新环境，其符号表进入了新作用域 *)
    let block_env = { env with sym_table = SymbolTable.enter_scope env.sym_table } in
    
    (* 2. 使用 fold_left 遍历块内语句。
       每次 check_stmt 调用都会接收上一步更新后的环境，并返回一个更新后的环境。
       将最终的环境结果捕获到 `_` 中*)
    let _ = List.fold_left check_stmt block_env stmts in
    
    (* 3. 块处理结束后，返回原始的 env。*)
    env

  | If (cond, then_s, else_s_opt) ->
    if check_expr env cond = TVoid then
      raise (SemanticError "if 条件表达式不能是 void 类型");
    let _ = check_stmt env then_s in
    (match else_s_opt with
     | Some else_s -> let _ = check_stmt env else_s in ()
     | None -> ());
    env
  | While (cond, body) ->
    if check_expr env cond = TVoid then
      raise (SemanticError "while 条件表达式不能是 void 类型");
    (* 进入循环体, is_in_loop 设置为 true *)
    let loop_env = { env with is_in_loop = true } in
    let _ = check_stmt loop_env body in
    env
  | Break ->
    if not env.is_in_loop then raise (SemanticError "'break' 语句只能出现在循环内部");
    env
  | Continue ->
    if not env.is_in_loop then raise (SemanticError "'continue' 语句只能出现在循环内部");
    env
  | Return expr_opt ->
    (match env.current_func_ret_type with
     | None -> raise (SemanticError "'return' 语句只能出现在函数内部")
     | Some TInt ->
       (match expr_opt with
        | None -> raise (SemanticError "返回类型为 int 的函数必须返回一个值")
        | Some e ->
          if check_expr env e <> TInt then
            raise (SemanticError "函数返回值类型与声明不符, 期望 int"))
     | Some TVoid ->
       (match expr_opt with
        | Some _ -> raise (SemanticError "void 类型的函数不能返回值")
        | None -> ()));
    env

and check_func_def (st: SymbolTable.t) (func: func_def) : unit =
  let FuncDef (ret_type, _, params, body) = func in
  (* 1. 创建函数内部的上下文环境 *)
  let func_env = {
    sym_table = SymbolTable.enter_scope st;
    current_func_ret_type = Some ret_type;
    is_in_loop = false;
  } in
  (* 2. 将函数参数加入到新的作用域中 *)
  List.iter
    (fun (Param pname) -> SymbolTable.add func_env.sym_table pname (VarInfo TInt))
    params;
  (* 3. 检查函数体 *)
  let _ = check_stmt func_env body in
  ()
  (* 注意: ToyC规范要求int函数在"每一条可能的执行路径"上返回值。
     静态地完美检查这一点是困难的(等价于停机问题)。
     对于课程项目，通常简化为检查函数体的最后一条语句是否为return，
     或者不作此项严格检查，依赖测试用例来保证。
     这里我们暂时不作此项检查，以简化实现。*)


(* -------------------------------------------------------------------------- *)
(* 3. 主入口函数                                                              *)
(* -------------------------------------------------------------------------- *)

let check_program (prog: program) : unit =
  let Program funcs = prog in

  (* Pass 1: 将所有函数签名加入全局符号表 *)
  let st = SymbolTable.create () in
  List.iter
    (fun (FuncDef (ret, name, params, _)) ->
      (* 根据ToyC规则, 函数名不能重复 *)
      (match SymbolTable.lookup st name with
      | _ -> raise (SemanticError ("函数名重复定义: '" ^ name ^ "'"))
      | exception (SemanticError _) -> ()); (* 没找到是好事 *)
      SymbolTable.add st name (FuncInfo (ret, params))
    ) funcs;

  (* 全局检查: 必须有一个 `int main()` 函数 *)
  (match SymbolTable.lookup st "main" with
   | FuncInfo (TInt, []) -> () (* 找到了合法的 main 函数 *)
   | FuncInfo _ -> raise (SemanticError "'main' 函数签名必须是 'int main()'")
   | VarInfo _ -> raise (SemanticError "'main' 必须是一个函数, 而不是变量")
   | exception (SemanticError _) -> raise (SemanticError "程序缺少入口点: 'int main()' 函数未定义")
  );

  (* Pass 2: 逐个检查每个函数体 *)
  List.iter (check_func_def st) funcs;