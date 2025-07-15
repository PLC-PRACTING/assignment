(* codegen.ml -- 从 IR 生成 RISC-V 汇编代码 *)

open Ir
open Ast 

(* 代码生成上下文环境, 针对单个函数 *)
type env = {
  stack_map: (string, int) Hashtbl.t; (* 变量名 -> 栈偏移量 (相对于 fp/s0) *)
  mutable stack_size: int;            (* 当前函数的总栈帧大小 *)
  mutable asm_code: string list;      (* 存放生成的汇编指令 *)
  mutable func_name: string;          (* 当前函数名 *)
}

(* 创建一个新的代码生成环境 *)
let create_env () = {
  stack_map = Hashtbl.create 20;
  stack_size = 0;
  asm_code = [];
  func_name = ""; 
}

(* 向环境中添加汇编代码 (逆序添加) *)
let emit env str =
  env.asm_code <- str :: env.asm_code

(* 将一个操作数加载到指定的寄存器 (e.g., "t0") *)
let load_operand env reg op =
  match op with
  | IntLiteral n -> emit env (Printf.sprintf "  li %s, %d" reg n)
  | Var s ->
    let offset = Hashtbl.find env.stack_map s in
    emit env (Printf.sprintf "  lw %s, %d(s0)" reg offset)

(* 将指定寄存器中的值存储到变量的栈位置 *)
let store_result env reg var_name =
  let offset = Hashtbl.find env.stack_map var_name in
  emit env (Printf.sprintf "  sw %s, %d(s0)" reg offset)

(* 翻译单条 IR 指令 *)
let rec translate_single_instruction env instr =
  match instr with
  | Ir.Label s -> emit env (s ^ ":")
  | Ir.Assign (v, op) ->
    load_operand env "t0" op;
    store_result env "t0" v
  | Ir.BinOp (res, op1, bop, op2) ->
    load_operand env "t0" op1;
    load_operand env "t1" op2;
    let result_reg = "t2" in
    (match bop with
     | Add -> emit env (Printf.sprintf "  add %s, t0, t1" result_reg)
     | Sub -> emit env (Printf.sprintf "  sub %s, t0, t1" result_reg)
     | Mul -> emit env (Printf.sprintf "  mul %s, t0, t1" result_reg)
     | Div -> emit env (Printf.sprintf "  div %s, t0, t1" result_reg)
     | Mod -> emit env (Printf.sprintf "  rem %s, t0, t1" result_reg)
     | And -> emit env (Printf.sprintf "  and %s, t0, t1" result_reg)
     | Or  -> emit env (Printf.sprintf "  or %s, t0, t1" result_reg)
     | Eq  -> emit env (Printf.sprintf "  sub %s, t0, t1; seqz %s, %s" result_reg result_reg result_reg)
     | Neq -> emit env (Printf.sprintf "  sub %s, t0, t1; snez %s, %s" result_reg result_reg result_reg)
     | Lt  -> emit env (Printf.sprintf "  slt %s, t0, t1" result_reg)
     | Gt  -> emit env (Printf.sprintf "  sgt %s, t0, t1" result_reg)
     | Leq -> emit env (Printf.sprintf "  sgt %s, t0, t1; xori %s, %s, 1" result_reg result_reg result_reg)
     | Geq -> emit env (Printf.sprintf "  slt %s, t0, t1; xori %s, %s, 1" result_reg result_reg result_reg)
    );
    store_result env result_reg res
  | Ir.UnOp (res, uop, op) ->
    load_operand env "t0" op;
    (match uop with
     | Uminus -> emit env "  neg t0, t0"
     | Uplus  -> ()
     | Unot   -> emit env "  seqz t0, t0");
    store_result env "t0" res
  | Ir.Goto label -> emit env ("  j " ^ label)
  
  | Ir.IfZero (cond, label) ->
    load_operand env "t0" cond;
    emit env (Printf.sprintf "  beqz t0, %s" label)

  | Ir.IfNotZero (cond, label) ->
    load_operand env "t0" cond;
    emit env (Printf.sprintf "  bnez t0, %s" label)

  | Ir.CondGoto (op1, op2, bop, label) ->
    load_operand env "t0" op1;
    load_operand env "t1" op2;
    let branch_op_str = match bop with
      | Eq  -> "beq"  (* Branch if Equal *)
      | Neq -> "bne"  (* Branch if Not Equal *)
      | Lt  -> "blt"  (* Branch if Less Than *)
      | Gt  -> "bgt"  (* Branch if Greater Than *)
      | Leq -> "ble"  (* Branch if Less than or Equal *)
      | Geq -> "bge"  (* Branch if Greater than or Equal *)
      | _ -> failwith "Invalid comparison operator for CondGoto"
    in
    emit env (Printf.sprintf "  %s t0, t1, %s" branch_op_str label)

  | Ir.Param (idx, op) ->
    let arg_regs = ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"] in
    if idx < List.length arg_regs then
      let reg = List.nth arg_regs idx in
      (* 将参数值加载到对应的 aN 寄存器 *)
      load_operand env reg op
    else
      failwith ("Function argument " ^ string_of_int (idx+1) ^ " not supported (stack passing)")

  (* ===  Arg 的处理 === *)
  | Ir.Arg (idx, var_name) ->
    let arg_regs = ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"] in
    if idx < List.length arg_regs then
      let reg = List.nth arg_regs idx in
      (* 将传入的寄存器参数值，保存到为该参数分配的栈空间中 *)
      store_result env reg var_name
    else
      failwith ("Function argument " ^ string_of_int (idx+1) ^ " not supported")
  | Ir.Call (res_opt, fname, _) ->
    emit env (Printf.sprintf "  call %s" fname);
    (match res_opt with
     | Some res_var -> store_result env "a0" res_var
     | None -> ())
  | Ir.ScopeBegin -> () (* 不生成任何代码 *)
  | Ir.ScopeEnd -> ()   (* 不生成任何代码 *)
  | Ir.Return (Some op) ->
    load_operand env "a0" op;
    emit env ("  j .L_ret_" ^ env.func_name)
  | Ir.Return None -> emit env ("  j .L_ret_" ^ env.func_name)

and translate_instructions env (instructions: Ir.instruction list) =
  match instructions with
  (* ========================================================================================= *)
  (* 模式 1: BinOp(res,...) 后紧跟 CondGoto(res,...) -> 优化条件判断                         *)
  (* IR:   t1 = x % 2; if (t1 == 0) goto L_true;                                             *)
  (* ========================================================================================= *)
  | Ir.BinOp(res, op1, bop, op2) :: Ir.CondGoto(Var res', op3, bop_cond, label) :: rest 
      when res = res' && String.starts_with ~prefix:"t" res ->
    
    (* 1. 计算 BinOp 的结果，直接放入 t0 寄存器 *)
    load_operand env "t0" op1;
    load_operand env "t1" op2;
    (match bop with
     | Add -> emit env "  add t0, t0, t1"
     | Sub -> emit env "  sub t0, t0, t1"
     | Mul -> emit env "  mul t0, t0, t1"
     | Div -> emit env "  div t0, t0, t1"
     | Mod -> emit env "  rem t0, t0, t1"
     | And -> emit env "  and t0, t0, t1"
     | Or  -> emit env "  or  t0, t0, t1"
     | Eq  -> emit env "  sub t0, t0, t1; seqz t0, t0"
     | Neq -> emit env "  sub t0, t0, t1; snez t0, t0"
     | Lt  -> emit env "  slt t0, t0, t1"
     | Gt  -> emit env "  sgt t0, t0, t1"
     | Leq -> emit env "  sgt t0, t0, t1; xori t0, t0, 1"
     | Geq -> emit env "  slt t0, t0, t1; xori t0, t0, 1"
    );
    
    (* 2. 加载 CondGoto 的第二个操作数到 t1 *)
    load_operand env "t1" op3;

    (* 3. 直接使用 t0 中的结果进行比较和跳转 *)
    let branch_op = match bop_cond with
      | Eq  -> "beq" | Neq -> "bne" | Lt  -> "blt"
      | Gt  -> "bgt" | Leq -> "ble" | Geq -> "bge"
      | _   -> failwith "Invalid comparison operator in fused CondGoto"
    in
    emit env (Printf.sprintf "  %s t0, t1, %s" branch_op label);
    
    (* 4. 继续处理剩下的指令 *)
    translate_instructions env rest


  | Ir.BinOp(res, op1, bop, op2) :: Ir.Return(Some (Var res')) :: rest when res = res' ->
  (* 这是一个 BinOp + Return 的融合 *)
  load_operand env "t0" op1;
  load_operand env "t1" op2;
  (* 计算结果直接放入 a0 *)
  (match bop with
     | Add -> emit env "  add a0, t0, t1"
     | Sub -> emit env "  sub a0, t0, t1"
     | Mul -> emit env "  mul a0, t0, t1"
     | Div -> emit env "  div a0, t0, t1"
     | Mod -> emit env "  rem a0, t0, t1"
     | And -> emit env "  and a0, t0, t1"
     | Or  -> emit env "  or  a0, t0, t1"
     | Eq  -> emit env "  sub a0, t0, t1; seqz a0, a0"
     | Neq -> emit env "  sub a0, t0, t1; snez a0, a0"
     | Lt  -> emit env "  slt a0, t0, t1"
     | Gt  -> emit env "  sgt a0, t0, t1"
     | Leq -> emit env "  sgt a0, t0, t1; xori a0, a0, 1"
     | Geq -> emit env "  slt a0, t0, t1; xori a0, a0, 1"
    );

  emit env ("  j .L_ret_" ^ env.func_name);
  translate_instructions env rest

  (* Assign + Return -> 优化 return x; *)
  | Ir.Assign(res, op) :: Ir.Return(Some (Var res')) :: rest when res = res' ->
  (* 直接将 op 的值加载到 a0 *)
  load_operand env "a0" op;
  emit env ("  j .L_ret_" ^ env.func_name);
  translate_instructions env rest

  (* ==================================================================== *)
  (* 模式 2: y = a * b; z = y + c -> 融合为 z = a * b + c (乘加)        *)
  (* IR:   y = a * b; z = y + c;                                         *)
  (* ==================================================================== *)
  | Ir.BinOp(y, op_a, Mul, op_b) :: Ir.BinOp(z, Var y', Ast.Add, op_c) :: rest when y = y' ->

    load_operand env "t0" op_a; (* a *)
    load_operand env "t1" op_b; (* b *)
    load_operand env "t2" op_c; (* c *)

    (* 使用 madd 指令: t0 = t0 * t1 + t2 *)
    emit env "  madd t0, t0, t1, t2";

    (* 将最终结果存入 z *)
    store_result env "t0" z;

    translate_instructions env rest

  (* ====================================================================== *)
  (* 模式 3: 复杂地址计算 y = b * c; z = a + y -> 融合为 z = a + b * c  *)
  (* ====================================================================== *)
  | Ir.BinOp(y, op_b, Mul, op_c) :: Ir.BinOp(z, op_a, Ast.Add, Var y') :: rest when y = y' ->

    load_operand env "t0" op_a; (* a (base) *)
    load_operand env "t1" op_b; (* b (index) *)
    load_operand env "t2" op_c; (* c (size) *)

    (* 优化指令序列 *)
    emit env "  mul t1, t1, t2";  (* t1 = index * size *)
    emit env "  add t0, t0, t1";  (* t0 = base + (index * size) *)

    (* 将最终结果存入 z *)
    store_result env "t0" z;

    translate_instructions env rest
  
  (* === 处理作用域标记的 case === *)
  (* 当看到作用域标记时，不能再进行跨边界的融合，*)
  (* 所以简单地处理掉这个标记，然后继续处理剩下的指令。*)
  | Ir.ScopeBegin :: rest ->
    (* 这里可以添加一些与作用域相关的逻辑，比如清空寄存器缓存 *)
    (* 但对于修复 Bug 而言，直接忽略并继续即可 *)
    translate_instructions env rest
  
  | Ir.ScopeEnd :: rest ->
    (* 同上 *)
    translate_instructions env rest

  (* =============================================== *)
  (* 如果没有匹配到任何优化模式，就回退到单条翻译 *)
  (* =============================================== *)
  | instr :: rest ->
    translate_single_instruction env instr; (* 调用后备的单指令翻译函数 *)
    translate_instructions env rest
  
  | [] -> ()

(* 分析一个函数的所有指令, 计算栈帧大小并为所有变量分配栈空间 *)
let allocate_stack_frame (env: env) (func: ir_func) =
  let saved_regs_size = 8 in (* 为 ra 和 s0 固定预留 8 字节 *)
  
  let allocate_var var_name =
    if not (Hashtbl.mem env.stack_map var_name) then (
      env.stack_size <- env.stack_size + 4;
      (* 变量的偏移量从 -12 开始，在预留的 8 字节之下 *)
      let offset = -(env.stack_size + saved_regs_size) in
      Hashtbl.add env.stack_map var_name offset
    )
  in
  
  List.iter allocate_var func.params;
  List.iter (fun instr ->
    match instr with
    | Ir.Assign (v, _) | Ir.BinOp (v, _, _, _) | Ir.UnOp (v, _, _) -> allocate_var v
    | Ir.Call (Some v, _, _) -> allocate_var v
    | Ir.Arg (_, v) -> allocate_var v
    | _ -> ()
  ) func.instructions

let translate_function (ir_func: ir_func) : string =
  let env = create_env () in
  env.func_name <- ir_func.name;

  (* 步骤 1: 计算变量所需的空间 *)
  allocate_stack_frame env ir_func;
  
  (* 步骤 2: 计算最终的、对齐的总栈帧大小 *)
  let total_stack_size =
    let size = env.stack_size + 8 in (* 变量空间 + 保存 ra/s0 的 8 字节 *)
    (size + 15) / 16 * 16 (* 向上舍入到 16 的倍数 *)
  in
  
  (* 步骤 3: 生成函数头部和序言 (Prologue) *)
  emit env ("\n.globl " ^ ir_func.name);
  emit env ".text";
  emit env (ir_func.name ^ ":");

  emit env (Printf.sprintf "  addi sp, sp, %d" (-total_stack_size));
  
  (* 关键修改：使用相对于总栈帧大小的、固定的正偏移量来保存 ra 和 s0 *)
  emit env (Printf.sprintf "  sw ra, %d(sp)" (total_stack_size - 4));
  emit env (Printf.sprintf "  sw s0, %d(sp)" (total_stack_size - 8));
  
  (* 设置新的帧指针 s0 *)
  emit env (Printf.sprintf "  addi s0, sp, %d" total_stack_size);
  
  (* 步骤 4: 调用主翻译循环 *)
  translate_instructions env ir_func.instructions;

  (* 步骤 5: 生成函数尾声 (Epilogue) *)
  emit env (".L_ret_" ^ ir_func.name ^ ":");
  
  (* 从同样固定的位置恢复寄存器 *)
  emit env (Printf.sprintf "  lw ra, %d(sp)" (total_stack_size - 4));
  emit env (Printf.sprintf "  lw s0, %d(sp)" (total_stack_size - 8));
  
  emit env (Printf.sprintf "  addi sp, sp, %d" total_stack_size);
  emit env "  ret";
  
  String.concat "\n" (List.rev env.asm_code)

(* 主入口函数: 翻译整个 IR 程序 *)
let generate_from_ir (prog: ir_program) : string =
  let code_parts = List.map translate_function prog in
  String.concat "\n" code_parts