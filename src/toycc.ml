(* main.ml -- ToyC 编译器的驱动程序 (最终整合版) *)

(* 命令行参数的简单解析 *)
let input_file = ref ""
let output_file = ref ""

let usage_msg = "toycc <源文件.tc> [-o <输出文件.s>]"

let speclist =
  [("-o", Arg.Set_string output_file, "指定输出文件名")]

let anon_fun filename =
  input_file := filename

(* -------------------------------------------------------------------------- *)
(* 辅助函数: 用于打印错误信息的位置                                               *)
(* -------------------------------------------------------------------------- *)
let string_of_position (pos: Lexing.position) : string =
  Printf.sprintf "第 %d 行, 第 %d 列"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* -------------------------------------------------------------------------- *)
(* 编译器主函数                                                               *)
(* -------------------------------------------------------------------------- *)
let compile () =
  (* 1. 打开输入文件 *)
  let in_channel = open_in !input_file in
  let lexbuf = Lexing.from_channel in_channel in

  try
    (* ======================== 前端阶段 ======================== *)
    
    (* 2. 词法和语法分析 *)
    print_endline "1. [前端] 正在进行词法和语法分析...";
    let ast = Parser.program Lexer.token lexbuf in
    print_endline "   -> 分析成功, AST 已生成。";

    (* 3. 语义分析 *)
    print_endline "2. [前端] 正在进行语义分析...";
    Semantic.check_program ast;
    print_endline "   -> 语义分析通过, 程序符合 ToyC 规范。";

    (* ======================== 后端阶段 ======================== *)
    
    (* 4. 中间代码生成 (IR) *)
    print_endline "3. [后端] 正在从 AST 生成中间代码 (IR)...";
    let ir = Ir_gen.generate_ir ast in
    print_endline "   -> IR 生成完毕。";
    
    (*
       在这里可以插入一个可选的优化阶段:
       print_endline "4. [后端] 正在优化 IR...";
       let optimized_ir = Optimize.run ir in
       print_endline "   -> IR 优化完成。";
    *)

    (* 5. 目标代码生成 (RISC-V) *)
    print_endline "4. [后端] 正在从 IR 生成 RISC-V 汇编代码...";
    let assembly_code = Codegen.generate_from_ir ir in (* 调用我们新加的模块 *)
    print_endline "   -> 汇编代码已生成。";


    (* 6. 写入输出文件 *)
    let out_channel = open_out !output_file in
    Printf.fprintf out_channel "%s\n" assembly_code; (* 在文件末尾加个换行符 *)
    close_out out_channel;
    
    print_endline ("\n编译成功! 输出文件已保存至: " ^ !output_file);

    (* 清理工作 *)
    close_in in_channel

  with
  (* 捕获并处理各个阶段的错误 *)
  | Lexer.LexerError msg ->
      Printf.eprintf "\n[错误] 词法错误在 %s: %s\n" (string_of_position lexbuf.lex_curr_p) msg;
      close_in_noerr in_channel;
      exit 1
  | Semantic.SemanticError msg ->
      Printf.eprintf "\n[错误] 语义错误: %s\n" msg;
      close_in_noerr in_channel;
      exit 1
  | Failure msg -> (* 捕获例如 break/continue 未实现的 'failwith' *)
      Printf.eprintf "\n[内部错误] 编译中止: %s\n" msg;
      close_in_noerr in_channel;
      exit 1
  | e -> (* 捕获所有其他异常，包括私有的 Parser.Error *)
      let pos_str = string_of_position lexbuf.lex_curr_p in
      (match Printexc.to_string e with
       | "Parser.Error" -> (* ocamlyacc 抛出的异常的字符串形式就是 "Parser.Error" *)
         Printf.eprintf "\n[错误] 语法错误在 %s 附近\n" pos_str
       | _ -> (* 其他未知异常 *)
         Printf.eprintf "\n[严重错误] 发生未知异常: %s (在 %s 附近)\n" (Printexc.to_string e) pos_str
      );
      close_in_noerr in_channel;
      exit 1


(* -------------------------------------------------------------------------- *)
(* 程序入口点                                                                 *)
(* -------------------------------------------------------------------------- *)
let () =
  (* 解析命令行参数 *)
  Arg.parse speclist anon_fun usage_msg;

  (* 检查是否提供了输入文件 *)
  if !input_file = "" then (
    Printf.eprintf "错误: 未指定输入源文件。\n%s\n" usage_msg;
    exit 1
  );

  (* 如果用户未指定输出文件名, 根据输入文件名自动生成 *)
  if !output_file = "" then (
    try
      (* Filename.chop_suffix 在 OCaml 4.04+ 中可用 *)
      output_file := (Filename.chop_suffix !input_file ".tc") ^ ".s"
    with Invalid_argument _ ->
      (* 如果输入文件不以 .tc 结尾, 则简单地附加 .s *)
      output_file := !input_file ^ ".s"
  );
  
  (* 开始编译流程 *)
  compile ()