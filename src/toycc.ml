(* main.ml -- ToyC 编译器的驱动程序 (最终整合版) *)

(* 命令行参数的简单解析 *)
let input_file = ref ""
let output_file = ref ""
let optimize_enabled = ref false (* 用于 -opt 标志的布尔引用 *)

let speclist = Arg.align [
  ("-o", Arg.Set_string output_file, " <file>  指定输出文件名 (默认为 stdout)");
  ("-opt", Arg.Set optimize_enabled, "         开启优化 ");
]

(* --- 定义用法信息和匿名参数处理函数 --- *)
let usage_msg = "用法: toycc [源文件] [-o <输出文件>] [-opt]\n如果省略 [源文件]，则从标准输入 stdin 读取。"

(* 解析匿名参数（即没有 -flag 的参数，通常是输入文件名） *)
let anon_fun filename =
  (* 只接受第一个匿名参数作为输入文件 *)
  if !input_file = "" then
    input_file := filename
  else
    (* 如果已经有输入文件了，再来一个就是错误 *)
    raise (Arg.Bad ("只能指定一个输入源文件，多余的参数: '" ^ filename ^ "'"))

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
  (* --- 决定输入通道 --- *)
  let in_channel, close_in_channel =
    if !input_file = "" then
      (stdin, fun () -> ()) (* 从 stdin 读取，关闭通道是无操作 *)
    else
       try
        (* 1. 只打开一次文件 *)
        let channel = open_in !input_file in
        (* 2. 创建一个闭包，它捕获了这个已经打开的 channel *)
        (channel, fun () -> close_in_noerr channel)
      with Sys_error msg ->
        Printf.eprintf "错误: 无法打开输入文件 '%s': %s\n" !input_file msg;
        exit 1
  in
  
  (* --- 决定输出通道 --- *)
  let out_channel, close_out_channel =
    if !output_file = "" then
      (stdout, fun () -> ()) (* 向 stdout 写入，关闭是无操作 *)
    else
      try
        (* 1. 只打开一次文件 *)
        let channel = open_out !output_file in
        (* 2. 创建一个闭包，它捕获了这个已经打开的 channel *)
        (channel, fun () -> close_out_noerr channel)
      with Sys_error msg ->
        Printf.eprintf "错误: 无法创建输出文件 '%s': %s\n" !output_file msg;
        exit 1
  in

  let lexbuf = Lexing.from_channel in_channel in

  try
    (* --- 前端 --- *)
    let ast = Parser.program Lexer.token lexbuf in
    Semantic.check_program ast;

    (* --- 后端 --- *)
    let ir = Ir_gen.generate_ir ast in

    (* 如果开启了优化，可以在这里插入优化 pass *)
    let final_ir =
      if !optimize_enabled then (
        Printf.eprintf "(-opt 标志被识别，但优化未实现)\n"; (* 向 stderr 打印提示信息 *)
        ir
      ) else (
        ir
      )
    in

    let assembly_code = Codegen.generate_from_ir final_ir in
    
    (* --- 写入输出 --- *)
    Printf.fprintf out_channel "%s" assembly_code;
    flush out_channel; (* 确保内容被立即写入，特别是对于 stdout *)

    (* --- 清理工作 --- *)
    close_in_channel ();
    close_out_channel ()

  with
  (* 错误处理部分保持不变，但要确保在退出前关闭文件 *)
  | Lexer.LexerError msg ->
      Printf.eprintf "[错误] 词法错误在 %s: %s\n" (string_of_position lexbuf.lex_curr_p) msg;
      close_in_channel (); close_out_channel ();
      exit 1
  | Semantic.SemanticError msg ->
      Printf.eprintf "[错误] 语义错误: %s\n" msg;
      close_in_channel (); close_out_channel ();
      exit 1
  | Failure msg ->
      Printf.eprintf "[内部错误] 编译中止: %s\n" msg;
      close_in_channel (); close_out_channel ();
      exit 1
  | e ->
      let pos_str = string_of_position lexbuf.lex_curr_p in
      (match Printexc.to_string e with
       | "Parser.Error" ->
         Printf.eprintf "[错误] 语法错误在 %s 附近\n" pos_str
       | _ ->
         Printf.eprintf "[严重错误] 发生未知异常: %s (在 %s 附近)\n" (Printexc.to_string e) pos_str
      );
      close_in_channel (); close_out_channel ();
      exit 1


(* -------------------------------------------------------------------------- *)
(* 程序入口点                                                                 *)
(* -------------------------------------------------------------------------- *)
let () =
  (* 解析命令行参数 *)
  Arg.parse speclist anon_fun usage_msg;
  
  (* 开始编译流程 *)
  compile ()