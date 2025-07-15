(* ir.ml -- 中间表示 (三地址码) 的类型定义 *)

(* 操作数类型: 可以是整数字面量，也可以是变量名 (包括临时变量) *)
type operand =
  | IntLiteral of int   (* e.g., 5 *)
  | Var of string       (* e.g., "x", "t0" *)

(* IR 指令的类型 *)
type instruction =
  | Label of string
  | Assign of string * operand
  | BinOp of string * operand * Ast.binop * operand
  | UnOp of string * Ast.unop * operand
  | Goto of string
  | CondGoto of operand * operand * Ast.binop * string
  | IfZero of operand * string
  | IfNotZero of operand * string
  | ScopeBegin
  | ScopeEnd

  (* === 修改 Param 指令 === *)
  | Param of int * operand              (* 修改：传递函数参数: param index, value *)
  
  | Call of string option * string * int
  | Return of operand option
  | Arg of int * string

(* 单个函数的 IR 表示 *)
and ir_func = {
  name: string;
  params: string list;
  instructions: instruction list;
}

(* 整个程序的 IR 表示 *)
and ir_program = ir_func list