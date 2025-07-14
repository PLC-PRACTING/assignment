

(* 二元运算符类型 *)
type binop =
  (* 算术运算符: + - * / % *)
  | Add | Sub | Mul | Div | Mod
  (* 关系运算符: == != < > <= >= *)
  | Eq | Neq | Lt | Gt | Leq | Geq
  (* 逻辑运算符: && || *)
  | And | Or

(* 一元运算符类型 *)
type unop =
  | Uplus   (* 正号 + *)
  | Uminus  (* 负号 - *)
  | Unot    (* 逻辑非 ! *)

(* 函数返回类型 *)
type ret_type =
  | TInt
  | TVoid


(* 表达式 (Expression) *)
and expr =
  | ConstInt of int                     (* 整数字面量, e.g., 123, -42 *)
  | Var of string                       (* 变量引用, e.g., x, my_var *)
  | UnaryOp of unop * expr              (* 一元运算, e.g., -x, !flag *)
  | BinOp of binop * expr * expr        (* 二元运算, e.g., a + b, c && d *)
  | Call of string * expr list          (* 函数调用, e.g., foo(a, 10) *)


(* 语句 (Statement) *)
and stmt =
  | Empty                               (* 空语句, e.g., ; *)
  | ExprStmt of expr                    (* 表达式语句, e.g., a + 1; foo(); *)
  | VarDecl of string * expr            (* 变量声明与初始化, e.g., int x = 10; *)
  | Assign of string * expr             (* 赋值语句, e.g., x = y + 1; *)
  | If of expr * stmt * stmt option     (* if-else 语句. 'stmt option' 用于处理可选的 else 分支 *)
  | While of expr * stmt                (* while 循环语句 *)
  | Return of expr option               (* return 语句. 'expr option' 用于处理 'return;' 和 'return expr;' *)
  | Break                               (* break 语句 *)
  | Continue                            (* continue 语句 *)
  | Block of stmt list                  (* 语句块, e.g., { stmt1; stmt2; } *)


(* 函数形参 (Parameter) *)
(* 根据文法 Param -> "int" ID, 类型总是 int, 所以我们只需要存储名字 *)
and param =
  Param of string


(* 函数定义 (Function Definition) *)
and func_def =
  FuncDef of ret_type * string * param list * stmt (* 返回类型, 函数名, 形参列表, 函数体 (是一个 Block 语句) *)


(* 编译单元 (Compilation Unit) - 程序的顶层结构 *)
(* CompUnit -> FuncDef+ *)
type program =
  Program of func_def list