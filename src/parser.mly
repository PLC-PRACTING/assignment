
%{
  open Ast
%}


%token <int> INT_LITERAL  
%token <string> ID

%token INT_KEYWORD VOID_KEYWORD
%token IF ELSE WHILE BREAK CONTINUE RETURN

%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA

%token ASSIGN
%token PLUS MINUS MUL DIV MOD
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT

%token EOF 


%right ASSIGN                /* 赋值运算符, 右结合 */
%left OR                    /* || */
%left AND                   /* && */
%left EQ NEQ                /* == != */
%left LT GT LEQ GEQ         /* < > <= >= */
%left PLUS MINUS            /* + - */
%left MUL DIV MOD           /* * / % */
%right NOT UMINUS UPLUS     /* 逻辑非、负号、正号 (一元运算符), 右结合, 优先级最高 */
/* %prec UMINUS 用于解决 "a * -b" 的歧义，让-作为一元负号而不是二元减号 */


/* ================== 开始符号和类型定义 ================== */
%start program  /* 定义语法的开始符号为 program */
%type <Ast.program> program /* 指定 program 非终结符返回的 OCaml 类型 */

/* 为其他会返回值的非终结符也指定类型 */
%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%type <Ast.stmt> stmt
%type <Ast.stmt list> stmt_list
%type <Ast.func_def> func_def
%type <Ast.func_def list> funcs_def
%type <Ast.param list> param_list_opt
%type <Ast.ret_type> ret_type

%% 
/* 语法规则部分*/

/* ================== 顶层规则 ================== */
program:
  /* 一个程序是一个或多个函数定义 */
  | funcs_def { Program($1) }
  ;

funcs_def:
  | func_def { [$1] } /* 单个函数定义 */
  | funcs_def func_def { $2 :: $1 } /* 函数定义列表，后出现的函数加在列表前面 */
  ;

/* ================== 函数定义 ================== */
func_def:
  /* ret_type ID ( param_list_opt ) { stmt_list } */
  | ret_type ID LPAREN param_list_opt RPAREN LBRACE stmt_list RBRACE
    { FuncDef($1, $2, List.rev $4, Block(List.rev $7)) }
    /* 注意: 因为我们用 a::b 的方式构建列表，结果是反的, 所以需要用 List.rev 纠正顺序 */
  ;

ret_type:
  | INT_KEYWORD  { TInt }
  | VOID_KEYWORD { TVoid }
  ;

param_list_opt:
  /* 可选的参数列表 */
  | /* 空 */ { [] }
  | INT_KEYWORD ID { [Param($2)] }
  | param_list_opt COMMA INT_KEYWORD ID { Param($4) :: $1 }
  ;

/* ================== 语句 (Statement) ================== */
stmt:
  | SEMICOLON                        { Empty }
  | expr SEMICOLON                   { ExprStmt($1) }
  | INT_KEYWORD ID ASSIGN expr SEMICOLON { VarDecl($2, $4) }
  | ID ASSIGN expr SEMICOLON         { Assign($1, $3) }
  | LBRACE stmt_list RBRACE          { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt       { If($3, $5, None) } /* if, 无 else */
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, Some($7)) } /* if-else */
  | WHILE LPAREN expr RPAREN stmt    { While($3, $5) }
  | BREAK SEMICOLON                  { Break }
  | CONTINUE SEMICOLON               { Continue }
  | RETURN SEMICOLON                 { Return(None) }
  | RETURN expr SEMICOLON            { Return(Some($2)) }
  ;

stmt_list:
  /* 语句列表, 可以为空 */
  | /* 空 */ { [] }
  | stmt_list stmt { $2 :: $1 } /* 将新语句加到列表头部 */
  ;

/* ================== 表达式 (Expression) ================== */
expr:
  | INT_LITERAL               { ConstInt($1) }
  | ID                        { Var($1) }
  | ID LPAREN RPAREN          { Call($1, []) } /* 无参数函数调用 */
  | ID LPAREN expr_list RPAREN { Call($1, List.rev $3) } /* 带参数函数调用 */
  | LPAREN expr RPAREN        { $2 } /* 括号表达式, 直接返回内部表达式的 AST */
  /* 一元运算 */
  | MINUS expr %prec UMINUS   { UnaryOp(Uminus, $2) }
  | PLUS expr %prec UPLUS     { UnaryOp(Uplus, $2) }
  | NOT expr                  { UnaryOp(Unot, $2) }
  /* 二元运算, yacc 会根据上面的优先级定义自动处理结合 */
  | expr PLUS expr            { BinOp(Add, $1, $3) }
  | expr MINUS expr           { BinOp(Sub, $1, $3) }
  | expr MUL expr             { BinOp(Mul, $1, $3) }
  | expr DIV expr             { BinOp(Div, $1, $3) }
  | expr MOD expr             { BinOp(Mod, $1, $3) }
  | expr EQ expr              { BinOp(Eq, $1, $3) }
  | expr NEQ expr             { BinOp(Neq, $1, $3) }
  | expr LT expr              { BinOp(Lt, $1, $3) }
  | expr GT expr              { BinOp(Gt, $1, $3) }
  | expr LEQ expr             { BinOp(Leq, $1, $3) }
  | expr GEQ expr             { BinOp(Geq, $1, $3) }
  | expr AND expr             { BinOp(And, $1, $3) }
  | expr OR expr              { BinOp(Or, $1, $3) }
  ;

expr_list:
  /* 函数调用的参数列表 */
  | expr { [$1] }
  | expr_list COMMA expr { $3 :: $1 }
  ;

%%