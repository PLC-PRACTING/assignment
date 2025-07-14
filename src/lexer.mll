{
  
  open Parser
  exception LexerError of string
}


(* 这是一个辅助规则, 专门用于匹配多行注释 "/* ... */" 的内容 *)
rule comment = parse
  | "*/"    { token lexbuf } (* 遇到 "*/", 注释结束, 返回到主 token 规则继续分析 *)
  | _       { comment lexbuf } (* 匹配任何其他单个字符, 继续在注释内查找 *)
  | eof     { raise (LexerError "多行注释没有正常结束 (未找到 */)") }


(* 这是主词法分析规则, ocamllex 会从上到下依次尝试匹配 *)
and token = parse

  | [' ' '\t' '\r' '\n']   { token lexbuf } (* 匹配空格、制表符、回车、换行 *)
  | "//" [^'\n']*         { token lexbuf } (* 匹配单行注释: // 直到行尾 *)
  | "/*"                   { comment lexbuf } (* 匹配多行注释的开始, 并切换到 comment 规则 *)

 
  | "int"       { INT_KEYWORD }
  | "void"      { VOID_KEYWORD }
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "break"     { BREAK }
  | "continue"  { CONTINUE }
  | "return"    { RETURN }


   
  | "&&"    { AND }
  | "||"    { OR }
  | "=="    { EQ }
  | "!="    { NEQ }
  | "<="    { LEQ }
  | ">="    { GEQ }
  | '<'     { LT }
  | '>'     { GT }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { MUL }
  | '/'     { DIV }
  | '%'     { MOD }
  | '!'     { NOT }
  | '='     { ASSIGN }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | ';'     { SEMICOLON }
  | ','     { COMMA }


  | '-'? ['0'-'9']+ as num { INT_LITERAL (int_of_string num) }


  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID id }

  | eof     { EOF }

  | _ as char { raise (LexerError ("非法字符: " ^ (String.make 1 char))) }