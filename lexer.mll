{open Parser
 let string_func s =
Scanf.sscanf("\"" ^ s ^ "\"") "%S%!" (fun y ->y )
}
let printable = [' ' -'~'] 
let escapes = "\\n" | "\\t"  | "\\v"          
            |"\\b"  | "\\r"  | "\\f"
            |"\\a"  | "\\\\" | "\\?"
			|"\\'" | "\\\""
let string = '"' ((printable|escapes)* as str) '"'

rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf}
| "**" {comment lexbuf}
| '(' {LPAREN}
| ')' {RPAREN}
| '{' {LBRACE}
| '}' {RBRACE}
| "_Da" {SEMI}
| '+' {PLUS}
| ','      { COMMA }
| '-' {MINUS}
| '/' {DIVIDE}
| '*' {TIMES}
| '<'         { LT }
| "<="        { LEQ }
| '>'         { GT }
| ">=" { GEQ }
| "divison_of_labor" {MOD}
| "is_appointed_to" {ASSIGN}
| "true_equality" {EQ}
| "Nyet" {NEQ}
|"&&" {AND}
| "||" {OR}
| "Captalist" {NOT}
|"you_see_ivan" {IF}
| "or_not" {ELSE}
| "for_the_motherland" {FOR}
| "while_the_capitalist_are_asleep" {WHILE}
| "pass_the_vodka" {RETURN}
| "proletariat" {INT}
| "western_lies" {BOOL}
| "words_of_marx" {STRING}
| "bourgeois" {VOID}
| "true"     {TRUE}
| "false"   {FALSE}
|['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as lxm { ID(lxm) }
| string {STRINGLIT(string_func str )}
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
| "**" {token lexbuf}
| _ {comment lexbuf}