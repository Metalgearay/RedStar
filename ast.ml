type op = Add | Sub | Mult | Div | Equal | Neq | And | Or

type uop = Neg | Not

type typ = Int | Bool | Void

type bind = typ * string

type expr =
	IntLit of int
	|BoolLit of bool 
	| Id of string
  	| Binop of expr * op * expr
  	| Unop of uop * expr
  	| Assign of string * expr
  	| Call of string * expr list
  	| Noexpr

type stmt= 
	Block of stmt list
	|Expr of expr
	|Return of expr
	| If of expr * stmt * stmt
	|For of expr * expr * expr * stmt
	|While of expr * stmt 

type funcl_decl= {
	typ : typ;
	fname: string;
	formals: bind list;
	locals: bind list;
	body: stmt list; 
}

type program = bind list * funcl_decl list 


let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "true_equality"
  | Neq -> "Nyet"
  | And -> "&&"
  | Or -> "||"

  let string_of_uop = function
    Neg -> "-"
  | Not -> "Captalist"

  let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " is_appointed_to " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ "_Da\n";
  | Return(expr) -> "pass_the_vodka" ^ string_of_expr expr ^ "_Da\n";
  | If(e, s, Block([])) -> "you_see_ivan (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "you_see_ivan (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "or_not\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for_the_motherland (" ^ string_of_expr e1  ^ " _Da" ^ string_of_expr e2 ^ " _Da " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while_the_capitalist_are_asleep (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "proletariat"
  | Bool -> "western_lies"
  | Void -> "bourgeois"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ "_Da\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
