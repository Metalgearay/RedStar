let file = Sys.argv.(1)
let ic = open_in file 
let _ =
  let lexbuf = Lexing.from_channel ic in
  let ast = Parser.program Lexer.token lexbuf in 
	print_string (Ast.string_of_program ast);
	close_in ic