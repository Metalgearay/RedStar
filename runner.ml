type action = Ast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Lexer.token lexbuf in 
  match action with
    Ast -> print_string (Ast.string_of_program ast)
  | LLVM_IR -> print_string("nada")
  | Compile -> print_string("nada")
    
