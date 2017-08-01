let file = Sys.argv.(1)
let output_file = file^".ll"
let ic = open_in file 
let _ =
  let lexbuf = Lexing.from_channel ic in
  let ast = Parser.program Lexer.token lexbuf in 
	let m = Codegen.ir_dump ast in
	Llvm_analysis.assert_valid_module m;
	Llvm.print_module output_file m;
	close_in ic