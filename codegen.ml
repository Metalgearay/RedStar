module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let ir_dump (globals,functions) =
	let context = L.global_context() in
	let the_mod = L.create_module context "Comrad"
	and i32_t = L.i32_type context
	and i8_t = L.i8_type context
	and il_t = L.i1_type context
	and void_t = L.void_type context in 

	let resolve_type = function
	A.Int -> i32_t
	|A.Bool -> il_t
	|A.Void -> void_t in

	let global_vars =
		let global_var m (t,n) =
		let init = L.const_int (resolve_type t) 0
	in StringMap.add n(L.define_global n init the_mod) m in
	List.fold_left global_var StringMap.empty globals in

	let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t|] in
	let printf_func = L.declare_function "printf" printf_t the_mod in

	let function_decls =
		let function_decl m fdecl = 
			let name = fdecl.A.fname
			and formal_type =
			Array.of_list (List.map (fun(t,_)-> resolve_type t)fdecl.A.formals)
		in let ftype = L.function_type (resolve_type fdecl.A.typ) formal_type in
			StringMap.add name (L.define_function name ftype the_mod,fdecl) m in
			List.fold_left function_decl StringMap.empty functions in

	let build_fun_body fdecl = 
		let (the_fun, _) = StringMap.find fdecl.A.fname function_decls in
		let builder= L.builder_at_end context (L.entry_block the_fun) in
		let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

		let local_vars = 
			let add_formal m (t,n) p = L.set_value_name n p;
			let local = L.build_alloca (resolve_type t) n builder in 
			ignore (L.build_store p local builder);
			StringMap.add n local m in

			let add_local m (t,n)  = 
			let local_var = L.build_alloca (resolve_type t) n builder 
		in StringMap.add n local_var m in 

		let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
		(Array.to_list(L.params the_fun)) in
		List.fold_left add_local formals fdecl.A.locals in

		let lookup n = try StringMap.find n local_vars
							with Not_found -> StringMap.find n global_vars 
			in 

			let rec exper builder = function
			A.IntLit i -> L.const_int i32_t i 
			|A.BoolLit b -> L.const_int il_t (if b then 1 else 0)
			|A.Noexpr -> L.const_int i32_t 0
			|A.Id s -> L.build_load (lookup s) s builder 
			|A.Binop (e1,op,e2) ->
			let e1' = exper builder e1 
			and e2' = exper builder e2 in
			(match op with
			A.Add -> L.build_add
			|A.Sub -> L.build_sub 
			|A.Mult -> L.build_mul
			|A.Div -> L.build_sdiv
			| A.And     -> L.build_and
	  		| A.Or      -> L.build_or
	  		| A.Equal   -> L.build_icmp L.Icmp.Eq
	  		| A.Neq     -> L.build_icmp L.Icmp.Ne
			) e1' e2' "tmp" builder
			|A.Unop (op, e) ->
			let e' = exper builder e in
			(match op with
				A.Neg -> L.build_neg
			|A.Not -> L.build_not) e' "tmp" builder
			
			|A.Assign(s,e) -> let e' = exper builder e in
						ignore(L.build_store e' (lookup s ) builder); e'
			|A.Call ("wordsOfMarx",[e]) | A.Call("truths_of_stalin",[e]) ->
			L.build_call printf_func [|int_format_str; (exper builder e)|]
			"printf" builder 
			|A.Call (f,act) ->
				let (fdef,fdecl) = StringMap.find f function_decls in
				let actuals = List.rev(List.map (exper builder)(List.rev act)) in
				let result = (match fdecl.A.typ with A.Void -> ""
								|_-> f ^"_result") in
				L.build_call fdef(Array.of_list actuals) result builder
			in

			let add_term builder f =
			match L.block_terminator (L.insertion_block builder ) with 
			Some _ -> ()
			|None -> ignore (f builder ) in 

			let rec stmt builder = function 
			A.Block sl -> List.fold_left stmt builder sl 
			|A.Expr e -> ignore (exper builder e); builder 
			|A.Return e -> ignore (match fdecl.A.typ with
				A.Void -> L.build_ret_void builder
				|_-> L.build_ret (exper builder e) builder); builder
				|A.If(predicate,then_stmt,else_stmt) ->
					let bool_val = exper builder predicate in
					let merge_bb = L.append_block context "merge" the_fun in
					let then_bb = L.append_block context "then" the_fun in 
					add_term(stmt (L.builder_at_end context then_bb)then_stmt)
					(L.build_br merge_bb);

					let else_bb = L.append_block context "else" the_fun in
					add_term (stmt (L.builder_at_end context else_bb) else_stmt)
					(L.build_br merge_bb);

					ignore(L.build_cond_br bool_val then_bb else_bb builder);
					L.builder_at_end context merge_bb

				|A.While (predicate,body) ->
					let pred_bb = L.append_block context "while" the_fun in
					ignore(L.build_br pred_bb builder);

					let body_bb = L.append_block context "while_body" the_fun in
					add_term(stmt (L.builder_at_end context body_bb)body)
					(L.build_br pred_bb);

					let  pred_builder = L.builder_at_end context pred_bb in 
					let bool_val= exper pred_builder predicate in

					let merge_bb = L.append_block context "merge" the_fun in
					ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
					L.builder_at_end context merge_bb

					|A.For(e1,e2,e3,body) -> stmt builder
					(A.Block[A.Expr e1; A.While(e2,A.Block[body;A.Expr e3])])
				in
				let builder = stmt builder (A.Block fdecl.A.body) in 

				add_term builder(match fdecl.A.typ with
					A.Void -> L.build_ret_void 
					| t-> L.build_ret (L.const_int (resolve_type t )0 )
				)
			in
			List.iter build_fun_body functions;
			the_mod

			