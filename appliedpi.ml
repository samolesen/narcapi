open Helper
open Tree

let string_of_theory = function
	| Equation (e1,e2) -> "equation " ^ string_of_term e1 ^ " = " ^ string_of_term e2 ^ "."
	| FunctionSymbol (f, a) -> "fun " ^ f ^ "/" ^ string_of_int a ^ "."

let string_of_equations equations =
	newline_separate (List.map string_of_theory equations)

let string_of_checks checks =
	let string_of_check = function
		| Knowledge.Check (left,right) -> "\n  if " ^ string_of_term left ^ " = " ^ string_of_term right ^ " then"
		| Knowledge.Let (left,right) -> "\n  let " ^ string_of_term (Tuple left) ^ " = " ^ string_of_term right ^ " in" in
	String.concat "" (List.map string_of_check checks)

let translate narration =
	Hashtbl.iter (fun _ k -> Knowledge.normalize_knowledge k narration.equations) narration.agents;
	Hashtbl.iter ( fun _ k -> ignore (Knowledge.analyse k narration.equations)) narration.agents;
	let equational_theory = 
		"(* Equational theory *)\n" ^
		string_of_equations narration.equational_theory ^ "\n\n" in
	let agent_processes = Hashtbl.create 10 in
	let add_process_actions = function
		| Exchange (a_agent, b_agent, message) ->
			let a_knowledge = Hashtbl.find narration.agents a_agent in
			let b_knowledge = Hashtbl.find narration.agents b_agent in
			let message_representation = Knowledge.exchange a_agent a_knowledge b_knowledge message narration.equations in
			let checks = string_of_checks (Knowledge.analyse b_knowledge narration.equations) in
			let message_synthesis = List.hd (Knowledge.synthesize a_knowledge narration.equations message) in
			let add_to_process agent str =
				Hashtbl.replace agent_processes agent ((try Hashtbl.find agent_processes agent with Not_found -> "") ^ str) in
			add_to_process a_agent ("\n  out(" ^ b_agent ^ ", " ^ string_of_term message_synthesis ^ ");");
			add_to_process b_agent ("\n  in(" ^ b_agent ^ ", " ^ string_of_term message_representation ^ ");" ^ checks) in
	List.iter add_process_actions narration.exchanges;
	let agent_projections = 
		let build_agent_process agent process =
			let generated_names =
				let add_generated_name k v a =
					match v with
					| Generated ag when agent = ag -> a ^ "\n  new " ^ k ^ ";"
					| _ -> a in
				Hashtbl.fold add_generated_name narration.names "" in
			"let " ^ agent ^ " =" ^ generated_names ^ process ^ " 0." in
		"(* Protocol *)\n" ^ String.concat "\n\n" (list_of_table build_agent_process agent_processes) in
	let global_process =
		let global_restricted_names =
			let add_global_restricted_name k v a =
				match v with
				| Private -> a ^ "  new " ^ k ^ ";\n"
				| _ -> a in
			Hashtbl.fold add_global_restricted_name narration.names "" in
		let parallel_composition = String.concat " | " (list_of_table (fun k v -> k) agent_processes) in
		"\n\nprocess\n" ^ global_restricted_names ^ "  !(" ^ parallel_composition ^ ")" in
	let warnings =
		let rec get_warnings acc = try get_warnings (acc ^ "  " ^ Queue.take raised_warnings ^ "\n") with Queue.Empty -> acc in
		if Queue.length raised_warnings = 0 then ""
		else "(*\n" ^ get_warnings "" ^ "*)\n\n" in
	warnings ^ equational_theory ^ agent_projections ^ global_process

let print_translation narration = print_string (translate narration)