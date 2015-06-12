open Tree

let () =
	while true do
		print_endline "#################################";
		let rec prompt_string str =
			let s = read_line () in
			if String.contains s '.' then str ^ s
			else prompt_string (str ^ s ^ "\n") in
		let lexbuf = Lexing.from_string (prompt_string "") in
		let narration = Parser.narration_dot Lexer.token lexbuf in
		print_endline "---------------------------------";
		Hashtbl.iter (fun _ k -> Knowledge.normalize_knowledge k narration.equations) narration.agents;
		Appliedpi.print_translation narration;
		print_newline ()
	done