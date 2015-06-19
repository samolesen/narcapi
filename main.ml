open Helper
open Tree

let parse filename = 
	try
		let ic = open_in filename in
		let lexbuf = Lexing.from_channel ic in
		lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
		let narration =
			try
				Parser.narration Lexer.token lexbuf
			with Parsing.Parse_error ->
				raise_error "Syntax error" in
		close_in ic;
		narration
	with Sys_error s ->
		raise_error ("Regarding file " ^ s)


let () =
	if Array.length Sys.argv = 2 then
		let filename = Sys.argv.(1) in
		let narration = parse filename in
		Appliedpi.print_translation narration
	else
		print_endline ("Usage: " ^ Sys.argv.(0) ^ " PATH\nWhere PATH is the path/filename of an equational protocol narration.")