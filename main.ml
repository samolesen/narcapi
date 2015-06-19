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
	(
		print_endline ("Usage: " ^ Sys.argv.(0) ^ " INPUTPATH [> OUTPUTPATH]");
		print_newline ();
		print_endline "INPUTPATH is the path/filename of an equational protocol narration";
		print_endline "OUTPUTPATH is the path/filename for the resulting applied pi process.";
		print_newline ();
		print_endline ("Example: " ^ Sys.argv.(0) ^ " examples/diffiehellman.epn > examples/diffiehellman.pv")
	)