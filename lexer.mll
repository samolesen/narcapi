{
	open Helper
	open Parser

	let keyword_list =
		[
			"generates", GENERATES;
			"private", PRIVATE;
			"knows", KNOW;
			"know", KNOW;
			"share", SHARE
		]

	let keyword_table =
		let table = Hashtbl.create 5 in
		let add_to_table (k,v) = Hashtbl.add table k v in
		List.iter add_to_table keyword_list;
		table
}

rule token = parse
	| [' ' '\t']+    { token lexbuf }   (* Ignored characters *)
	| ';'+           { SEPARATOR }
	| '\n'+          { NEWLINE }
	| '.'            { DOT }
	| '/'            { SLASH }
	| '('            { LPAREN }
	| ')'            { RPAREN }
	| ','            { COMMA }
	| '='            { EQUAL }
	| ':'            { COLON }
	| "->"           { ARROW }
	| eof            { EOF }
	| [ '0'-'9' ]+ as lexeme  { INTEGER(int_of_string lexeme) }
	| ([ 'a'-'z' 'A'-'Z' ] ([ 'a'-'z' 'A'-'Z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9' ])*) as lexeme
		{
			try Hashtbl.find keyword_table (String.lowercase lexeme)
			with Not_found -> IDENT(lexeme) 
		}
	| _ as symbol
		{
			let loc_start = Lexing.lexeme_start_p lexbuf in
			input_error (Printf.sprintf "Illegal character \"%s\" in file \"%s\", at line %d, character %d."
				(String.make 1 symbol)
				loc_start.Lexing.pos_fname
				loc_start.Lexing.pos_lnum
				(loc_start.Lexing.pos_cnum - loc_start.Lexing.pos_bol +1))
		}