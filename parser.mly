%{
open Tree

let signature_table = Hashtbl.create 10
let name_table = Hashtbl.create 10
let agent_table = Hashtbl.create 10

let add_function ?(warning=false) func arity =
	match Hashtbl.mem signature_table func with
	| false -> if warning then print_endline ("WARNING: Function \""^func^"\" is not explicitly defined."); Hashtbl.add signature_table func arity
	| true -> if Hashtbl.find signature_table func <> arity then
		print_endline ("ERROR: Function \""^func^"\" is used with conflicting numbers of arguments.")


let check_function func arity =
	try 
		if Hashtbl.find signature_table func <> arity then
			print_endline ("ERROR: Function \""^func^"\" is used with conflicting numbers of arguments.")
	with Not_found -> print_endline ("ERROR: Function \""^func^"\" is not defined.")

type table_search_result =
	| Both
	| Agent
	| Name
	| None

let exist_in_tables e =
	match (Hashtbl.mem name_table e, Hashtbl.mem agent_table e) with
	| (true,true) -> Both
	| (false,true) -> Agent
	| (true,false) -> Name
	| (false,false) -> None

let add_private e =
	match exist_in_tables e with
	| None -> Hashtbl.add name_table e Private
	| Name -> if Hashtbl.find name_table e = Private || Hashtbl.find name_table e = Public then Hashtbl.replace name_table e Private
	| Agent | Both -> Hashtbl.replace name_table e Private

let add_public e =
	match exist_in_tables e with
	| None | Agent -> Hashtbl.add name_table e Public
	| Name | Both -> ()

let add_generated e a =
	match exist_in_tables e with
	| None -> Hashtbl.add name_table e (Generated a)
	| Name -> if Hashtbl.find name_table e = Private || Hashtbl.find name_table e = Public then failwith (e ^ " cannot be generated due to previous declaration.")
	| Agent | Both -> failwith (e ^ " cannot be generated due to being an agent.")

let add_agent a =
	match exist_in_tables a with
	| None | Name ->
		begin try
			if Hashtbl.find name_table a <> Private && Hashtbl.find name_table a <> Public then failwith (a ^ " cannot be an agent due to being generated.")
		with Not_found -> () end;
		Hashtbl.add agent_table a (Hashtbl.create 10); add_public a
	| Agent | Both -> ()
%}

%token <string> IDENT
%token <int> INTEGER
%token EQUAL
%token LPAREN RPAREN
%token COMMA
%token SLASH
%token SEPARATOR
%token NEWLINE
%token ARROW
%token COLON
%token KNOW
%token PRIVATE
%token GENERATES
%token SHARE
%token DOT
%token EOF
%start narration_dot
%type <Tree.narration> narration_dot

%%

term:
	| IDENT LPAREN optional_termlist RPAREN  { Function ($1, $3) }
	| LPAREN optional_termlist RPAREN        { Tuple $2 }
	| IDENT                                  { Variable $1 }

termlist:
	| term COMMA termlist  { $1 :: $3 }
	| term                 { [$1] }
	/* | error { print_string "Comma trouble"; [Variable "x"] } */

optional_termlist:
	| termlist  { $1 }
	|           { [] }

optional_newline:
	| NEWLINE  {}
	|          {}

sep:
	| NEWLINE sep    {}
	| SEPARATOR sep  {}
	| NEWLINE        {}
	| SEPARATOR      {}

optional_sep:
	| sep  {}
	|      {}

equation:
	| term optional_newline EQUAL optional_newline term 
		{
			let function_definitions term =
				let add_definition func arity acc =
					let result = if Hashtbl.mem signature_table func then acc else FunctionSymbol (func, arity) :: acc in
					add_function ~warning:true func arity;
					result in
				term_function_fold add_definition [] term in
			Equation ($1, $5) ::	(function_definitions $5) @ (function_definitions $1)
		}
	| IDENT SLASH INTEGER  { add_function $1 $3; [FunctionSymbol ($1, $3)] }
	/* | error SEPARATOR { print_string "It was not that bad.."; Equation (Variable "x", Variable "y") }*/

equationlist:
	| equationlist sep equation  { $3 @ $1 }
	| equation                   { $1 }

agentlist:
	| IDENT COMMA agentlist  { $1 :: $3 }
	| IDENT                  { [$1] }

declaration:
	| IDENT GENERATES IDENT  { add_agent $1; add_generated $3 $1; Hashtbl.add (Hashtbl.find agent_table $1) (Variable $3) (Variable $3) }
	| PRIVATE IDENT          { add_private $2 }
	| agentlist KNOW term    { term_function_iter check_function $3; term_variable_iter add_public $3; List.iter (fun a -> add_agent a; Hashtbl.add (Hashtbl.find agent_table a) $3 $3) $1 }
	| agentlist SHARE IDENT  { List.iter add_agent $1; add_private $3; List.iter (fun a -> add_agent a; Hashtbl.add (Hashtbl.find agent_table a) (Variable $3) (Variable $3)) $1 }

declarationlist:
	| declarationlist sep declaration  { $3 :: $1 }
	| declaration                      { [$1] }

exchange:
	| IDENT ARROW IDENT COLON term
		{
			add_agent $1; add_agent $3;
			term_variable_iter
				(fun x ->
					if not (Hashtbl.mem name_table x) then (* Replace with check in $1's knowledge *)
						print_endline ("ERROR: \"" ^ x ^ "\" is not a name or an agent."))
				$5;
			term_function_iter check_function $5;
			Exchange ($1, $3, $5) }

exchangelist:
	| exchangelist sep exchange  { $3 :: $1 }
	| exchange                   { [$1] }

narration2:
	| declarationlist sep exchangelist optional_sep  { $3 }
	| declarationlist optional_sep                   { [] }
	| exchangelist optional_sep                      { $1 }

narration1:
	| equationlist sep narration2  { ($1,$3) }
	| equationlist optional_sep    { ($1,[]) }
	| narration2                   { ([],$1) }
	|                              { ([],[]) }

narration:
	optional_sep narration1
		{
			let (eq,ex) = $2 in
			let new_signature_table = Hashtbl.copy signature_table in
			let new_name_table = Hashtbl.copy name_table in
			let new_agent_table = Hashtbl.copy agent_table in
			(* Reset hashtables for future parsing *)
			Hashtbl.reset signature_table;
			Hashtbl.reset name_table;
			Hashtbl.reset agent_table;

			(* Build narration record *)
			{
				signatures = new_signature_table;
				equational_theory = List.rev eq;
				equations = get_equations eq;
				names = new_name_table;
				agents = new_agent_table;
				exchanges = List.rev ex
			}
		}

narration_dot:
	narration DOT { $1 }