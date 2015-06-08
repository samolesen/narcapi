(** Abstract Syntax Tree *)

type term =
	| Variable of string
	| Tuple of tuple
	| Function of string * tuple
and tuple = term list


type equation = Equation of term * term


type exchange = Exchange of string * string * term


type access_level =
	| Public
	| Private

type signature_table = (string, int) Hashtbl.t

type name_table = (string, access_level) Hashtbl.t

(* Piece of knowledge: expectation -> representation *)
type knowledge_table = (term, term) Hashtbl.t

type agent_table = (string, knowledge_table) Hashtbl.t

type narration =
	{
		signatures : signature_table;
		equations : equation list;
		names : name_table;
		agents : agent_table;
		exchanges : exchange list
	}


let subterms = function
	| Function (_,es) | Tuple es -> es
	| Variable _ -> failwith "Variables have no subterms."

open Helper

let rec term_variable_iter f = function
	| Function (_,terms) | Tuple (terms) -> List.iter (term_variable_iter f) terms
	| Variable (name) -> f name

let rec term_function_iter f = function
	| Function (name,terms) -> f name (List.length terms); List.iter (term_function_iter f) terms
	| Tuple (terms) -> List.iter (term_function_iter f) terms
	| Variable (_) -> ()

let rec theory_function_iter f = function
	(* | FunctionSymbol (name, arity) -> f name arity *)
	| Equation (term1, term2) -> term_function_iter f term1; term_function_iter f term2

(* let add_distinct knowledge_table duplicate_table k v =
	if Hashtbl.mem knowledge_table k then (
		if Hashtbl.find knowledge_table k <> v
			&&
			List.fold_right
				(fun x a -> if a then x<>v else a)
				(Hashtbl.find_all duplicate_table k) true
		then Hashtbl.add duplicate_table k v
		else print_endline ("Duplicate: ("^string_of_term k^","^string_of_term v^") already exist in knowledge.")
	)
	else
		Hashtbl.add knowledge_table k v *)

let rec string_of_term = function
	| Variable (v) -> v
	| Tuple (e) -> create_tuple_string (List.map string_of_term e)
	| Function (f,e) -> f ^ create_tuple_string (List.map string_of_term e)

let string_of_equation = function
	(* | FunctionSymbol (f, a) -> f ^ "/" ^ string_of_int a *)
	| Equation (e1,e2) -> string_of_term e1 ^ " = " ^ string_of_term e2

let string_of_knowledge knowledge =
	"{ "^
	Hashtbl.fold
		(fun exp rep a -> "\n  " ^ string_of_term exp ^ " * " ^ string_of_term rep ^ a)
		knowledge (if Hashtbl.length knowledge > 0 then "\n" else "")
	^"}"

let string_of_agents t = newline_separate (list_of_table (fun k v -> k^": "^string_of_knowledge v) t)

let string_of_access_level = function
	| Private -> "private"
	| Public -> "public"

let string_of_names t =
	newline_separate (list_of_table (fun k v -> string_of_access_level v ^ " " ^ k) t)

let string_of_exchange = function
	| Exchange (a1,a2,e) -> a1 ^ " -> " ^ a2 ^ " : " ^ string_of_term e

let string_of_narration n =
		"(* Equational theory *)\n" ^
		newline_separate (List.map string_of_equation n.equations) ^ "\n\n" ^
		"(* Principals *)\n" ^
		string_of_agents n.agents ^ "\n\n" ^
		"(* Names *)\n" ^
		string_of_names n.names ^ "\n\n" ^
		(* "(* Declarations *)\n" ^
		newline_separate (List.map string_of_declaration de) ^ "\n\n" ^ *)
		"(* Exchanges *)\n" ^
		newline_separate (List.map string_of_exchange n.exchanges)

let print_knowledge knowledge =
	print_endline (string_of_knowledge knowledge)

let print_equations equations =
	print_endline "{";
	List.iter
		(fun eq -> print_endline ("  " ^ string_of_equation eq))
		equations;
	print_endline "}"

let print_narration n = print_endline (string_of_narration n)

let print_equations equations =
	print_endline "{";
	List.iter
		(fun eq -> print_endline ("  " ^ string_of_equation eq))
		equations;
	print_endline "}"