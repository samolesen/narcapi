open Helper
open Tree

module StringMap = Map.Make(String)

type check =
	| Check of term * term
	| Let of term * term


let print_substitution substitution =
	print_endline "{";
	StringMap.iter
		(fun k v -> print_endline ("  " ^ k ^ " -> " ^ string_of_term v))
		substitution;
	print_endline "}"

let string_of_checks checks =
	"{ "^
	List.fold_right
		(fun x a ->
			match x with
			| Check (left,right) -> "\n  [" ^ string_of_term left ^ " = " ^ string_of_term right ^ "]" ^ a
			| Let (left,right) -> "\n  let " ^ string_of_term left ^ " = " ^ string_of_term right ^ a
			)
		checks (if List.length checks > 0 then "\n" else "")
	^"}"

let print_checks checks =
	print_endline (string_of_checks checks)

let same_outer_term t1 t2 =
	match t1,t2 with
	| Function (f1,_), Function (f2,_) when f1=f2 -> true
	| Tuple _, Tuple _ -> true
	| _ -> false

let rec term_unification term1 substitution term2 =
	match (term1, term2) with
	| (Variable x, _) ->
		if StringMap.mem x substitution then
			if (StringMap.find x substitution) = term2 then Some substitution else None
		else Some (StringMap.add x term2 substitution)
	| (Function (_, e1), Function (_,e2)) | (Tuple e1, Tuple e2)
		when same_outer_term term1 term2 ->
			let unify_subterms res x1 x2 =
				match res with Some s -> term_unification x1 s x2 | None -> None in
			List.fold_left2 unify_subterms (Some substitution) e1 e2
	| (_,_) -> None

let rec exist_parameter term predicate =
	match term with
	| Variable x -> predicate x
	| Function (_, e) | Tuple e ->
		List.exists (fun x -> exist_parameter x predicate) e

let exist_substituted_parameter term substitution =
	exist_parameter term (fun x -> StringMap.mem x substitution)

let exist_nonsubstituted_parameter term substitution =
	exist_parameter term (fun x -> not (StringMap.mem x substitution))

let term_fit term (Equation (left,right)) =
	let rec term_fit term = function
		| Variable _ -> []
		| (Function (_, es) | Tuple es) as left ->
			match term_unification left (StringMap.empty) term with
			| Some s -> if exist_nonsubstituted_parameter right s then [] else [s]
			| None -> List.fold_left (fun res e -> res@(term_fit term e)) [] es in
	term_fit term left

let change_inner_terms t e =
	match t with
	| Function (f,_) -> Function (f,e)
	| Tuple _ -> Tuple e
	| Variable _ -> failwith "Variables do not contain inner terms"

let rec apply_substitution term substitution =
	match term with
	| Variable x -> (try StringMap.find x substitution with Not_found -> Tuple [])
	| Function (_, es) | Tuple es ->
		let untitled = List.map (fun e -> apply_substitution e substitution) es in
		change_inner_terms term untitled

let equation_instances term equations =
	let untitled a (Equation (left,right)) =
		match term_unification left (StringMap.empty) term with
		| Some s when not (exist_nonsubstituted_parameter right s) -> apply_substitution right s :: a
		| _ -> a in
	List.fold_left untitled [term] equations

let rec can_synthesize_term knowledge equations term =
	let subterm_syn = function
		| Variable _ -> false
		| Function (_, es) | Tuple es -> List.for_all (can_synthesize_term knowledge equations) es in
	let representation_exists t = Hashtbl.mem knowledge t || subterm_syn t in
	List.exists representation_exists (equation_instances term equations)

let knowledge_substitutions term substitution knowledge equations =
	match term with
	| Variable x ->
		let can_synthesize = can_synthesize_term knowledge equations (StringMap.find x substitution) in
		if StringMap.mem x substitution && can_synthesize then [substitution] else []
	| _ ->
		let unify_with_knowledge k _ a =
			match (term_unification term substitution k) with None -> a | Some m -> m :: a in
		Hashtbl.fold unify_with_knowledge knowledge []

let satisfy_substitution term substitution knowledge equations =
	let rec satisfy_substitution termlist ignoredterms substitution =
		match termlist with
		| [] -> [substitution]
		| term::terms ->
			let term_sub termlist ignoredterms =
				let term_subs = knowledge_substitutions term substitution knowledge equations in
				let satisfy_term_sub res sub =
					(satisfy_substitution termlist ignoredterms sub)@res in
				List.fold_left satisfy_term_sub [] term_subs in
			match term with
			| Function (_,es) | Tuple es ->
				let subterm_sub = satisfy_substitution (List.append es terms) ignoredterms substitution in
				if subterm_sub = [] then (term_sub (ignoredterms@terms) []) else subterm_sub
			| Variable x ->
				if StringMap.mem x substitution then term_sub terms ignoredterms
				else satisfy_substitution terms (term::ignoredterms) substitution in
	satisfy_substitution [term] [] substitution

let find_substitutions equation knowledge equations =
	let Equation (eq_left, eq_right) = equation in
	let untitled1 k _ a =
		let untitled2 a s =
			satisfy_substitution eq_left s knowledge equations @ a in
		List.fold_left untitled2 a (term_fit k equation) in
	Hashtbl.fold untitled1 knowledge []

let rec synthesize knowledge equations term =
	let sub_synthesis = function
		| (Function (_, es) | Tuple es) as outer ->
			let synthesize_and_add_term e replist =
				(* assert (List.length (synthesize knowledge equations e) > 0); *)
				let add_representation ret rep = List.fold_left (fun a x -> (rep::x)::a) ret replist in
				List.fold_left add_representation [] (synthesize knowledge equations e) in
			let synthesized_subterms_list = List.fold_right synthesize_and_add_term es [[]] in
			List.map (fun x -> change_inner_terms outer x) synthesized_subterms_list
		| Variable _ -> [] in
	let representations t = Hashtbl.find_all knowledge t @ sub_synthesis t in
	List.flatten (List.map representations (equation_instances term equations))

let add_check_or_knowledge knowledge equations exp res rep =
	match synthesize knowledge equations exp with
	| [] -> Hashtbl.add knowledge exp rep; res
	| r::_ ->
		match Check (rep,r) with
		| Check (Variable x, Variable y) when x = y -> res
		| check when List.mem check res -> res
		| check -> check :: res

let analyse_step knowledge equations =
	let derive_from_equation res eq =
		let substitutions = find_substitutions eq knowledge equations in
		let create_representations res sub =
			let Equation (eq_left, eq_right) = eq in
			let exp = apply_substitution eq_right sub in
			let representations = synthesize knowledge equations (apply_substitution eq_left sub) in
			assert (List.length representations > 0); (* We expect to find at least one representation for each substitution *)
			List.fold_left (add_check_or_knowledge knowledge equations exp) res representations in
		List.fold_left create_representations res substitutions in
	List.fold_left derive_from_equation [] equations

let prune knowledge equations =
	let try_prune k v a =
		Hashtbl.remove knowledge k;
		add_check_or_knowledge knowledge equations k a v in
	Hashtbl.fold try_prune knowledge []

let divide_tuples knowledge =
	let divide_tuples key value a =
		let rec divide_tuples k =
			match k with
			| Tuple es ->
				let untitled = function
				| Tuple _ as e -> get_some(divide_tuples e)
				| e ->
					if key=value then (Hashtbl.add knowledge e e; e)
					else
						let subterm_rep = (Parser.representation_name e) in
						(Hashtbl.add knowledge e subterm_rep; subterm_rep) in
				Some (Tuple (List.map untitled es))
			| _ -> None in
		match divide_tuples key with
		| None -> a
		| Some x -> Hashtbl.remove knowledge key; Let (x,value) :: a in
	Hashtbl.fold divide_tuples knowledge []

let rec term_variants equations term =
	(equation_instances term equations) @
	match term with
	| (Function (_, es) | Tuple es) as outer ->
		let synthesized_subterms_list = product_of_list (List.map (term_variants equations) es) in
		List.map (change_inner_terms outer) synthesized_subterms_list
	| Variable _ -> []

module TermSet = Set.Make( 
  struct
    type t = term
    let compare = compare
  end )

let normalize_term term equations =
	let rec normalize_term terms =
		let new_terms =
			List.fold_left (fun a x -> TermSet.add x a) TermSet.empty (List.flatten (List.map (fun x -> term_variants equations x) (TermSet.elements terms))) in
		if terms = new_terms then TermSet.elements new_terms
		else (normalize_term new_terms) in
	normalize_term (TermSet.singleton term)

let add_normalized_terms term rep knowledge equations =
	List.iter (fun t -> (if not (Hashtbl.mem knowledge t) then Hashtbl.add knowledge t rep)) (normalize_term term equations)

let normalize_knowledge knowledge equations =
	Hashtbl.iter (fun k v -> add_normalized_terms k v knowledge equations) knowledge

let analyse knowledge equations =
	let rec analyse knowledge equations checks =
		let tuple_lets = divide_tuples knowledge in
		let prune_checks = prune knowledge equations in
		let old_size = Hashtbl.length knowledge in
		let new_checks = tuple_lets @ checks @ prune_checks @ analyse_step knowledge equations in
		let new_size = Hashtbl.length knowledge in
		if old_size = new_size then new_checks
		else analyse knowledge equations new_checks in
	analyse knowledge equations []

let print_analysis knowledge equations =
	print_endline "Equations:";
	print_equations equations;
	print_endline "Old knowledge:";
	print_knowledge knowledge;
	let checks = analyse knowledge equations in
	print_endline "New knowledge:";
	print_knowledge knowledge;
	print_endline "Checks:";
	print_checks checks;
	print_newline ();
	print_newline ()





let exchange knowledge_a knowledge_b term equations =
	if can_synthesize_term knowledge_a equations term then (
		let rep = (Parser.representation_name term) in
		(* add_normalized_terms term rep knowledge_b equations *)
		Hashtbl.add knowledge_b term rep; rep
	)
	else failwith ("ERROR: Cannot synthesize "^string_of_term term)
	(* Should the function return the knowledge piece? *)