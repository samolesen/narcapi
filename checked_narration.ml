open Helper
open Tree

let translate narration =
	let before = 
		Hashtbl.iter ( fun _ knowledge -> ignore (Knowledge.analyse knowledge narration.equations)) narration.agents;
			"(* Equational theory *)\n" ^
			newline_separate (List.map string_of_equation narration.equations) ^ "\n\n" ^
			"(* Principals *)\n" ^
			string_of_agents narration.agents ^ "\n\n" ^
			"(* Names *)\n" ^
			string_of_names narration.names ^ "\n\n" ^
			"(* Exchanges *)\n" in
	let after =
		let asdf e =
			let Exchange (a,b,t) = e in
			let rep = Knowledge.exchange (Hashtbl.find narration.agents a) (Hashtbl.find narration.agents b) t narration.equations in
			let checks = Knowledge.string_of_checks (Knowledge.analyse (Hashtbl.find narration.agents b) narration.equations) in
			string_of_exchange e ^ " * " ^ string_of_term rep ^ "\n" ^ checks ^ "\n" ^ string_of_knowledge (Hashtbl.find narration.agents b) ^ "\n" in
		newline_separate (List.map asdf narration.exchanges) in
	before ^ after

let print_translation narration = print_string (translate narration)

(* 
dec(enc(x,y),y) = x

f(f(g(),x),y) = f(f(g(),y),x)

A,B knows (A,B,m)
A knows a
B knows b

A->B: f(g(),a)
B->A: f(g(),b)
A->B: enc(m, f(f(g(),b),a) ).
 *)

(* 
f(x,y) = g(x)

A knows f(a,b).
 *)

(* 
dec(enc(x,y),y) = x

f(g(x),y) = f(g(y),x)

A,B knows (A,B,m)
A knows a
B knows b

A->B: g(a)
B->A: g(b)
A->B: enc(m, f(g(b),a) ).
 *)

(* 
f(x,y) = g(x)

A knows f(a,b).
 *)

(*  Genererer ikke tjek (konvergerer ikke i ét skridt)
f(x,y) = g(x,y)
f(x,y) = f(y,x)

A knows (a,b)

A->B: g(a,b)
A->B: g(b,a).
 *)