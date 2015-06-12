open Tree

let start = Sys.time ()

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("h",[Variable "a"]),
				Variable "rep_ha";
			Function ("g",[Variable "a";Variable "b"]),
				Variable "rep_gab";
			Function ("g",[Variable "a";Variable "c"]),
				Variable "rep_gac";
			Variable "b",
				Variable "rep_b";
			Variable "c",
				Variable "rep_c";
			Function ("enc",[Variable "m";Function ("h",[Variable "k"])]),
				Variable "rep_enc";
			Variable "k",
				Variable "rep_k";
		];
	knowledge_table

let () =
	let eq = (
		Function ("f",[(Function ("g",[Variable "x";Variable "y"]));Function ("h",[Variable "z"])]),
		Variable "x") in
	Knowledge.print_analysis knowledge [eq]

let () =
	let eq = (
		Function ("o",[(Function ("h",[Variable "x"]));Variable "y";(Function ("g",[Variable "x";Variable "y"]))]),
		Variable "x") in
	Knowledge.print_analysis knowledge [eq]

let () =
	let eq = (
		Function ("h",[(Function ("h",[Variable "x"]))]),
		Variable "x") in
	Knowledge.print_analysis knowledge [eq]

let () =
	let eq = (
		Function ("dec",[(Function ("enc",[Variable "x";Variable "y"]));Variable "y"]),
		Variable "x") in
	Knowledge.print_analysis knowledge [eq]

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("h",[Variable "m"]),
				Variable "a";
			Function ("g",[Variable "m";Variable "u"]),
				Variable "b";
			Function ("g",[Variable "m";(Function ("h",[Variable "k"]))]),
				Variable "c";
			Variable "k",
				Variable "d";
		];
	knowledge_table

let () =
	let eq = (
		Function ("f",[(Function ("h",[Variable "x"]));Variable "y";Variable "z";(Function ("g",[Variable "x";Variable "y"]))]),
		Variable "x") in
		Knowledge.print_analysis knowledge [eq]

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Variable "m",
				Variable "a";
			Function ("f",[Variable "m"]),
				Variable "b";
			Function ("f",[Variable "m"]),
				Variable "d";
			Function ("f",[Function ("f",[Variable "m"])]),
				Variable "c";
		];
		knowledge_table

let () =
	let eq = (
		Function ("f",[Function ("f",[Variable "x"])]),
		Variable "x") in
		Knowledge.print_analysis knowledge [eq]

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("f",[Variable "m";Variable "n"]),
				Variable "a";
			Function ("f",[Variable "n";Variable "n"]),
				Variable "b";
			Variable "m",
				Variable "c";
		];
		knowledge_table

let () =
	let eq = (
		Function ("g",[Function ("f",[Variable "x";Variable "y"]); Function ("f",[Variable "x";Variable "z"]); Function ("f",[Variable "y";Variable "z"])]),
		Variable "y") in
		Knowledge.print_analysis knowledge [eq]

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("f",[Variable "m";Function ("h",[Variable "n"])]),
				Variable "a";
			Variable "n",
				Variable "b";
			Variable "m",
				Variable "c";
			Variable "m",
				Variable "d";
		];
		knowledge_table

let () =
	let equations = 
		[
			(
				Function ("f",[Function ("f",[Variable "x";Variable "y"]);Function ("h",[Variable "y"])]),
				Variable "x")
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("f",[Variable "m"]),
				Variable "a";
			Function ("g",[Variable "m"]),
				Variable "b";
		];
		knowledge_table

let () =
	let equations = 
		[
			(
				Function ("f",[Variable "x"]),
				Variable "x");
			(
				Function ("g",[Variable "x"]),
				Variable "x");
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[(* 
			Function ("enc",[Variable "c";Variable "k"]),
				Variable "a";
			Function ("enc",[Variable "m";Variable "c"]),
				Variable "b";
			Variable "k",
				Variable "c"; *)
			Function ("h",[Variable "m"]),
				Variable "a";
			Variable "m",
				Variable "b";
		];
		knowledge_table

let () =
	let equations = 
		[
			(
				Function ("dec",[Function ("enc",[Variable "x"; Variable "y"]); Variable "y"]),
				Variable "x");
			(
				Function ("g",[Variable "x"]),
				Variable "x");
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("enc",[Tuple ([Variable "m";Variable "k";Tuple ([Variable "q";Variable "s";Variable "n"])]);Variable "k";Variable "n"]),
				Variable "a";
		];
		knowledge_table

let () =
	let equations = 
		[
			(
				Function ("dec",[Function ("enc",[Variable "x";Variable "y";Variable "z"]);Variable "y"]),
				Variable "x")
			;
			(
				Function ("enc",[Variable "x";Variable "y";Variable "z"]),
				Variable "y")
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("enc",[Variable "m";Variable "k";Variable "n"]),
				Variable "a";
			Variable "k",
				Variable "b"
		];
		knowledge_table

let () =
	let equations = 
		[
			(
				Function ("dec",[Function ("enc",[Variable "x";Variable "y";Variable "z"]);Variable "y"]),
				Variable "x")
			;
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("g",[Variable "m";Function ("h",[Function ("h",[Variable "k"])])]),
				Variable "a";
			Variable "k",
				Variable "b"
		];
		knowledge_table
let () =
	let equations = 
		[
			(
				Function ("f",[Function ("g",[Variable "x";Variable "y"]);Variable "y"]),
				Variable "x")
			;
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("g",[Variable "m";Variable "k"]),
				Variable "a";
			Function ("h",[Variable "k";Variable "i"]),
				Variable "b";
			Function ("h",[Variable "k";Variable "j"]),
				Variable "c";
			Variable "i",
				Variable "d";
			Variable "j",
				Variable "e";
		];
		knowledge_table
let () =
	let equations = 
		[
			(
				Function ("f",[Function ("g",[Variable "x";Variable "y"]);Function ("h",[Variable "y";Variable "z"]);Variable "z"]),
				Variable "x")
			;
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("h->",[Variable "holygrail"]),
				Variable "a";
		];
		knowledge_table
let () =
	let equations = 
		[
			(
				Function ("f->",[Function ("g->",[Variable "x"])]),
				Variable "x")
			;
			(
				Function ("g->",[Variable "x"]),
				Function ("h->",[Variable "x"]))
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Variable "b",
				Variable "b";
			Function ("dhg",[Variable "a"]),
				Variable "ag";
			Function ("enc",[Variable "m"; Function ("dhe",[Function ("dhg",[Variable "b"]);Variable "a"])]),
				Variable "a";
		];
		knowledge_table
let () =
	let equations = 
		[
			(
				Function ("dec",[Function ("enc",[Variable "x";Variable "y"]);Variable "y"]),
				Variable "x")
			;
			(
				Function ("dhe",[Function ("dhg",[Variable "x"]);Variable "y"]),
				Function ("dhe",[Function ("dhg",[Variable "y"]);Variable "x"]))
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("h",[Function ("f",[Variable "m";Variable "n"])]),
				Variable "a";
			Function ("enc",[Variable "m"; Function ("h",[Function ("f",[Variable "n";Variable "m"])])]),
				Variable "b";
		];
		knowledge_table
let () =
	let equations = 
		[
			(
				Function ("dec",[Function ("enc",[Variable "x";Variable "y"]);Variable "y"]),
				Variable "x")
			;
			(
				Function ("f",[Variable "x";Variable "y"]),
				Function ("f",[Variable "y";Variable "x"]))
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations

let () =
	let asdf = Knowledge.normalize_term (Function ("f",[Function ("h",[Function ("f",[Variable "x";Variable "y"])]);Variable "y"])) [(Function ("f",[Variable "x";Variable "y"]), Variable "x")] in
	List.iter (fun x -> print_endline (string_of_term x)) asdf;print_newline ();
	let asdf = Knowledge.normalize_term (Function ("h",[Function ("h",[Function ("f",[Variable "x";Variable "y"])])])) [(Function ("f",[Variable "x";Variable "y"]), Variable "x")] in
	List.iter (fun x -> print_endline (string_of_term x)) asdf


let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Tuple [Variable "m";Variable "n";Variable "k"],
				Variable "x";
			Tuple [Variable "a";Variable "b";Variable "c"],
				Tuple [Variable "a";Variable "b";Variable "c"];
		];
		knowledge_table
let () = Knowledge.print_analysis knowledge []



let knowledge =
	let knowledge_table = Hashtbl.create 10 in
	List.iter
		(fun (exp,rep) -> Hashtbl.add knowledge_table exp rep)
		[
			Function ("f",[Variable "k"]),
				Variable "a";
		];
		knowledge_table
let () =
	let equations = 
		[
			(
				Function ("f",[Variable "x"]),
				Variable "x")
		] in
		Knowledge.normalize_knowledge knowledge equations;
		Knowledge.print_analysis knowledge equations



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

let () =
	let stop = Sys.time () in
	Printf.printf "Execution time: %.0fms\n" ((stop -. start) *. 1000.0)


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
		Checked_narration.print_translation narration;
		print_newline ()
	done

(* f(g(x),y)=x *)
(* f(g(x),y)=h(x,y) *)
(* Kun udgangspunktets mappede parametre må være i højresiden, når viden udledes? *)
(* f(g(x),y)=(x,y) *)

(* a, x *)
(* h(a), y *)
(* [h(x) = y] *)

(* f(g(x),y)=h(x,y) *)
(* g(a), x *)
(* h(a,b), y *)
(* b, z *)
(* [f(x,z) = y] *)

(* f(g(x),h(x))=x *)

(* Følgende ligning er pæn, men genererer k^n nye dele viden, hvor n er antal h(_). *)
(* f(h(x),h(y)) = f(x,y)  (terminerer ikke i proverif) *)
(* f(h(x),h(y)) = g(x,y) *)

(* HVordan håndteres følgende: (naivt uendelig) *)
(* f(h(x),h(y)) = h((x,y)) *)
(* Hvis man modtager noget der ligner den ene side, og man kan bygge det der er på den anden side, så lav et tjek *)


(* 1. Udled ny viden *)
(* 2. Generer tjek, hvis et udtryk kan synteseres af et andet udtryk *)
(* 3. Et udtryk passer i højresiden? *)

(*

f(g(x,y),y)=x
h(x)=x

Analysis (returning duplicates)
{
	(g(m,h(k)),a)
	(g(m,k),b)
	(g(m,f(m,k)),c)
	(h(k),d)
}
=1>
{
	(g(m,h(k)),a)
	(g(m,k),b)
	(g(m,f(m,k)),c)
	(h(k),d)
	(m,f(a,d))
	(k,d)
}
{}
=2>
{
	(g(m,h(k)),a)
	(g(m,k),b)
	(g(m,f(m,k)),c)
	(h(k),d)
	(m,f(a,d))
	(k,d)
}
{
	(m,f(b,d))
	(m,f(c,f(f(a,d),d)))
}
=> generate checks
{
	(g(m,h(k)),a)
	(g(m,k),b)
	(g(m,f(m,k)),c)
	(h(k),d)
	(m,f(a,d))
	(k,d)
}
{
	[f(a,d)=f(b,d)]
	[f(a,d)=f(c,f(f(a,d),d))]
}
=> remove
{
	(m,f(a,d))
	(k,d)
}
{
	[f(a,d)=f(b,d)]
	[f(a,d)=f(c,f(f(a,d),d))]
	[d=d]
	[a=g(f(a,d),d)]
	[b=g(f(a,d),d)]
	[c=g(f(a,d),f(f(a,d),d))]
}




Analysis (returning checks (remove first))
{
	(g(m,h(k)),a)
	(g(m,k),b)
	(g(m,f(m,k)),c)
	(h(k),d)
	(h(k),e)
}
{}
=remove>
{
	(g(m,h(k)),a)
	(g(m,k),b)
	(g(m,f(m,k)),c)
	(h(k),d)
}
{
	[e=d]
}
=derive>
{
	(g(m,h(k)),a)
	(g(m,k),b)
	(g(m,f(m,k)),c)
	(h(k),d)
	(m,f(a,d))
	(k,d)
}
{
	[e=d]
}
=remove>
{
	(m,f(a,d))
	(k,d)
}
{
	[e=d]
	[a=g(f(a,d),d)]
	[b=g(f(a,d),d)]
	[c=g(f(a,d),f(f(a,d),d))]
	[d=d]
}
*)


(* 
f(x)=x
g(x)=x


{
	(f(m),a)
	(g(m),b)
}
{
	(m,a)
	(m,b)
}
 *)