type token =
  | IDENT of (string)
  | INTEGER of (int)
  | EQUAL
  | LPAREN
  | RPAREN
  | COMMA
  | SLASH
  | SEPARATOR
  | NEWLINE
  | ARROW
  | COLON
  | KNOW
  | PRIVATE
  | GENERATES
  | SHARE
  | DOT
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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
# 81 "parser.ml"
let yytransl_const = [|
  259 (* EQUAL *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* COMMA *);
  263 (* SLASH *);
  264 (* SEPARATOR *);
  265 (* NEWLINE *);
  266 (* ARROW *);
  267 (* COLON *);
  268 (* KNOW *);
  269 (* PRIVATE *);
  270 (* GENERATES *);
  271 (* SHARE *);
  272 (* DOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* INTEGER *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\004\000\004\000\003\000\003\000\005\000\
\005\000\006\000\006\000\006\000\006\000\007\000\007\000\008\000\
\008\000\009\000\009\000\010\000\010\000\011\000\011\000\011\000\
\011\000\012\000\012\000\013\000\014\000\014\000\015\000\015\000\
\015\000\016\000\016\000\016\000\016\000\017\000\001\000\000\000"

let yylen = "\002\000\
\004\000\003\000\001\000\003\000\001\000\001\000\000\000\001\000\
\000\000\002\000\002\000\001\000\001\000\001\000\000\000\005\000\
\003\000\003\000\001\000\003\000\001\000\003\000\002\000\003\000\
\003\000\003\000\001\000\005\000\003\000\001\000\004\000\002\000\
\002\000\003\000\002\000\001\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\040\000\014\000\000\000\000\000\
\011\000\010\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\027\000\000\000\030\000\000\000\036\000\038\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\023\000\008\000\000\000\000\000\035\000\000\000\000\000\
\000\000\032\000\000\000\033\000\000\000\000\000\020\000\017\000\
\000\000\022\000\000\000\002\000\000\000\018\000\034\000\024\000\
\025\000\000\000\026\000\000\000\000\000\029\000\001\000\000\000\
\004\000\000\000\031\000\028\000\016\000"

let yydgoto = "\002\000\
\005\000\031\000\032\000\033\000\036\000\043\000\007\000\015\000\
\016\000\017\000\018\000\019\000\020\000\021\000\022\000\023\000\
\008\000"

let yysindex = "\001\000\
\034\255\000\000\034\255\034\255\000\000\000\000\015\255\251\254\
\000\000\000\000\040\255\030\255\017\255\026\255\000\000\034\255\
\043\255\000\000\034\255\000\000\034\255\000\000\000\000\000\000\
\030\255\048\255\051\255\050\255\062\255\063\255\060\255\064\255\
\000\000\000\000\000\000\065\255\015\255\000\000\030\255\069\255\
\002\255\000\000\070\255\000\000\067\255\068\255\000\000\000\000\
\066\255\000\000\030\255\000\000\026\255\000\000\000\000\000\000\
\000\000\046\255\000\000\034\255\071\255\000\000\000\000\030\255\
\000\000\030\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\008\255\000\000\009\255\013\255\000\000\000\000\057\255\000\000\
\000\000\000\000\024\255\073\255\000\000\072\255\000\000\074\255\
\000\000\000\000\074\255\000\000\074\255\000\000\000\000\000\000\
\073\255\000\000\000\000\000\000\000\000\032\255\075\255\000\000\
\000\000\000\000\000\000\000\000\076\255\000\000\000\000\000\000\
\076\255\000\000\076\255\000\000\000\000\049\255\000\000\000\000\
\000\000\000\000\000\000\000\000\061\255\000\000\000\000\000\000\
\000\000\049\255\000\000\074\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\249\255\051\000\028\000\029\000\004\000\241\255\046\000\
\000\000\058\000\044\000\000\000\043\000\047\000\050\000\000\000\
\000\000"

let yytablesize = 92
let yytable = "\014\000\
\038\000\001\000\058\000\042\000\006\000\044\000\009\000\010\000\
\015\000\013\000\024\000\015\000\013\000\012\000\013\000\011\000\
\012\000\034\000\012\000\037\000\015\000\013\000\041\000\015\000\
\013\000\012\000\003\000\013\000\012\000\014\000\030\000\056\000\
\003\000\012\000\035\000\021\000\003\000\003\000\021\000\003\000\
\003\000\003\000\004\000\025\000\067\000\026\000\027\000\003\000\
\046\000\028\000\049\000\026\000\048\000\029\000\039\000\028\000\
\068\000\040\000\069\000\029\000\021\000\009\000\050\000\021\000\
\009\000\051\000\025\000\053\000\052\000\057\000\061\000\063\000\
\037\000\026\000\009\000\045\000\064\000\007\000\065\000\005\000\
\028\000\066\000\054\000\047\000\059\000\062\000\055\000\060\000\
\000\000\015\000\000\000\014\000"

let yycheck = "\007\000\
\016\000\001\000\001\001\019\000\001\000\021\000\003\000\004\000\
\001\001\001\001\016\001\004\001\004\001\001\001\013\001\001\001\
\004\001\001\001\004\001\016\000\013\001\013\001\019\000\016\001\
\016\001\013\001\003\001\013\001\016\001\037\000\001\001\039\000\
\009\001\004\001\009\001\012\001\005\001\006\001\015\001\008\001\
\009\001\008\001\009\001\004\001\060\000\006\001\007\001\016\001\
\001\001\010\001\001\001\006\001\002\001\014\001\012\001\010\001\
\064\000\015\001\066\000\014\001\012\001\001\001\001\001\015\001\
\004\001\006\001\004\001\003\001\005\001\001\001\001\001\005\001\
\016\001\006\001\003\001\025\000\011\001\005\001\051\000\005\001\
\010\001\053\000\037\000\026\000\041\000\043\000\037\000\041\000\
\255\255\016\001\255\255\016\001"

let yynames_const = "\
  EQUAL\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  SLASH\000\
  SEPARATOR\000\
  NEWLINE\000\
  ARROW\000\
  COLON\000\
  KNOW\000\
  PRIVATE\000\
  GENERATES\000\
  SHARE\000\
  DOT\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  INTEGER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'optional_termlist) in
    Obj.repr(
# 83 "parser.mly"
                                          ( Function (_1, _3) )
# 222 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_termlist) in
    Obj.repr(
# 84 "parser.mly"
                                          ( Tuple _2 )
# 229 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                                          ( Variable _1 )
# 236 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 88 "parser.mly"
                        ( _1 :: _3 )
# 244 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 89 "parser.mly"
                        ( [_1] )
# 251 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 93 "parser.mly"
             ( _1 )
# 258 "parser.ml"
               : 'optional_termlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
             ( [] )
# 264 "parser.ml"
               : 'optional_termlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
            ()
# 270 "parser.ml"
               : 'optional_newline))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
            ()
# 276 "parser.ml"
               : 'optional_newline))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sep) in
    Obj.repr(
# 101 "parser.mly"
                  ()
# 283 "parser.ml"
               : 'sep))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sep) in
    Obj.repr(
# 102 "parser.mly"
                  ()
# 290 "parser.ml"
               : 'sep))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                  ()
# 296 "parser.ml"
               : 'sep))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
                  ()
# 302 "parser.ml"
               : 'sep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sep) in
    Obj.repr(
# 107 "parser.mly"
        ()
# 309 "parser.ml"
               : 'optional_sep))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
        ()
# 315 "parser.ml"
               : 'optional_sep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'optional_newline) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'optional_newline) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 112 "parser.mly"
  (
			let function_definitions term =
				let add_definition func arity acc =
					let result = if Hashtbl.mem signature_table func then acc else FunctionSymbol (func, arity) :: acc in
					add_function ~warning:true func arity;
					result in
				term_function_fold add_definition [] term in
			Equation (_1, _5) ::	(function_definitions _5) @ (function_definitions _1)
		)
# 333 "parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 121 "parser.mly"
                        ( add_function _1 _3; [FunctionSymbol (_1, _3)] )
# 341 "parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 125 "parser.mly"
                              ( _3 @ _1 )
# 350 "parser.ml"
               : 'equationlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 126 "parser.mly"
                              ( _1 )
# 357 "parser.ml"
               : 'equationlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'agentlist) in
    Obj.repr(
# 129 "parser.mly"
                          ( _1 :: _3 )
# 365 "parser.ml"
               : 'agentlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parser.mly"
                          ( [_1] )
# 372 "parser.ml"
               : 'agentlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
                          ( add_agent _1; add_generated _3 _1; Hashtbl.add (Hashtbl.find agent_table _1) (Variable _3) (Variable _3) )
# 380 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
                          ( add_private _2 )
# 387 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agentlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 135 "parser.mly"
                          ( term_function_iter check_function _3; term_variable_iter add_public _3; List.iter (fun a -> add_agent a; Hashtbl.add (Hashtbl.find agent_table a) _3 _3) _1 )
# 395 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agentlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
                          ( List.iter add_agent _1; add_private _3; List.iter (fun a -> add_agent a; Hashtbl.add (Hashtbl.find agent_table a) (Variable _3) (Variable _3)) _1 )
# 403 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'declarationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 139 "parser.mly"
                                    ( _3 :: _1 )
# 412 "parser.ml"
               : 'declarationlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 140 "parser.mly"
                                    ( [_1] )
# 419 "parser.ml"
               : 'declarationlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 144 "parser.mly"
  (
			add_agent _1; add_agent _3;
			term_variable_iter
				(fun x ->
					if not (Hashtbl.mem name_table x) then (* Replace with check in $1's knowledge *)
						print_endline ("ERROR: \"" ^ x ^ "\" is not a name or an agent."))
				_5;
			term_function_iter check_function _5;
			Exchange (_1, _3, _5) )
# 436 "parser.ml"
               : 'exchange))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exchangelist) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exchange) in
    Obj.repr(
# 155 "parser.mly"
                              ( _3 :: _1 )
# 445 "parser.ml"
               : 'exchangelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exchange) in
    Obj.repr(
# 156 "parser.mly"
                              ( [_1] )
# 452 "parser.ml"
               : 'exchangelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'declarationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exchangelist) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'optional_sep) in
    Obj.repr(
# 159 "parser.mly"
                                                  ( _3 )
# 462 "parser.ml"
               : 'narration2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declarationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_sep) in
    Obj.repr(
# 160 "parser.mly"
                                                  ( [] )
# 470 "parser.ml"
               : 'narration2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exchangelist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_sep) in
    Obj.repr(
# 161 "parser.mly"
                                                  ( _1 )
# 478 "parser.ml"
               : 'narration2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'narration2) in
    Obj.repr(
# 164 "parser.mly"
                                ( (_1,_3) )
# 487 "parser.ml"
               : 'narration1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'equationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_sep) in
    Obj.repr(
# 165 "parser.mly"
                                ( (_1,[]) )
# 495 "parser.ml"
               : 'narration1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'narration2) in
    Obj.repr(
# 166 "parser.mly"
                                ( ([],_1) )
# 502 "parser.ml"
               : 'narration1))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "parser.mly"
                                ( ([],[]) )
# 508 "parser.ml"
               : 'narration1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'optional_sep) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'narration1) in
    Obj.repr(
# 171 "parser.mly"
  (
			let (eq,ex) = _2 in
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
		)
# 535 "parser.ml"
               : 'narration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'narration) in
    Obj.repr(
# 193 "parser.mly"
               ( _1 )
# 542 "parser.ml"
               : Tree.narration))
(* Entry narration_dot *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let narration_dot (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree.narration)
