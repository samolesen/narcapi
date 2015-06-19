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
open Helper
open Tree

let signature_table = Hashtbl.create 10
let name_table = Hashtbl.create 10
let agent_table = Hashtbl.create 10

let add_function ?(warning=false) func arity =
	match Hashtbl.mem signature_table func with
	| false ->
		if warning then raise_warning ("Missing declaration of function \""^func^"\" has been added.");
		Hashtbl.add signature_table func arity
	| true -> if Hashtbl.find signature_table func <> arity then
		raise_error ("Function \""^func^"\" is used with conflicting numbers of arguments.")


let check_function func arity =
	try 
		if Hashtbl.find signature_table func <> arity then
			raise_error ("Function \""^func^"\" is used with conflicting numbers of arguments.")
	with Not_found -> raise_error ("Function \""^func^"\" is not declared.")

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
	| Name -> if Hashtbl.find name_table e = Private || Hashtbl.find name_table e = Public then raise_error (e ^ " cannot be generated due to previous declaration.")
	| Agent | Both -> raise_error (e ^ " cannot be generated due to being an agent.")

let add_agent a =
	match exist_in_tables a with
	| None | Name ->
		begin try
			if Hashtbl.find name_table a <> Private && Hashtbl.find name_table a <> Public then raise_error (a ^ " cannot be an agent due to being generated.")
		with Not_found -> () end;
		Hashtbl.add agent_table a (Hashtbl.create 10); add_public a
	| Agent | Both -> ()
# 84 "parser.ml"
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
\003\000\003\000\003\000\005\000\005\000\004\000\004\000\006\000\
\006\000\007\000\007\000\007\000\007\000\008\000\008\000\009\000\
\009\000\010\000\010\000\011\000\011\000\012\000\012\000\012\000\
\012\000\013\000\013\000\014\000\015\000\015\000\016\000\016\000\
\016\000\017\000\017\000\017\000\017\000\002\000\001\000\000\000\
\000\000"

let yylen = "\002\000\
\004\000\003\000\001\000\003\000\001\000\001\000\000\000\001\000\
\000\000\002\000\002\000\001\000\001\000\001\000\000\000\005\000\
\003\000\003\000\001\000\003\000\001\000\003\000\002\000\003\000\
\003\000\003\000\001\000\005\000\003\000\001\000\004\000\002\000\
\002\000\003\000\002\000\001\000\000\000\002\000\002\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\000\000\014\000\
\000\000\041\000\011\000\010\000\039\000\000\000\000\000\000\000\
\000\000\019\000\000\000\000\000\027\000\000\000\030\000\000\000\
\036\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\023\000\008\000\000\000\000\000\035\000\
\000\000\000\000\000\000\032\000\000\000\033\000\000\000\000\000\
\020\000\017\000\000\000\022\000\000\000\002\000\000\000\018\000\
\034\000\024\000\025\000\000\000\026\000\000\000\000\000\029\000\
\001\000\000\000\004\000\000\000\031\000\028\000\016\000"

let yydgoto = "\003\000\
\006\000\007\000\033\000\034\000\035\000\038\000\008\000\009\000\
\018\000\019\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000"

let yysindex = "\049\000\
\044\255\044\255\000\000\044\255\044\255\000\000\244\254\000\000\
\016\255\000\000\000\000\000\000\000\000\029\255\037\255\039\255\
\046\255\000\000\044\255\030\255\000\000\044\255\000\000\044\255\
\000\000\000\000\037\255\047\255\052\255\057\255\059\255\058\255\
\055\255\060\255\000\000\000\000\000\000\061\255\016\255\000\000\
\037\255\062\255\024\255\000\000\066\255\000\000\063\255\064\255\
\000\000\000\000\065\255\000\000\037\255\000\000\046\255\000\000\
\000\000\000\000\000\000\000\255\000\000\044\255\067\255\000\000\
\000\000\037\255\000\000\037\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\018\255\012\000\000\000\007\000\011\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\012\255\068\255\000\000\
\069\255\000\000\003\000\000\000\000\000\003\000\000\000\003\000\
\000\000\000\000\068\255\000\000\000\000\000\000\000\000\001\000\
\070\255\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
\000\000\000\000\005\000\000\000\005\000\000\000\000\000\032\255\
\000\000\000\000\000\000\000\000\000\000\000\000\045\255\000\000\
\000\000\000\000\000\000\032\255\000\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\067\000\247\255\044\000\021\000\023\000\004\000\250\255\
\040\000\000\000\052\000\038\000\000\000\037\000\041\000\046\000\
\000\000"

let yytablesize = 283
let yytable = "\017\000\
\003\000\037\000\015\000\013\000\014\000\028\000\013\000\011\000\
\012\000\030\000\012\000\015\000\040\000\031\000\003\000\044\000\
\014\000\046\000\015\000\015\000\003\000\015\000\039\000\021\000\
\060\000\043\000\021\000\045\000\016\000\017\000\015\000\058\000\
\027\000\015\000\028\000\029\000\016\000\032\000\030\000\036\000\
\015\000\041\000\031\000\021\000\042\000\009\000\021\000\048\000\
\009\000\001\000\002\000\004\000\005\000\050\000\037\000\069\000\
\070\000\051\000\071\000\052\000\053\000\027\000\059\000\055\000\
\054\000\045\000\063\000\065\000\010\000\028\000\047\000\009\000\
\007\000\067\000\005\000\066\000\030\000\068\000\056\000\049\000\
\061\000\064\000\000\000\062\000\057\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\003\000\003\000\013\000\
\003\000\003\000\013\000\012\000\015\000\000\000\012\000\015\000\
\003\000\037\000\015\000\013\000\014\000\000\000\013\000\012\000\
\015\000\000\000\012\000"

let yycheck = "\009\000\
\000\000\000\000\000\000\016\001\000\000\006\001\000\000\004\000\
\005\000\010\001\000\000\000\000\019\000\014\001\003\001\022\000\
\001\001\024\000\001\001\004\001\009\001\004\001\019\000\012\001\
\001\001\022\000\015\001\024\000\013\001\039\000\013\001\041\000\
\004\001\016\001\006\001\007\001\013\001\001\001\010\001\001\001\
\004\001\012\001\014\001\012\001\015\001\001\001\015\001\001\001\
\004\001\001\000\002\000\008\001\009\001\002\001\009\001\062\000\
\066\000\001\001\068\000\001\001\006\001\004\001\001\001\003\001\
\005\001\062\000\001\001\005\001\002\000\006\001\027\000\003\001\
\005\001\053\000\005\001\011\001\010\001\055\000\039\000\028\000\
\043\000\045\000\255\255\043\000\039\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\006\001\001\001\
\008\001\009\001\004\001\001\001\001\001\255\255\004\001\004\001\
\016\001\016\001\016\001\013\001\016\001\255\255\016\001\013\001\
\013\001\255\255\016\001"

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
# 88 "parser.mly"
                                          ( Function (_1, _3) )
# 275 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_termlist) in
    Obj.repr(
# 89 "parser.mly"
                                          ( Tuple _2 )
# 282 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
                                          ( Variable _1 )
# 289 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 93 "parser.mly"
                        ( _1 :: _3 )
# 297 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 94 "parser.mly"
                        ( [_1] )
# 304 "parser.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'termlist) in
    Obj.repr(
# 98 "parser.mly"
             ( _1 )
# 311 "parser.ml"
               : 'optional_termlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
             ( [] )
# 317 "parser.ml"
               : 'optional_termlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
            ()
# 323 "parser.ml"
               : 'optional_newline))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
            ()
# 329 "parser.ml"
               : 'optional_newline))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sep) in
    Obj.repr(
# 106 "parser.mly"
                  ()
# 336 "parser.ml"
               : 'sep))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sep) in
    Obj.repr(
# 107 "parser.mly"
                  ()
# 343 "parser.ml"
               : 'sep))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                  ()
# 349 "parser.ml"
               : 'sep))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                  ()
# 355 "parser.ml"
               : 'sep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sep) in
    Obj.repr(
# 112 "parser.mly"
        ()
# 362 "parser.ml"
               : 'optional_sep))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
        ()
# 368 "parser.ml"
               : 'optional_sep))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'optional_newline) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'optional_newline) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 117 "parser.mly"
  (
			let function_definitions term =
				let add_definition func arity acc =
					let result = if Hashtbl.mem signature_table func then acc else FunctionSymbol (func, arity) :: acc in
					add_function ~warning:true func arity;
					result in
				term_function_fold add_definition [] term in
			Equation (_1, _5) ::	(function_definitions _5) @ (function_definitions _1)
		)
# 386 "parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 126 "parser.mly"
                        ( add_function _1 _3; [FunctionSymbol (_1, _3)] )
# 394 "parser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 130 "parser.mly"
                              ( _3 @ _1 )
# 403 "parser.ml"
               : 'equationlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 131 "parser.mly"
                              ( _1 )
# 410 "parser.ml"
               : 'equationlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'agentlist) in
    Obj.repr(
# 134 "parser.mly"
                          ( _1 :: _3 )
# 418 "parser.ml"
               : 'agentlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "parser.mly"
                          ( [_1] )
# 425 "parser.ml"
               : 'agentlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 138 "parser.mly"
                          ( add_agent _1; add_generated _3 _1; Hashtbl.add (Hashtbl.find agent_table _1) (Variable _3) (Variable _3) )
# 433 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 139 "parser.mly"
                          ( add_private _2 )
# 440 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agentlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 140 "parser.mly"
                          ( term_function_iter check_function _3; term_variable_iter add_public _3; List.iter (fun a -> add_agent a; Hashtbl.add (Hashtbl.find agent_table a) _3 _3) _1 )
# 448 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'agentlist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 141 "parser.mly"
                          ( List.iter add_agent _1; add_private _3; List.iter (fun a -> add_agent a; Hashtbl.add (Hashtbl.find agent_table a) (Variable _3) (Variable _3)) _1 )
# 456 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'declarationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 144 "parser.mly"
                                    ( _3 :: _1 )
# 465 "parser.ml"
               : 'declarationlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 145 "parser.mly"
                                    ( [_1] )
# 472 "parser.ml"
               : 'declarationlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 149 "parser.mly"
  (
			add_agent _1; add_agent _3;
			term_variable_iter
				(fun x ->
					if not (Hashtbl.mem name_table x) then (* Replace with check in $1's knowledge *)
						raise_error ("\"" ^ x ^ "\" is not a name or an agent.")
				)
				_5;
			term_function_iter check_function _5;
			Exchange (_1, _3, _5) )
# 490 "parser.ml"
               : 'exchange))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exchangelist) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exchange) in
    Obj.repr(
# 161 "parser.mly"
                              ( _3 :: _1 )
# 499 "parser.ml"
               : 'exchangelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exchange) in
    Obj.repr(
# 162 "parser.mly"
                              ( [_1] )
# 506 "parser.ml"
               : 'exchangelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'declarationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exchangelist) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'optional_sep) in
    Obj.repr(
# 165 "parser.mly"
                                                  ( _3 )
# 516 "parser.ml"
               : 'narration2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declarationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_sep) in
    Obj.repr(
# 166 "parser.mly"
                                                  ( [] )
# 524 "parser.ml"
               : 'narration2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exchangelist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_sep) in
    Obj.repr(
# 167 "parser.mly"
                                                  ( _1 )
# 532 "parser.ml"
               : 'narration2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sep) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'narration2) in
    Obj.repr(
# 170 "parser.mly"
                                ( (_1,_3) )
# 541 "parser.ml"
               : 'narration1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'equationlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_sep) in
    Obj.repr(
# 171 "parser.mly"
                                ( (_1,[]) )
# 549 "parser.ml"
               : 'narration1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'narration2) in
    Obj.repr(
# 172 "parser.mly"
                                ( ([],_1) )
# 556 "parser.ml"
               : 'narration1))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "parser.mly"
                                ( ([],[]) )
# 562 "parser.ml"
               : 'narration1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'optional_sep) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'narration1) in
    Obj.repr(
# 177 "parser.mly"
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
# 589 "parser.ml"
               : Tree.narration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Tree.narration) in
    Obj.repr(
# 199 "parser.mly"
               ( _1 )
# 596 "parser.ml"
               : Tree.narration))
(* Entry narration_dot *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry narration *)
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
let narration (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Tree.narration)
