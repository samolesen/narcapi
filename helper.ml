let comma_separate strlist =
	String.concat ", " strlist

let newline_separate strlist =
	String.concat "\n" strlist

let parenthesize str =
	"(" ^ str ^ ")"

let create_tuple_string strlist =
	parenthesize (comma_separate strlist)

let list_of_table f t =
	Hashtbl.fold (fun k v a -> (f k v) :: a) t []

let if_none opt x =
	match opt with
	| None -> x
	| some -> some

let get_some = function
	| Some x -> x
	| None -> failwith "Expression should be of the form: Some x."

let product_of_list listlist =
	let add_list_to_product elist result =
		assert (List.length elist > 0); (* The sublists should not be empty *)
		let add_element_to_product ret e = List.fold_left (fun a x -> (e::x)::a) ret result in
		List.fold_left add_element_to_product [] elist in
	List.fold_right add_list_to_product listlist [[]]

let raised_warnings = Queue.create ()

let raise_warning message =
	Queue.add ("Warning: " ^ message) raised_warnings

let raise_error message =
	print_endline ("Error: " ^ message);
	exit 2