let timer ?(iter=1) f =
	let start = Sys.time () in
	let func =
		for i=1 to iter do
			ignore (f ())
		done in
	let stop = Sys.time () in
	Printf.printf "Execution time: %.0fms\n" ((stop -. start) *. 1000.0);
	func