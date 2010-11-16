let d = false

let debug s =
	if d then
		print_endline s
	else
		()

(* Should go in Jsonrpc module in rpc-light *)

exception Malformed_method_notification of string

(* already in rpc-light *)
let get name dict =
	if List.mem_assoc name dict then
		List.assoc name dict
	else begin
		Printf.eprintf "%s was not found in the dictionnary\n" name;
		let str = List.map (fun (n,_) -> Printf.sprintf "%s=..." n) dict in
		let str = Printf.sprintf "{%s}" (String.concat "," str) in
		raise (Malformed_method_notification str)
	end

let notification_of_string str =
	match Jsonrpc.of_string str with
	| Rpc.Dict d ->
		let name = match get "method" d with Rpc.String s -> s | _ -> raise (Malformed_method_notification str) in
		let params = match get "params" d with Rpc.Enum l -> l | _ -> raise (Malformed_method_notification str) in
		let (_:unit) = match get "id" d with Rpc.Null -> () | _ -> raise (Malformed_method_notification str) in
		Rpc.call name params
	| _ -> raise (Malformed_method_notification str)

(* JSON-RPC Client *)

let input_json_object fin =
	let buf = Buffer.create 1024 in
	let brace_cnt = ref 0 in
	let in_string = ref false in
	let last_char () = Buffer.nth buf (Buffer.length buf - 1) in
	let rec get () =
		let c = input_char fin in
		begin
			match c with
			| '{' when not !in_string -> brace_cnt := !brace_cnt + 1
			| '}' when not !in_string -> brace_cnt := !brace_cnt - 1
			| '"' when !in_string && (last_char () <> '\\') -> in_string := false
			| '"' when not !in_string -> in_string := true
			| _ -> ()
		end;
		Buffer.add_char buf c;
		if !brace_cnt > 0 then
			get ()
	in
	get ();
	Buffer.contents buf

let received = ref None
let c = Condition.create ()
let m = Mutex.create ()

let rec receive fin () =
	let obj = input_json_object fin in
	debug ("Received:\n\t" ^ obj);
	Mutex.lock m;
	received := Some (Jsonrpc.response_of_string obj);
	Condition.signal c;
	Mutex.unlock m;
	receive fin ()

let with_connection socket f =
	let fin, fout = Unix.open_connection socket in
	debug "Connected.";
	let (_ : Thread.t) = Thread.create (receive fin) () in
	let result = f fin fout in
	Unix.shutdown_connection fin;
	debug "Shut down.";
	result

let with_rpc socket f =
	with_connection socket (fun fin fout ->
		let rpc req =
			let req = Jsonrpc.string_of_call req in
			debug ("Request:\n\t" ^ req);
			output_string fout req;
			flush fout;
			Mutex.lock m;
			Condition.wait c m;
			let res = match !received with Some res -> res | None -> failwith "No response!" in
			Mutex.unlock m;
			res
		in
		f rpc
	)

