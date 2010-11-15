open Ovsdb_types

let d = true

let debug s =
	if d then
		print_endline s
	else
		()

type string_list = string list with rpc
type object_list = Rpc.t list with rpc

let process_response res f =
	if res.Rpc.success then
		f res.Rpc.contents
	else
		failwith ("Error:" ^ (Rpc.string_of_rpc res.Rpc.contents))

(* list_dbs call *)

let list_dbs rpc =
	let request = Rpc.call "list_dbs" [] in
	let response = rpc request in
	process_response response string_list_of_rpc

(* get_schema call *)

let get_schema rpc db_name =
	let req = Rpc.call "get_schema" [rpc_of_db_name db_name] in
	let res = rpc req in
	process_response res Rpc.to_string

(* insert operation *)

type insert_result = {uuid: string list} with rpc

let insert_handler res =
	List.hd (List.tl (insert_result_of_rpc res).uuid)
	
let insert table row uuid_name =
	let params =
		match uuid_name with
		| None ->
			Rpc.Dict [
				"op", Rpc.String "insert";
				"table", rpc_of_table table;
				"row", rpc_of_row row;
			]
		| Some n ->
			Rpc.Dict [
				"op", Rpc.String "insert";
				"table", rpc_of_table table;
				"row", rpc_of_row row;
				"uuid-name", rpc_of_named_uuid n;
			]
	in
	params, insert_handler

(* select operation *)

type select_result = {rows: (string * Rpc.t) list list} with rpc

let select_handler res =
	let process_row row =
		String.concat ", " (List.map (fun (n, v) -> n ^ " = " ^ (Rpc.to_string v)) row)
	in
	String.concat "\n" (List.map process_row (select_result_of_rpc res).rows)
	
let select table where columns =
	let params =
		match columns with
		| None ->
			Rpc.Dict [
				"op", Rpc.String "select";
				"table", rpc_of_table table;
				"where", Rpc.Enum (List.map (fun w -> rpc_of_condition w) where);
			]
		| Some cols ->
			Rpc.Dict [
				"op", Rpc.String "select";
				"table", rpc_of_table table;
				"where", Rpc.Enum (List.map (fun w -> rpc_of_condition w) where);
				"columns", Rpc.Enum (List.map (fun col -> rpc_of_column col) cols);
			]
	in
	params, select_handler

(* update operation *)

type update_result = {count: int} with rpc

let update_handler res =
	string_of_int (update_result_of_rpc res).count
	
let update table where row =
	let params =
		Rpc.Dict [
			"op", Rpc.String "update";
			"table", rpc_of_table table;
			"where", Rpc.Enum (List.map (fun w -> rpc_of_condition w) where);
			"row", rpc_of_row row;
		]
	in
	params, update_handler

(* transact call *)

let transact rpc db_name operations_with_handlers =
	let operations, handlers = List.split operations_with_handlers in
	let params = Rpc.String db_name :: operations in
	let req = Rpc.call "transact" params in
	let res = rpc req in
	let results = process_response res object_list_of_rpc in
	let rec process ac rs hs = match (rs, hs) with
		| [], [] -> ac
		| r :: rs, h :: hs -> process (h r :: ac) rs hs
		| _ -> failwith "Numbers of operations and results do not match"
	in
	List.rev (process [] results handlers)

