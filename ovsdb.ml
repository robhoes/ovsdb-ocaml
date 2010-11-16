open Ovsdb_types

let d = false

let debug s =
	if d then
		print_endline s
	else
		()

let process_response res f =
	if res.Rpc.success then
		f res.Rpc.contents
	else
		failwith ("Error:" ^ (Rpc.string_of_rpc res.Rpc.contents))

(* list_dbs call *)

type list_dbs_result = db_name list with rpc

let list_dbs rpc =
	let request = Rpc.call "list_dbs" [] in
	let response = rpc request in
	process_response response list_dbs_result_of_rpc

(* get_schema call *)

let get_schema rpc db_name =
	let req = Rpc.call "get_schema" [rpc_of_db_name db_name] in
	let res = rpc req in
	process_response res Rpc.to_string

type insert_result = {uuid: uuid} with rpc
type select_result = {rows: row list} with rpc
type update_result = {count: int} with rpc
type mutate_result = update_result with rpc
type delete_result = update_result with rpc

type result =
	| Insert_result of uuid
	| Select_result of row list
	| Update_result of int
	| Mutate_result of int
	| Delete_result of int

let string_of_result = function
	| Insert_result x -> string_of_uuid x
	| Select_result x -> String.concat "\n" (List.map string_of_row x)
	| Update_result x -> string_of_int x
	| Mutate_result x -> string_of_int x
	| Delete_result x -> string_of_int x

(* insert operation *)

let insert_handler res =
	Insert_result (insert_result_of_rpc res).uuid
	
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

let select_handler res =
	Select_result (select_result_of_rpc res).rows

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

let update_handler res =
	Update_result (update_result_of_rpc res).count
	
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

(* mutate operation *)

let mutate_handler res =
	Mutate_result (mutate_result_of_rpc res).count
	
let mutate table where mutations =
	let params =
		Rpc.Dict [
			"op", Rpc.String "mutate";
			"table", rpc_of_table table;
			"where", Rpc.Enum (List.map (fun w -> rpc_of_condition w) where);
			"mutations", Rpc.Enum (List.map (fun w -> rpc_of_mutation w) mutations);
		]
	in
	params, mutate_handler

(* delete operation *)

let delete_handler res =
	Mutate_result (delete_result_of_rpc res).count
	
let delete table where =
	let params =
		Rpc.Dict [
			"op", Rpc.String "delete";
			"table", rpc_of_table table;
			"where", Rpc.Enum (List.map (fun w -> rpc_of_condition w) where);
		]
	in
	params, delete_handler

(* transact call *)

type object_list = Rpc.t list with rpc

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

