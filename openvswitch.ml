open Ovsdb
open Ovsdb_types

let db_name = "Open_vSwitch"
let socket = ref None

let set_socket_unix path =
	socket := Some (Unix.ADDR_UNIX path)

let set_socket_tcp ip port =
	socket := Some (Unix.ADDR_INET ((Unix.inet_addr_of_string ip), port))

let do_calls l =
	match !socket with
	| Some s -> Jsonrpc_client.with_rpc s (fun rpc -> transact rpc db_name l)
	| None -> failwith "No socket configured"

let do_call c =
	List.hd (do_calls [c])

module Bridge = struct
	type t = {
		uuid: string;
		name: string;
		datapath_id: string;
		ports: string list;
	}

	let get_all () =
		let result = do_call (select db_name [] (Some ["bridges"])) in
		match result with
		| Select_result [["bridges", Set l]] ->
			List.map (function Uuid p -> string_of_uuid p | _ -> "") l
		| _ -> failwith "Unexpected response"

	let get uuid =
		let result = do_call (select "Bridge" ["_uuid", Eq, Atom (Uuid uuid)] None) in
		let make row = {
			uuid = uuid;
			name = string_of_value (List.assoc "name" row);
			datapath_id = string_of_value (List.assoc "datapath_id" row);
			ports = match (List.assoc "ports" row) with
				| Set l -> List.map (function Uuid p -> string_of_uuid p | _ -> "") l
				| Atom (Uuid u) -> [string_of_uuid u]
				| _ -> [];
		} in
		match result with
		| Select_result [row] -> make row
		| _ -> failwith "Unexpected response"

	let create ~name =
		let row = [
			"name", Atom (String name);
		] in
		let result = do_call (insert "Bridge" row None) in
		let uuid = match result with
			| Insert_result u -> u
			| _ -> failwith "Unexpected response"
		in
		let (_ : result) = do_call (mutate db_name [] ["bridges", Insert, Atom (Uuid uuid)]) in
		string_of_uuid uuid

	let destroy uuid =
		let results = do_calls [
			mutate db_name [] ["bridges", Delete, Atom (Uuid uuid)];
			delete "Bridge" ["_uuid", Eq, Atom (Uuid uuid)]
		] in
		match results with
		| [_; Delete_result count] -> count
		| _ -> failwith "Unexpected response"
end

module Port = struct
	type t = {
		uuid: string;
		mac: string;
		name: string;
		interfaces: string list;
	}

	let get uuid =
		let result = do_call (select "Port" ["_uuid", Eq, Atom (Uuid uuid)] None) in
		let make row = {
			uuid = uuid;
			mac = string_of_value (List.assoc "mac" row);
			name = string_of_value (List.assoc "name" row);
			interfaces = match (List.assoc "interfaces" row) with
				| Set l -> List.map (function Uuid p -> string_of_uuid p | _ -> "") l
				| Atom (Uuid u) -> [string_of_uuid u]
				| _ -> [];
		} in
		match result with
		| Select_result [row] -> make row
		| _ -> failwith "Unexpected response"
end

module Interface = struct
	type t = {
		uuid: string;
		mac: string;
		name: string;
	}

	let get uuid =
		let result = do_call (select "Interface" ["_uuid", Eq, Atom (Uuid uuid)] None) in
		let make row = {
			uuid = uuid;
			mac = string_of_value (List.assoc "mac" row);
			name = string_of_value (List.assoc "name" row);
		} in
		match result with
		| Select_result [row] -> make row
		| _ -> failwith "Unexpected response"
end

