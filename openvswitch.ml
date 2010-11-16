open Ovsdb
open Ovsdb_types

let db_name = "Open_vSwitch"
let socket = ref None

let set_socket_unix path =
	socket := Some (Unix.ADDR_UNIX path)

let set_socket_tcp ip port =
	socket := Some (Unix.ADDR_INET ((Unix.inet_addr_of_string ip), port))

type bridge = {
	name: string;
	datapath_id: string;
	ports: string list;
}

let do_call c =
	match !socket with
	| Some s -> Jsonrpc_client.with_rpc s (fun rpc -> List.hd (transact rpc db_name [c]))
	| None -> failwith "No socket configured"

let get_bridges () =
	let result = do_call (select db_name [] (Some ["bridges"])) in
	match result with
	| Select_result [["bridges", Set l]] ->
		List.map (function Uuid p -> string_of_uuid p | _ -> "") l
	| _ -> failwith "Unexpected response"

let get_bridge uuid =
	let result = do_call (select "Bridge" ["_uuid", Eq, Atom (Uuid uuid)] None) in
	let make_bridge row = {
		name = string_of_value (List.assoc "name" row);
		datapath_id = string_of_value (List.assoc "datapath_id" row);
		ports = match (List.assoc "ports" row) with
			| Set l -> List.map (function Uuid p -> string_of_uuid p | _ -> "") l
			| _ -> [];
	} in
	match result with
	| Select_result [row] -> make_bridge row
	| _ -> failwith "Unexpected response"

