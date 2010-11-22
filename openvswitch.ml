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

module Interface = struct
	type iftype =
		| System
		| Internal
		| Tap
		| Gre of string * (string * string) list
		| Capwap of string * (string * string) list
		| Patch of string

	type t = {
		uuid: string;
		mac: string;
		name: string;
		ty: iftype;
	}

	let get_all () =
		let result = do_call (select "Interface" [] (Some ["_uuid"])) in
		match result with
		| Select_result rows ->
			List.map (fun row -> string_of_value (List.assoc "_uuid" row)) rows
		| _ -> failwith "Unexpected response"

	let get uuid =
		let result = do_call (select "Interface" ["_uuid", Eq, Atom (Uuid uuid)] None) in
		let make row = {
			uuid = uuid;
			mac = string_of_value (List.assoc "mac" row);
			name = string_of_value (List.assoc "name" row);
			ty = match List.assoc "type" row with
				| Atom (String "") | Atom (String "system") -> System
				| Atom (String "internal") -> Internal
				| Atom (String "tap") -> Tap
				| Atom (String "gre") -> Gre ("", [])
				| Atom (String "capwap") -> Capwap ("", [])
				| Atom (String "patch") -> Patch ""
				| _ -> failwith "Illegal type"
		} in
		match result with
		| Select_result [row] -> make row
		| _ -> failwith "Unexpected response"

	let create ?port ?ty name =
		let row = [
				"name", Atom (String name);
			] @
			match ty with
			| None -> []
			| Some System -> ["type", Atom (String "system")]
			| Some Internal -> ["type", Atom (String "internal")]
			| Some Tap -> ["type", Atom (String "tap")]
			| Some (Gre (remote_ip, options)) ->
				["type", Atom (String "gre"); "options", Map [String "remote_ip", String remote_ip]]
			| Some (Capwap (remote_ip, options)) ->
				["type", Atom (String "capwap"); "options", Map [String "remote_ip", String remote_ip]]
			| Some (Patch peer) ->
				["type", Atom (String "patch"); "options", Map [String "peer", String peer]]
		in
		let result = do_call (insert "Interface" row None) in
		let uuid = match result with
			| Insert_result u -> u
			| _ -> failwith "Unexpected response"
		in
		begin
			match port with
			| Some p ->
				let (_ : result) = do_call (mutate "Port" ["_uuid", Eq, Atom (Uuid p)] ["interfaces", Insert, Atom (Uuid uuid)]) in ()
			| None -> ()
		end;
		string_of_uuid uuid

	let destroy uuid =
		let results = do_calls [
			mutate "Port" [] ["interfaces", Delete, Atom (Uuid uuid)];
			delete "Interface" ["_uuid", Eq, Atom (Uuid uuid)]
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

	let get_all () =
		let result = do_call (select "Port" [] (Some ["_uuid"])) in
		match result with
		| Select_result rows ->
			List.map (fun row -> string_of_value (List.assoc "_uuid" row)) rows
		| _ -> failwith "Unexpected response"

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

	let create ?bridge ?interfaces name =
		let ifs = match interfaces with
			| None -> [Interface.create name]
			| Some ifs -> ifs
		in
		let row = [
				"name", Atom (String name);
				"interfaces", Set (List.map (fun i -> Uuid i) ifs);
			]
		in
		let result = do_call (insert "Port" row None) in
		let uuid = match result with
			| Insert_result u -> u
			| _ -> failwith "Unexpected response"
		in
		begin
			match bridge with
			| Some b ->
				let (_ : result) = do_call (mutate "Bridge" ["_uuid", Eq, Atom (Uuid b)] ["ports", Insert, Atom (Uuid uuid)]) in ()
			| None -> ()
		end;
		string_of_uuid uuid

	let destroy uuid =
		let port = get uuid in
		let (_ : int list) = List.map Interface.destroy port.interfaces in
		let results = do_calls [
			mutate "Bridge" [] ["ports", Delete, Atom (Uuid uuid)];
			delete "Port" ["_uuid", Eq, Atom (Uuid uuid)];
		] in
		match results with
		| [_; Delete_result count] -> count
		| _ -> failwith "Unexpected response"
end

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

	let create name =
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
		let bridge = get uuid in
		let (_ : int list) = List.map Port.destroy bridge.ports in
		let results = do_calls [
			mutate db_name [] ["bridges", Delete, Atom (Uuid uuid)];
			delete "Bridge" ["_uuid", Eq, Atom (Uuid uuid)]
		] in
		match results with
		| [_; Delete_result count] -> count
		| _ -> failwith "Unexpected response"
end
