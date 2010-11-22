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
	(** The type of an interface. *)
	type iftype =
		| System
			(** An ordinary network device, e.g. [eth0] on Linux.
			Sometimes referred to as ``external interfaces'' since they are
			generally connected to hardware external to that on which the Open
			vSwitch is running.  The empty string is a synonym for
			[system]. *)
		| Internal
			(** A simulated network device that sends and receives traffic.  An
			internal interface whose [name] is the same as its bridge's [name] is called the
			``local interface.''  It does not make sense to bond an internal
			interface, so the terms ``port'' and ``interface'' are often used
			imprecisely for internal interfaces. *)
		| Tap
			(** A TUN/TAP device managed by Open vSwitch. *)
		| Gre of string * (string * string) list
			(** An Ethernet over RFC 2890 Generic Routing Encapsulation over IPv4
			tunnel.  Each tunnel must be uniquely identified by the
			combination of [remote_ip], [local_ip], and
			[in_key].  Note that if two ports are defined that are
			the same except one has an optional identifier and the other does
			not, the more specific one is matched first.  [in_key]
			is considered more specific than [local_ip] if a port
			defines one and another port defines the other.
			The first parameter of the constructor is the remote IP. The second is a
			list of options as key/value pairs. Available options are [local_ip], [in_key],
			[out_key], [key], [tos], [ttl], [csum], [pmtud], and [header_cache]. *)
		| Capwap of string * (string * string) list
			(** Ethernet tunneling over the UDP transport portion of CAPWAP
			(RFC 5415).  This allows interoperability with certain switches
			where GRE is not available.  Note that only the tunneling component
			of the protocol is implemented.  Due to the non-standard use of
			CAPWAP, UDP ports 58881 and 58882 are used as the source and
			destinations ports respectivedly.  Each tunnel must be uniquely
			identified by the combination of [remote_ip] and
			[local_ip].  If two ports are defined that are the same
			except one includes [local_ip] and the other does not,
			the more specific one is matched first.  CAPWAP support is not
			available on all platforms.  Currently it is only supported in the
			Linux kernel module with kernel versions >= 2.6.25.
			The first parameter of the constructor is the remote IP. The second is a
			list of options as key/value pairs. Available options are [local_ip], [tos],
			[ttl], [pmtud], and [header_cache]. *)
		| Patch of string
			(** A pair of virtual devices that act as a patch cable.
			There is one required parameter called [peer].
			[peer] is the [name] of the Interface for the other side of the patch.
			The named Interface's own [peer] option must specify
			this Interface's name.  That is, the two patch
			interfaces must have reversed [name] and [peer] values. *)

	type t = {
		uuid: string;
		mac: string;
		name: string;
		ty: iftype;
	}

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

	let create ?ty ~name =
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

