open Ovsdb
open Ovsdb_types

(* Test *)

let _ =
	(* let socket = Unix.ADDR_UNIX "/var/run/openvswitch/db.sock" *)
	let socket = Unix.ADDR_INET ((Unix.inet_addr_of_string "10.80.224.133"), 6632) in
	Jsonrpc_client.with_rpc socket (fun rpc ->
		let dbs = list_dbs rpc in
		print_endline (List.hd dbs);
		let schema = get_schema rpc "Open_vSwitch" in
		print_endline schema;
		let result = transact rpc "Open_vSwitch" [
			select "Bridge" [] (Some ["name"; "datapath_id"]);
			select "Port" ["name", Eq, Atom (String "eth1")] (Some ["name"; "tag"]);
			(* insert "Bridge" ["name", Atom (String "testbr")] None; *)
			update "Bridge"
				["name", Eq, Atom (String "testbr")]
				["other_config", Map [String "hwaddr", String "xx:xx"]];
			select "Open_vSwitch" [] None;
		] in
		List.iter (fun r -> print_endline (string_of_result r)) result;
		()
	)

