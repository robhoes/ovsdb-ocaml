open Openvswitch

let _ =
	(* set_socket_unix "/var/run/openvswitch/db.sock" *)
	set_socket_tcp "10.80.224.133" 6632;
	
	let bridges = get_bridges () in
	Printf.printf "bridges: %s" (String.concat " " bridges);
	
	let b = get_bridge (List.hd bridges) in
	Printf.printf "%s, %s, %s\n" b.name b.datapath_id (String.concat " " b.ports)
	
