open Openvswitch

let _ =
	(* set_socket_unix "/var/run/openvswitch/db.sock" *)
	set_socket_tcp "10.80.224.133" 6632;
	
	let show_bridge uuid =
		let b = Bridge.get uuid in
		Printf.printf "%s, %s\n" b.Bridge.name b.Bridge.datapath_id;
	
		let show_port uuid =
			let p = Port.get uuid in
			Printf.printf "\t%s, %s\t" p.Port.name p.Port.mac;
			
			let show_interface uuid =
				let i = Interface.get uuid in
				Printf.printf "\t\t%s, %s\t" i.Interface.name i.Interface.mac
			in
			List.iter show_interface p.Port.interfaces
		in
		List.iter show_port b.Bridge.ports
	in
	let bridges = Bridge.get_all () in
	List.iter show_bridge bridges
	
