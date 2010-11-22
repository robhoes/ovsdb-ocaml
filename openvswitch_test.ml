open Openvswitch

let _ =
	(* set_socket_unix "/var/run/openvswitch/db.sock" *)
	set_socket_tcp "10.80.224.133" 6632;
	
	let show_bridge uuid =
		let b = Bridge.get uuid in
		Printf.printf "%s, %s\n" b.Bridge.name uuid;
	
		let show_port uuid =
			let p = Port.get uuid in
			Printf.printf "\t%s, %s\n" p.Port.name p.Port.mac;
			
			let show_interface uuid =
				let i = Interface.get uuid in
				Printf.printf "\t\t%s, %s\n" i.Interface.name i.Interface.mac
			in
			List.iter show_interface p.Port.interfaces
		in
		List.iter show_port b.Bridge.ports
	in
	print_endline "Current list of bridges";
	let bridges = Bridge.get_all () in
	List.iter show_bridge bridges;
	print_endline "====";
	
	let bridge = Bridge.create ~name:"testtest" in
	print_endline ("Created bridge " ^ bridge);
	
	let bridges = Bridge.get_all () in
	List.iter show_bridge bridges;
	print_endline "====";
	
	let n = Bridge.destroy bridge in
	print_endline ("Deleted " ^ (string_of_int n) ^ " bridge ");
	
	let bridges = Bridge.get_all () in
	List.iter show_bridge bridges;
	print_endline "====";
	
	let interface = Interface.create ~name:"newif" ~ty:(Interface.Gre("10.80.3.142", [])) in
	print_endline ("Created interface: " ^ interface);
	let n = Interface.destroy interface in
	print_endline ("Deleted " ^ (string_of_int n) ^ " interface ");
	()

