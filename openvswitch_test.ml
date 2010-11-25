open Openvswitch

let _ =
	(* set_socket_unix "/var/run/openvswitch/db.sock" *)
	set_socket_tcp "10.80.224.133" 6632;

	let show_interface uuid =
		let i = Interface.get uuid in
		Printf.printf "\t\t%s, %s\n" i.Interface.name i.Interface.mac
	in
	let show_port uuid =
		let p = Port.get uuid in
		Printf.printf "\t%s, %s\n" p.Port.name p.Port.mac;
		List.iter show_interface p.Port.interfaces
	in
	let show_bridge uuid =
		let b = Bridge.get ~uuid () in
		Printf.printf "%s, %s\n" b.Bridge.name uuid;
		List.iter show_port b.Bridge.ports
	in
	print_endline "Current list of bridges";
	let bridges = Bridge.get_all () in
	List.iter show_bridge bridges;
	print_endline "====";
	
	let bridge = Bridge.create "testtest" in
	print_endline ("Created bridge " ^ bridge);
	
	let port = Port.create ~bridge "newport" in
	print_endline ("Created port: " ^ port);
	
	let interface = Interface.create ~port ~ty:(Interface.Gre("10.80.3.142", [])) "newif" in
	print_endline ("Created interface: " ^ interface);
	
	let interface_rec = Interface.get interface in
	let _ = Interface.update interface {interface_rec with Interface.name = "newerif"} in
	print_endline ("Modified interface: " ^ interface);
	
	let bridges = Bridge.get_all () in
	List.iter show_bridge bridges;
	print_endline "====";
	
	let n = Bridge.destroy bridge in
	print_endline ("Deleted " ^ (string_of_int n) ^ " bridge ");
	
	let bridges = Bridge.get_all () in
	List.iter show_bridge bridges;
	print_endline "====";
	
	print_endline "All ports";
	let ports = Port.get_all () in
	List.iter show_port ports;
	print_endline "====";
	
	print_endline "All interfaces";
	let interfaces = Interface.get_all () in
	List.iter show_interface interfaces;
	print_endline "====";
	()

