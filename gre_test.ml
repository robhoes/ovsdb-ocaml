open Openvswitch

let _ =
	let ip0 = "10.80.239.239" in
	let ip1 = "10.80.237.116" in
	
	set_socket_tcp ip0 6632;
	let i = Interface.create ~ty:(Interface.Gre (ip1, ["key", "666"; "local_ip", ip0])) "gre666" in
	let p = Port.create ~interfaces:[i] "gre666" in
	let b = Bridge.get ~name:"xapi15" () in
	Bridge.add_port b.Bridge.uuid p;

	set_socket_tcp ip1 6632;
	let i = Interface.create ~ty:(Interface.Gre (ip0, ["key", "666"; "local_ip", ip1])) "gre666" in
	let p = Port.create ~interfaces:[i] "gre666" in
	let b = Bridge.get ~name:"xapi15" () in
	Bridge.add_port b.Bridge.uuid p

