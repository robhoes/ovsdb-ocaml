val set_socket_unix : string -> unit
val set_socket_tcp : string -> int -> unit

type bridge = { name : string; datapath_id : string; ports : string list; }

val get_bridges : unit -> Ovsdb_types.uuid list
val get_bridge : Ovsdb_types.uuid -> bridge
