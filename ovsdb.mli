open Ovsdb_types

val list_dbs : (Rpc.call -> Rpc.response) -> string list

val get_schema : (Rpc.call -> Rpc.response) -> string -> string

val insert: table -> row -> named_uuid option -> Rpc.t * (Rpc.t -> string)
val select: table -> condition list -> column list option -> Rpc.t * (Rpc.t -> string)
val update: table -> condition list -> row -> Rpc.t * (Rpc.t -> string)

val transact :
	(Rpc.call -> Rpc.response) ->
	string -> (Rpc.t * (Rpc.t -> 'a)) list -> 'a list

