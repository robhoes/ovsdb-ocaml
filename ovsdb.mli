open Ovsdb_types

val list_dbs : (Rpc.call -> Rpc.response) -> string list

val get_schema : (Rpc.call -> Rpc.response) -> string -> string

type result

val string_of_result: result -> string

val insert: table -> row -> named_uuid option -> Rpc.t * (Rpc.t -> result)
val select: table -> condition list -> column list option -> Rpc.t * (Rpc.t -> result)
val update: table -> condition list -> row -> Rpc.t * (Rpc.t -> result)

val transact :
	(Rpc.call -> Rpc.response) ->
	db_name -> (Rpc.t * (Rpc.t -> result)) list -> result list

