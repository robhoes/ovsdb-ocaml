open Ovsdb_types

val list_dbs : (Rpc.call -> Rpc.response) -> string list

val get_schema : (Rpc.call -> Rpc.response) -> string -> string

type result =
	| Insert_result of uuid
	| Select_result of row list
	| Update_result of int
	| Mutate_result of int
	| Delete_result of int

val string_of_result: result -> string

val insert: table -> row -> named_uuid option -> Rpc.t * (Rpc.t -> result)
val select: table -> condition list -> column list option -> Rpc.t * (Rpc.t -> result)
val update: table -> condition list -> row -> Rpc.t * (Rpc.t -> result)
val mutate: table -> condition list -> mutation list -> Rpc.t * (Rpc.t -> result)
val delete: table -> condition list -> Rpc.t * (Rpc.t -> result)

val transact :
	(Rpc.call -> Rpc.response) ->
	db_name -> (Rpc.t * (Rpc.t -> result)) list -> result list

