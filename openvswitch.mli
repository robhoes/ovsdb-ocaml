val db_name : string
val socket : Unix.sockaddr option ref
val set_socket_unix : string -> unit
val set_socket_tcp : string -> int -> unit
val do_calls : (Rpc.t * (Rpc.t -> Ovsdb.result)) list -> Ovsdb.result list
val do_call : Rpc.t * (Rpc.t -> Ovsdb.result) -> Ovsdb.result
module Bridge :
  sig
    type t = {
      uuid : string;
      name : string;
      datapath_id : string;
      ports : string list;
    }
    val get_all : unit -> Ovsdb_types.uuid list
    val get : Ovsdb_types.uuid -> t
    val create : name:string -> Ovsdb_types.uuid
    val destroy : Ovsdb_types.uuid -> int
  end
module Port :
  sig
    type t = {
      uuid : string;
      mac : string;
      name : string;
      interfaces : string list;
    }
    val get : Ovsdb_types.uuid -> t
  end
module Interface :
  sig
    type iftype =
        System
      | Internal
      | Tap
      | Gre of string * (string * string) list
      | Capwap of string * (string * string) list
      | Patch of string
    type t = { uuid : string; mac : string; name : string; ty : iftype; }
    val get : Ovsdb_types.uuid -> t
    val create : name:string -> ?ty:iftype -> Ovsdb_types.uuid
  end
