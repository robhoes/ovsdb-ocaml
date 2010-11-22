val set_socket_unix : string -> unit
val set_socket_tcp : string -> int -> unit

module Interface :
	sig
		(** The type of an interface. *)
		type iftype =
			| System
				(** An ordinary network device, e.g. [eth0] on Linux.
				Sometimes referred to as ``external interfaces'' since they are
				generally connected to hardware external to that on which the Open
				vSwitch is running.  The empty string is a synonym for
				[system]. *)
			| Internal
				(** A simulated network device that sends and receives traffic.  An
				internal interface whose [name] is the same as its bridge's [name] is called the
				``local interface.''  It does not make sense to bond an internal
				interface, so the terms ``port'' and ``interface'' are often used
				imprecisely for internal interfaces. *)
			| Tap
				(** A TUN/TAP device managed by Open vSwitch. *)
			| Gre of string * (string * string) list
				(** An Ethernet over RFC 2890 Generic Routing Encapsulation over IPv4
				tunnel.  Each tunnel must be uniquely identified by the
				combination of [remote_ip], [local_ip], and
				[in_key].  Note that if two ports are defined that are
				the same except one has an optional identifier and the other does
				not, the more specific one is matched first.  [in_key]
				is considered more specific than [local_ip] if a port
				defines one and another port defines the other.
				The first parameter of the constructor is the remote IP. The second is a
				list of options as key/value pairs. Available options are [local_ip], [in_key],
				[out_key], [key], [tos], [ttl], [csum], [pmtud], and [header_cache]. *)
			| Capwap of string * (string * string) list
				(** Ethernet tunneling over the UDP transport portion of CAPWAP
				(RFC 5415).  This allows interoperability with certain switches
				where GRE is not available.  Note that only the tunneling component
				of the protocol is implemented.  Due to the non-standard use of
				CAPWAP, UDP ports 58881 and 58882 are used as the source and
				destinations ports respectivedly.  Each tunnel must be uniquely
				identified by the combination of [remote_ip] and
				[local_ip].  If two ports are defined that are the same
				except one includes [local_ip] and the other does not,
				the more specific one is matched first.  CAPWAP support is not
				available on all platforms.  Currently it is only supported in the
				Linux kernel module with kernel versions >= 2.6.25.
				The first parameter of the constructor is the remote IP. The second is a
				list of options as key/value pairs. Available options are [local_ip], [tos],
				[ttl], [pmtud], and [header_cache]. *)
			| Patch of string
				(** A pair of virtual devices that act as a patch cable.
				There is one required parameter called [peer].
				[peer] is the [name] of the Interface for the other side of the patch.
				The named Interface's own [peer] option must specify
				this Interface's name.  That is, the two patch
				interfaces must have reversed [name] and [peer] values. *)

		type t = { uuid : string; mac : string; name : string; ty : iftype; }
		val get_all : unit -> string list
		val get : string -> t
		val create : ?port:string -> ?ty:iftype -> string -> string
		val destroy : string -> int
	end
module Port :
	sig
		type t = {
			uuid : string;
			mac : string;
			name : string;
			interfaces : string list;
		}
		val get_all : unit -> string list
		val get : string -> t
		val create : ?bridge:string ->
			?interfaces:string list -> string -> string
		val destroy : string -> int
		val add_interface : string -> string -> unit
	end
module Bridge :
	sig
		type t = {
			uuid : string;
			name : string;
			datapath_id : string;
			ports : string list;
		}
		val get_all : unit -> string list
		val get : string -> t
		val create : string -> string
		val destroy : string -> int
		val add_port : string -> string -> unit
	end
