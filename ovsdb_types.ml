type db_name = string
and table = string
and column = string
and row = (column * value) list
and value = Atom of atom | Set of set | Map of map
and atom = String of string | Number of float | Integer of int64 | Boolean of bool | Uuid of uuid | Named_uuid of named_uuid
and set = atom list
and map = pair list
and pair = atom * atom
and uuid = string
and named_uuid = string
and condition = column * funct * value
and funct = Lt | Leq | Eq | Neq | Geq | Gt | Includes | Excludes
and mutation = column * mutator * value
and mutator = Add | Sub | Mul | Div | Mod | Insert | Delete

let rec mutator_of_rpc __x36__ =
	match __x36__ with
	| Rpc.String "delete" -> Delete
	| Rpc.String "insert" -> Insert
	| Rpc.String "%=" -> Mod
	| Rpc.String "/=" -> Div
	| Rpc.String "*=" -> Mul
	| Rpc.String "-=" -> Sub
	| Rpc.String "+=" -> Add
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "mutator" "__x36__" (Rpc.to_string __x__) "Enum[String s;...]"
			 else ();
			 raise (Rpc.Runtime_error (("Enum[String s;...]", __x__))))
and mutation_of_rpc __x37__ =
	match __x37__ with
	| Rpc.Enum ([ __x38__; __x39__; __x40__ ]) ->
			((column_of_rpc __x38__), (mutator_of_rpc __x39__),
			 (value_of_rpc __x40__))
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "mutation" "__x37__" (Rpc.to_string __x__) "List"
			 else ();
			 raise (Rpc.Runtime_error (("List", __x__))))
and funct_of_rpc __x41__ =
	match __x41__ with
	| Rpc.String "excludes" -> Excludes
	| Rpc.String "includes" -> Includes
	| Rpc.String ">" -> Gt
	| Rpc.String ">=" -> Geq
	| Rpc.String "!=" -> Neq
	| Rpc.String "==" -> Eq
	| Rpc.String "<=" -> Leq
	| Rpc.String "<" -> Lt
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "funct" "__x41__" (Rpc.to_string __x__) "Enum[String s;...]"
			 else ();
			 raise (Rpc.Runtime_error (("Enum[String s;...]", __x__))))
and condition_of_rpc __x42__ =
	match __x42__ with
	| Rpc.Enum ([ __x43__; __x44__; __x45__ ]) ->
			((column_of_rpc __x43__), (funct_of_rpc __x44__),
			 (value_of_rpc __x45__))
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "condition" "__x42__" (Rpc.to_string __x__) "List"
			 else ();
			 raise (Rpc.Runtime_error (("List", __x__))))
and named_uuid_of_rpc __x46__ =
	match __x46__ with
	| Rpc.Enum [Rpc.String "named-uuid"; Rpc.String x] -> x
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "named_uuid" "__x46__" (Rpc.to_string __x__) "String(string)"
			 else ();
			 raise (Rpc.Runtime_error (("String(string)", __x__))))
and uuid_of_rpc __x47__ =
	match __x47__ with
	| Rpc.Enum [Rpc.String "uuid"; Rpc.String x] -> x
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "uuid" "__x47__" (Rpc.to_string __x__) "String(string)"
			 else ();
			 raise (Rpc.Runtime_error (("String(string)", __x__))))
and pair_of_rpc __x48__ =
	match __x48__ with
	| Rpc.Enum ([ __x49__; __x50__ ]) ->
			((atom_of_rpc __x49__), (atom_of_rpc __x50__))
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "pair" "__x48__" (Rpc.to_string __x__) "List"
			 else ();
			 raise (Rpc.Runtime_error (("List", __x__))))
and map_of_rpc __x51__ =
	match __x51__ with
	| Rpc.Enum ([Rpc.String "map"; Rpc.Enum x]) -> List.map (fun x' -> pair_of_rpc x') x
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "map" "__x51__" (Rpc.to_string __x__) "List"
			 else ();
			 raise (Rpc.Runtime_error (("List", __x__))))
and set_of_rpc __x54__ =
	match __x54__ with
	| Rpc.Enum ([Rpc.String "set"; Rpc.Enum x]) -> List.map (fun x' -> atom_of_rpc x') x
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "set" "__x54__" (Rpc.to_string __x__) "List"
			 else ();
			 raise (Rpc.Runtime_error (("List", __x__))))
and atom_of_rpc __x57__ =
	match __x57__ with
	| Rpc.Enum ([ Rpc.String "named-uuid"; _ ]) as x -> Named_uuid (named_uuid_of_rpc x)
	| Rpc.Enum ([ Rpc.String "uuid"; _ ]) as x -> Uuid (uuid_of_rpc x)
	| Rpc.Bool x -> Boolean x
	| Rpc.Float x -> Number x
	| Rpc.Int x -> Integer x
	| Rpc.String x -> String x
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "atom" "__x57__" (Rpc.to_string __x__) "Enum[String s;...]"
			 else ();
			 raise (Rpc.Runtime_error (("Enum[String s;...]", __x__))))
and value_of_rpc __x63__ =
	match __x63__ with
	| Rpc.Enum (Rpc.String "map" :: _) as __x64__ -> Map (map_of_rpc __x64__)
	| Rpc.Enum (Rpc.String "set" :: _) as __x65__ -> Set (set_of_rpc __x65__)
	| x -> Atom (atom_of_rpc x)
and row_of_rpc __x67__ =
	let is_a_real_dict =
		try let (_ : column) = column_of_rpc (Rpc.String "") in true
		with | _ -> false
	in
		if is_a_real_dict
		then
			(match __x67__ with
			 | Rpc.Dict d ->
					 List.map
						 (fun (key, __x68__) ->
								((column_of_rpc (Rpc.String key)), (value_of_rpc __x68__)))
						 d
			 | __x__ ->
					 (if Rpc.get_debug ()
						then
							Printf.eprintf
								"Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
								"row" "__x67__" (Rpc.to_string __x__) "Dict"
						else ();
						raise (Rpc.Runtime_error (("Dict", __x__)))))
		else
			(match __x67__ with
			 | Rpc.Enum e ->
					 List.map
						 (fun __x68__ ->
								match __x68__ with
								| Rpc.Enum ([ __x69__; __x70__ ]) ->
										((column_of_rpc __x69__), (value_of_rpc __x70__))
								| __x__ ->
										(if Rpc.get_debug ()
										 then
											 Printf.eprintf
												 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
												 "row" "__x68__" (Rpc.to_string __x__) "List"
										 else ();
										 raise (Rpc.Runtime_error (("List", __x__)))))
						 e
			 | __x__ ->
					 (if Rpc.get_debug ()
						then
							Printf.eprintf
								"Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
								"row" "__x67__" (Rpc.to_string __x__) "Enum"
						else ();
						raise (Rpc.Runtime_error (("Enum", __x__)))))
and column_of_rpc __x71__ =
	match __x71__ with
	| Rpc.String x -> x
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "column" "__x71__" (Rpc.to_string __x__) "String(string)"
			 else ();
			 raise (Rpc.Runtime_error (("String(string)", __x__))))
and table_of_rpc __x72__ =
	match __x72__ with
	| Rpc.String x -> x
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "table" "__x72__" (Rpc.to_string __x__) "String(string)"
			 else ();
			 raise (Rpc.Runtime_error (("String(string)", __x__))))
and db_name_of_rpc __x73__ =
	match __x73__ with
	| Rpc.String x -> x
	| __x__ ->
			(if Rpc.get_debug ()
			 then
				 Printf.eprintf
					 "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\n"
					 "db_name" "__x73__" (Rpc.to_string __x__) "String(string)"
			 else ();
			 raise (Rpc.Runtime_error (("String(string)", __x__))))

let rec rpc_of_mutator __x1__ =
	match __x1__ with
	| Delete -> Rpc.String "delete"
	| Insert -> Rpc.String "insert"
	| Mod -> Rpc.String "%="
	| Div -> Rpc.String "/="
	| Mul -> Rpc.String "*="
	| Sub -> Rpc.String "-="
	| Add -> Rpc.String "+="
and rpc_of_mutation __x2__ =
	let (__x3__, __x4__, __x5__) = __x2__
	in
		Rpc.Enum
			[ rpc_of_column __x3__; rpc_of_mutator __x4__; rpc_of_value __x5__ ]
and rpc_of_funct __x6__ =
	match __x6__ with
	| Excludes -> Rpc.String "excludes"
	| Includes -> Rpc.String "includes"
	| Gt -> Rpc.String ">"
	| Geq -> Rpc.String ">="
	| Neq -> Rpc.String "!="
	| Eq -> Rpc.String "=="
	| Leq -> Rpc.String "<="
	| Lt -> Rpc.String "<"
and rpc_of_condition __x7__ =
	let (__x8__, __x9__, __x10__) = __x7__
	in
		Rpc.Enum
			[ rpc_of_column __x8__; rpc_of_funct __x9__; rpc_of_value __x10__ ]
and rpc_of_named_uuid __x11__ = Rpc.Enum [Rpc.String "named-uuid"; Rpc.String __x11__]
and rpc_of_uuid __x12__ = Rpc.Enum [Rpc.String "uuid"; Rpc.String __x12__]
and rpc_of_pair __x13__ =
	let (__x14__, __x15__) = __x13__
	in Rpc.Enum [ rpc_of_atom __x14__; rpc_of_atom __x15__ ]
and rpc_of_map __x16__ =
	Rpc.Enum ([Rpc.String "map"; Rpc.Enum (List.map (fun __x17__ -> rpc_of_pair __x17__) __x16__)])
and rpc_of_set __x18__ =
	if List.length __x18__ <> 1 then
		Rpc.Enum (Rpc.String "set" :: List.map (fun __x19__ -> rpc_of_atom __x19__) __x18__)
	else
		rpc_of_atom (List.hd __x18__)
and rpc_of_atom __x20__ =
	match __x20__ with
	| Named_uuid __x21__ -> rpc_of_named_uuid __x21__
	| Uuid __x22__ -> rpc_of_uuid __x22__
	| Boolean __x23__ -> Rpc.Bool __x23__
	| Number __x24__ -> Rpc.Float __x24__
	| Integer x -> Rpc.Int x
	| String __x25__ -> Rpc.String __x25__
and rpc_of_value __x26__ =
	match __x26__ with
	| Map __x27__ -> rpc_of_map __x27__
	| Set __x28__ -> rpc_of_set __x28__
	| Atom __x29__ -> rpc_of_atom __x29__
and rpc_of_row __x30__ =
	let is_a_real_dict =
		try let (_ : column) = column_of_rpc (Rpc.String "") in true
		with | _ -> false in
	let dict =
		List.map
			(fun (__x31__, __x32__) ->
				 ((rpc_of_column __x31__), (rpc_of_value __x32__)))
			__x30__
	in
		if is_a_real_dict
		then
			Rpc.Dict
				(List.map
					 (function | (Rpc.String k, v) -> (k, v) | _ -> assert false) dict)
		else Rpc.Enum (List.map (fun (k, v) -> Rpc.Enum [ k; v ]) dict)
and rpc_of_column __x33__ = Rpc.String __x33__
and rpc_of_table __x34__ = Rpc.String __x34__
and rpc_of_db_name __x35__ = Rpc.String __x35__

let rec string_of_db_name x = x
and string_of_table x = x
and string_of_column x = x
and string_of_row x =
	let r = String.concat ";\n\t"
		(List.map (fun (c, v) -> Printf.sprintf "(%s, %s)" (string_of_column c) (string_of_value v)) x) in
	Printf.sprintf "[%s]" r
and string_of_value = function
	| Atom x -> string_of_atom x
	| Set x -> string_of_set x
	| Map x -> string_of_map x
and string_of_atom = function
	| String x -> x
	| Number x -> string_of_float x
	| Integer x -> Int64.to_string x
	| Boolean x -> string_of_bool x
	| Uuid x -> string_of_uuid x
	| Named_uuid x -> string_of_named_uuid x
and string_of_set x =
	let r = String.concat ";\n\t" (List.map (fun a -> string_of_atom a) x) in
	Printf.sprintf "{%s}" r
and string_of_map x =
	let r = String.concat ";\n\t" (List.map (fun p -> string_of_pair p) x) in
	Printf.sprintf "[%s]" r
and string_of_pair (a, b) =
	Printf.sprintf "(%s, %s)" (string_of_atom a) (string_of_atom b)
and string_of_uuid x = x
and string_of_named_uuid x = x

