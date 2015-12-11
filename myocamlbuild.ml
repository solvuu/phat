(** Build system. *)

(** Information about a library or app provided by this project. *)
module Info : sig

  (** Library or app name. *)
  type name =  [`Lib of string | `App of string]

  type item = {
    name : name;

    (** Internal libraries this library or app directly depends on. *)
    libs : string list;

    (** Additional ocamlfind packages this library or app depends
	on. By "additional", we mean it is not necessary to list packages
	that are already listed for one of this item's [libs]. *)
    pkgs : string list;
  }

  type t = private item list

  val of_list : item list -> t

  val libs : t -> t
  val apps : t -> t
  val names : t -> string list

  val name_as_string : name -> string

  (** Returns item in [t] with given [name]. *)
  val get : t -> name -> item

  (** Returns direct dependencies of item with given [name]. *)
  val libs_direct : t -> name -> string list

  (** Returns all dependencies of item with given [name]. *)
  val libs_all : t -> name -> string list

  (** Returns all packages item with given [name] directly depends
      on. *)
  val pkgs_direct : t -> name -> string list

  (** Returns all packages item with given [name] depends on. *)
  val pkgs_all : t -> name -> string list

end = struct
  type name = [`Lib of string | `App of string]
  type item = {name:name; libs:string list; pkgs:string list}
  type t = item list

  let is_lib item = match item.name with `Lib _ -> true | `App _ -> false
  let is_app item = match item.name with `Lib _ -> false | `App _ -> true

  let names t = List.map (fun x -> match x.name with `Lib s | `App s -> s) t
  let name_as_string = function `Lib x | `App x -> x

  let is_uniq (l : string list) : bool =
    let m = List.length l in
    let n = List.length (List.sort_uniq compare l) in
    m = n

  let libs t = List.filter is_lib t
  let apps t = List.filter is_app t

  let of_list items =
    let libs = names (libs items) in
    let apps = names (apps items) in
    if not (is_uniq libs) then
      failwith "lib names must be unique"
    else if not (is_uniq apps) then
      failwith "app names must be unique"
    else
      items

  let get t name = List.find (fun x -> x.name = name) t

  let libs_direct t name = (get t name).libs

  let libs_all t name =
    let rec loop libs_seen name : string list =
      match name with
      | `Lib lib -> (
	if List.mem lib libs_seen then
	  failwith "cycle detected in Info.data"
	else
	  let libs_seen = lib::libs_seen in
	  let libs_direct = libs_direct t name in
	  let libs_indirect =
	    List.map (fun x -> loop libs_seen (`Lib x)) libs_direct
	    |> List.flatten
	  in
	  libs_direct@libs_indirect
      )
      | `App _ -> (
	let libs_direct = libs_direct t name in
	let libs_indirect =
	  List.map (fun x -> loop libs_seen (`Lib x)) libs_direct
	  |> List.flatten
	in
	libs_direct@libs_indirect
      )
    in
    loop [] name
    |> List.sort_uniq compare

  let pkgs_direct t name = (get t name).pkgs

  let pkgs_all t name =
    let rec loop libs_seen name : string list =
      match name with
      | `Lib lib -> (
	if List.mem lib libs_seen then
	  failwith "cycle detected in Info.data"
	else
	  let libs_seen = lib::libs_seen in
	  let libs_direct = libs_direct t name in
	  let pkgs_direct = pkgs_direct t name in
	  let pkgs_indirect =
	    List.map (fun x -> loop libs_seen (`Lib x)) libs_direct
	    |> List.flatten
	  in
	  pkgs_direct@pkgs_indirect
      )
      | `App _ -> (
	let libs_direct = libs_direct t name in
	let pkgs_direct = pkgs_direct t name in
	let pkgs_indirect =
	  List.map (fun x -> loop libs_seen (`Lib x)) libs_direct
          |> List.flatten
	in
	pkgs_direct@pkgs_indirect
      )
    in
    loop [] name
    |> List.sort_uniq compare

end

module type PROJECT = sig
  val name : string
  val version : string
  val info : Info.t
end

module Make(Project:PROJECT) : sig
  val dispatch : unit -> unit
end = struct
  open Printf
  open Ocamlbuild_plugin

  let failwithf fmt = ksprintf (fun s () -> failwith s) fmt
  let dash_to_underscore x = String.map (function '-' -> '_' | c -> c) x

  let all_libs : string list =
    let found =
      Sys.readdir "lib" |> Array.to_list
      |> List.filter (fun x -> Sys.is_directory ("lib"/x))
      |> List.sort compare
    in
    let given =
      Info.libs Project.info
      |> Info.names
      |> List.sort compare
    in
    assert (found=given);
    given

  let all_apps : string list =
    let found =
      Sys.readdir "app" |> Array.to_list
      |> List.map Filename.chop_extension
      |> List.sort compare
    in
    let given =
      Info.apps Project.info
      |> Info.names
      |> List.sort compare
    in
    assert (found=given);
    given

  let lib_name lib = sprintf "%s-%s" Project.name lib

  let git_commit =
    if Sys.file_exists ".git" then
      sprintf "Some \"%s\""
	(
	  Ocamlbuild_pack.My_unix.run_and_read "git rev-parse HEAD"
	  |> fun x -> String.sub x 0 (String.length x - 1)
	)
    else
      "None"

  let tags_lines : string list =
    [
      "true: thread, bin_annot, annot, short_paths, safe_string, debug";
      "\"lib\": include";
    ]
    @(List.map
	(fun x ->
	  sprintf
	    "<lib/%s/*.cmx>: for-pack(%s_%s)"
	    x (String.capitalize Project.name) (dash_to_underscore x)
	)
	all_libs
    )
    @(
      let libs = (Info.libs Project.info :> Info.item list) in
      List.map
	(fun lib -> lib.Info.name, Info.pkgs_all Project.info lib.Info.name)
	libs
      |> List.filter (function (_,[]) -> false | (_,_) -> true)
      |> List.map (fun (name,pkgs) ->
	sprintf "<lib/%s/*>: %s"
	  (Info.name_as_string name)
	  (String.concat ", " (List.map (sprintf "package(%s)") pkgs))
      )
    )
    @(
      let apps = (Info.apps Project.info :> Info.item list) in
      List.map
	(fun app -> app.Info.name, Info.pkgs_all Project.info app.Info.name)
	apps
      |> List.filter (function (_,[]) -> false | (_,_) -> true)
      |> List.map (fun (name,pkgs) ->
	sprintf "<app/%s.*>: %s"
	  (Info.name_as_string name)
	  (String.concat "," (List.map (sprintf "package(%s)") pkgs))
      )
    )

  let merlin_file : string list =
    [
      "S ./lib/**";
      "S ./app/**";
      "B ./_build/lib";
      "B ./_build/lib/**";
      "B ./_build/app/**";
      "B +threads";
    ]
    @(
      List.map (fun x -> x.Info.pkgs) (Project.info :> Info.item list)
      |> List.flatten
      |> List.sort_uniq compare
      |> List.map (fun x -> sprintf "PKG %s" x)
    )
    |> List.map (sprintf "%s\n") (* I think due to bug in ocamlbuild. *)

  let meta_file : string list =
    List.map (fun x ->
      let lib_name = lib_name x in
      let requires : string list =
	(Info.pkgs_all Project.info (`Lib x))
	@(List.map
	    (sprintf "%s.%s" Project.name)
	    (Info.libs_direct Project.info (`Lib x))
	)
      in
      [
	sprintf "package \"%s\" (" x;
	sprintf "  version = \"%s\"" Project.version;
	sprintf "  archive(byte) = \"%s.cma\"" lib_name;
	sprintf "  archive(native) = \"%s.cmxa\"" lib_name;
	sprintf "  requires = \"%s\"" (String.concat " " requires);
	sprintf "  exists_if = \"%s.cma\"" lib_name;
	sprintf ")";
      ]) all_libs
    |> List.flatten
    |> List.filter ((<>) "")
    |> List.map (sprintf "%s\n") (* I think due to bug in ocamlbuild. *)

  let make_static_file path contents =
    rule path ~prod:path (fun _ _ -> Echo (contents,path))

  let dispatch () = dispatch (function
    | Before_options -> (
      Options.use_ocamlfind := true;
      List.iter Ocamlbuild_pack.Configuration.parse_string tags_lines
    )
    | After_rules -> (
      rule "m4: ml.m4 -> ml"
	~prod:"%.ml"
	~dep:"%.ml.m4"
	(fun env _ ->
	  let ml_m4 = env "%.ml.m4" in
	  Cmd (S [
	    A "m4";
	    A "-D"; A ("VERSION=" ^ Project.version);
            A "-D"; A ("GIT_COMMIT=" ^ git_commit);
	    P ml_m4;
	    Sh ">";
	    P (env "%.ml");
	  ]) )
      ;

      make_static_file ".merlin" merlin_file;
      make_static_file "META" meta_file;

      rule "project files"
	~stamp:"project_files.stamp"
	(fun _ build ->
	  let project_files = [[".merlin"]] in
	  List.map Outcome.good (build project_files)
	  |> List.map (fun result ->
	    Cmd (S [A "ln"; A "-sf";
		    P (!Options.build_dir/result);
		    P Pathname.pwd] )
	  )
	  |> fun l -> Seq l
	)
    )
    | _ -> ()
  )

end

include Make(struct
  let name = "phat"
  let version = "dev"

  let info = Info.of_list [
    {
      Info.name = `Lib "pure";
      libs = [];
      pkgs = ["core_kernel"; "ppx_sexp_conv"; "ppx_here"];
    };

    {
      Info.name = `Lib "async-unix";
      libs = ["pure"];
      pkgs = ["async"];
    };

    {
      Info.name = `App "run_ounit_tests";
      libs = ["async-unix"];
      pkgs = ["oUnit"];
    };
  ]

end)

let () = dispatch()
