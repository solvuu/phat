(** Build system. *)
open Printf
let failwithf fmt = ksprintf (fun s () -> failwith s) fmt

module List = struct
  include List
  include ListLabels
end

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

  let names t = List.map t ~f:(fun x -> match x.name with `Lib s | `App s -> s)
  let name_as_string = function `Lib x | `App x -> x

  let is_uniq (l : string list) : bool =
    let m = List.length l in
    let n = List.length (List.sort_uniq compare l) in
    m = n

  let libs t = List.filter ~f:is_lib t
  let apps t = List.filter ~f:is_app t

  let get t name = List.find t ~f:(fun x -> x.name = name)

  (** Check that given item's [libs] dependencies do not lead to a
      cycle. *)
  let assert_no_cycle t item : unit =
    let visited = ref [] in
    let rec loop item =
      match item.name with
      | `App _ -> ()
      | `Lib lib ->
	 if List.mem lib ~set:!visited then
	   failwithf "cycle involving %s detected in Info.t" lib ()
	 else (
	   visited := lib::!visited;
	   let libs = List.map item.libs ~f:(fun x -> get t (`Lib x)) in
	   List.iter libs ~f:loop
	 )
    in
    loop item

  let of_list items =
    let libs = names (libs items) in
    let apps = names (apps items) in
    if not (is_uniq libs) then
      failwith "lib names must be unique"
    else if not (is_uniq apps) then
      failwith "app names must be unique"
    else (
      List.iter items ~f:(assert_no_cycle items);
      items
    )

  let libs_direct t name = (get t name).libs

  let rec libs_all t name =
    let item = get t name in
    item.libs
    @(
      List.map item.libs ~f:(fun x -> libs_all t (`Lib x))
      |> List.flatten
    )
    |> List.sort_uniq compare

  let pkgs_direct t name = (get t name).pkgs

  let rec pkgs_all t name =
    let item = get t name in
    item.pkgs
    @(
      List.map item.libs ~f:(fun x -> pkgs_all t (`Lib x))
      |> List.flatten
    )
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
  open Ocamlbuild_plugin

  (* override the one from Ocamlbuild_plugin *)
  module List = struct
    include List
    include ListLabels
  end

  let all_libs : string list =
    let found =
      Sys.readdir "lib" |> Array.to_list
      |> List.filter ~f:(fun x -> Sys.is_directory ("lib"/x))
      |> List.sort ~cmp:compare
    in
    let given =
      Info.libs Project.info
      |> Info.names
      |> List.sort ~cmp:compare
    in
    assert (found=given);
    given

  let all_apps : string list =
    let found =
      Sys.readdir "app" |> Array.to_list
      |> List.map ~f:Filename.chop_extension
      |> List.sort ~cmp:compare
    in
    let given =
      Info.apps Project.info
      |> Info.names
      |> List.sort ~cmp:compare
    in
    assert (found=given);
    given

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
    @(List.map all_libs ~f:(fun x ->
      sprintf
	"<lib/%s/*.cmx>: for-pack(%s_%s)"
	x (String.capitalize Project.name) x )
    )
    @(
      let libs = (Info.libs Project.info :> Info.item list) in
      List.map libs ~f:(fun lib ->
	lib.Info.name, Info.pkgs_all Project.info lib.Info.name
      )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (name,pkgs) ->
	sprintf "<lib/%s/*>: %s"
	  (Info.name_as_string name)
	  (String.concat ", " (List.map pkgs ~f:(sprintf "package(%s)")))
      )
    )
    @(
      let apps = (Info.apps Project.info :> Info.item list) in
      List.map apps ~f:(fun app ->
	app.Info.name, Info.pkgs_all Project.info app.Info.name )
      |> List.filter ~f:(function (_,[]) -> false | (_,_) -> true)
      |> List.map ~f:(fun (name,pkgs) ->
	sprintf "<app/%s.*>: %s"
	  (Info.name_as_string name)
	  (String.concat "," (List.map pkgs ~f:(sprintf "package(%s)")))
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
      List.map (Project.info :> Info.item list) ~f:(fun x -> x.Info.pkgs)
      |> List.flatten
      |> List.sort_uniq compare
      |> List.map ~f:(fun x -> sprintf "PKG %s" x)
    )
    |> List.map ~f:(sprintf "%s\n") (* I think due to bug in ocamlbuild. *)

  let meta_file : string list =
    List.map all_libs ~f:(fun x ->
      let lib_name = sprintf "%s_%s" Project.name x in
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
      ]
    )
    |> List.flatten
    |> List.filter ~f:((<>) "")
    |> List.map ~f:(sprintf "%s\n") (* I think due to bug in ocamlbuild. *)

  let install_file : string list =
    let suffixes = [
      "a";"annot";"cma";"cmi";"cmo";"cmt";"cmti";"cmx";"cmxa";
      "cmxs";"dll";"o";"so"]
    in
    let lib_files =
      List.map all_libs ~f:(fun lib ->
	List.map (fun suffix ->
	  sprintf "  \"?_build/lib/%s_%s.%s\""
	    Project.name lib suffix
	) suffixes
      )
      |> List.flatten
      |> fun l -> "  \"_build/META\""::l
    in
    let app_files =
      List.map all_apps ~f:(fun app ->
	List.map (fun suffix ->
	  sprintf "  \"?_build/app/%s.%s\" {\"%s\"}" app suffix app
	) ["byte"; "native"]
      )
      |> List.flatten
    in
    ["lib: ["]@lib_files@["]"; ""; "bin: ["]@app_files@["]"]
    |> List.map ~f:(sprintf "%s\n") (* I think due to bug in ocamlbuild. *)

  let make_static_file path contents =
    rule path ~prod:path (fun _ _ -> Echo (contents,path))

  let dispatch () = dispatch (function
    | Before_options -> (
      Options.use_ocamlfind := true;
      List.iter tags_lines ~f:Ocamlbuild_pack.Configuration.parse_string
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
      make_static_file (sprintf "%s.install" Project.name) install_file;

      rule "project files"
	~stamp:"project_files.stamp"
	(fun _ build ->
	  let project_files = [[
	    ".merlin";
	    sprintf "%s.install" Project.name;
	  ]]
	  in
	  List.map (build project_files) ~f:Outcome.good
	  |> List.map ~f:(fun result ->
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
      Info.name = `Lib "async_unix";
      libs = ["pure"];
      pkgs = ["async"];
    };

    {
      Info.name = `App "phat_tests";
      libs = ["async_unix"];
      pkgs = ["oUnit"];
    };
  ]

end)

let () = dispatch()
