open Solvuu_build

include Make(struct
  let name = "phat"
  let version = "dev"

  let info = Info.of_list [
    {
      Info.name = `Lib "pure";
      libs = [];
      pkgs = ["core_kernel"; "ppx_sexp_conv"; "ppx_here"];
      build_if = [];
    };

    {
      Info.name = `Lib "async";
      libs = ["pure"];
      pkgs = ["async"];
      build_if = [`Pkgs_installed];
    };

    {
      Info.name = `App "phat_tests";
      libs = ["async"];
      pkgs = ["oUnit"];
      build_if = [`Pkgs_installed];
    };

    {
      Info.name = `App "phat";
      libs = ["async"];
      pkgs = [];
      build_if = [`Pkgs_installed];
    };
  ]

  let ocamlinit_postfix = [
    "open Phat_async.Std";
  ]

end)

let () = dispatch()
