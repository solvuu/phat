open Solvuu_build

include Make(struct
  let info = Info.of_list [
    {
      Info.name = `Lib "pure";
      libs = [];
      pkgs = ["core_kernel"; "ppx_sexp_conv"; "ppx_here"];
    };

    {
      Info.name = `Lib "async";
      libs = ["pure"];
      pkgs = ["async"];
    };

    {
      Info.name = `App "phat_tests";
      libs = ["async"];
      pkgs = ["oUnit"];
    };

    {
      Info.name = `App "phat";
      libs = ["async"];
      pkgs = [];
    };
  ]

  let ocamlinit_postfix = [
    "open Phat_async.Std";
  ]

end)

let () = dispatch()
