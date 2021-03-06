{ pkgs ? import ./sources.nix {}, doCheck ? true, ocamlVersion ? "4_10" }:

let
  inherit (pkgs) lib stdenv ocamlPackages;
in

  with ocamlPackages;

  let
    genSrc = { dirs, files }: lib.filterGitSource {
      src = ./..;
      inherit dirs;
      files = files ++ [ "dune-project" ];
    };
    buildPkg = args: buildDunePackage ({
      version = "0.13.0-dev";
      doCheck = doCheck;
    } // args);

  in

  rec {
    graphql_parser = buildPkg {
      pname = "graphql_parser";
      src = genSrc {
        dirs = [ "graphql_parser" ];
        files = [ "graphql_parser.opam" ];
      };

      checkInputs = [ alcotest ];
      propagatedBuildInputs = [
        menhir
        fmt
        re
      ];
    };

    graphql = buildPkg {
      pname = "graphql";
      src = genSrc {
        dirs = [ "graphql" ];
        files = [ "graphql.opam" ];
      };
      checkInputs = [ alcotest ];
      propagatedBuildInputs = [
        graphql_parser
        yojson
        rresult
        seq
      ];
    };

    graphql-lwt = buildPkg {
      pname = "graphql-lwt";
      src = genSrc {
        dirs = [ "graphql-lwt" ];
        files = [ "graphql-lwt.opam" ];
      };

      checkInputs = [ alcotest ];

      inherit doCheck;
      propagatedBuildInputs = [
        graphql
        lwt
      ];
    };

  }
