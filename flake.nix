{
  description = "A language server for Jasmin based on LSP";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";

  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:

    let pkgs = import nixpkgs { inherit system; }; in

    rec {

     packages.default = self.packages.${system}.jasmin-language-server;

     packages.jasmin-language-server =
       pkgs.ocamlPackages.buildDunePackage {
         duneVersion = "3";
         pname = "jasmin-language-server";
         version = "0.0.1";
         src = ./.;
         buildInputs = [
            pkgs.dune_3
          ] ++ pkgs.jasmin-compiler.buildInputs
          ++ (with pkgs.ocamlPackages; [
           ocaml
           yojson
           findlib
           ppx_inline_test
           ppx_assert
           ppx_sexp_conv
           ppx_deriving
           sexplib
           ppx_yojson_conv
           jsonrpc
           lsp
           menhirLib
           sel
         ]);
         propagatedBuildInputs = 
            pkgs.jasmin-compiler.propagatedBuildInputs;
         nativeBuildInputs = 
            pkgs.jasmin-compiler.nativeBuildInputs;
       };

     devShells.default =
       with import nixpkgs { inherit system; };
       mkShell {
         buildInputs =
           self.packages.${system}.jasmin-language-server.buildInputs
           ++ (with ocamlPackages; [
             ocaml-lsp
           ]);
          propagatedBuildInputs = self.packages.${system}.jasmin-language-server.propagatedBuildInputs;
          nativeBuildInputs = self.packages.${system}.jasmin-language-server.nativeBuildInputs;
       };

  });
}
