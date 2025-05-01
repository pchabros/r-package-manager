{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } {

      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        {
          self',
          pkgs,
          config,
          ...
        }:
        {
          haskellProjects.default = {
            packages = { };
            settings = { };
            devShell = { };
            autoWire = [
              "packages"
              "apps"
              "checks"
            ];
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
            packages = with pkgs; [ just ];
          };
        };
    };
}
