{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs @ {
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = {
        self',
        pkgs,
        config,
        ...
      }: {
        haskellProjects.default.autoWire = ["packages" "checks"];
        packages.default = self'.packages.r-package-manager;
        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = with pkgs; [just];
        };
      };
      flake.templates = {
        app = {
          path = ./templates/app;
          description = "R Package Manager application";
        };
      };
    };
}
