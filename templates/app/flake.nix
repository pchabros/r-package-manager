{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} (_: {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      perSystem = {pkgs, ...}: let
        inherit (pkgs) lib rPackages mkShell;
        inherit (lib) getAttr flip importJSON;
        packages = map (flip getAttr rPackages) (importJSON ./r-pm.json).packages;
      in {
        devShells.default = mkShell {
          packages = with pkgs; [
            (radianWrapper.override {inherit packages;})
          ];
        };
      };
    });
}
