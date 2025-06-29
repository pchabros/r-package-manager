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
        inherit (pkgs) fetchurl lib mkShell rPackages;
        inherit (lib) importJSON mapAttrsToList;
        packages =
          mapAttrsToList (
            name: data:
              rPackages.${name}.overrideAttrs (_: {
                inherit (data) version;
                src = fetchurl {
                  url = data.resolved;
                  sha256 = data.integrity;
                };
              })
          )
          (importJSON ./r-pm-lock.json).dependencies;
      in {
        devShells.default = mkShell {
          packages = with pkgs; [
            (radianWrapper.override {inherit packages;})
          ];
        };
      };
    });
}
