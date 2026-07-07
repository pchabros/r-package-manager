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
      perSystem = {
        system,
        pkgs,
        ...
      }: let
        inherit (pkgs.lib) importJSON mapAttrsToList;

        lock = importJSON ./r-pm-lock.json;
        pinned-pkgs = import (fetchTarball {
          url = lock.nixpkgs.resolved;
          sha256 = lock.nixpkgs.integrity;
        }) {inherit system;};

        inherit (pinned-pkgs) fetchurl mkShell rPackages;

        packages =
          mapAttrsToList (
            name: data:
              rPackages.${name}.overrideAttrs (_: {
                src = fetchurl {
                  url = data.resolved;
                  sha256 = data.integrity;
                };
              })
          )
          lock.dependencies;
      in {
        devShells.default = mkShell {
          packages = with pinned-pkgs; [
            (radianWrapper.override {inherit packages;})
          ];
        };
      };
    });
}
