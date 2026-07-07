#!/usr/bin/env nix-shell
#!nix-shell -i bash -p bash git jq nix

set -euo pipefail

branches=$(
  git ls-remote --heads https://github.com/NixOS/nixpkgs.git |
    grep -oE 'refs/heads/nixos-[0-9]{2}\.[0-9]{2}$' |
    sed 's|refs/heads/||' |
    sort -V
)

{
  for branch in $branches; do
    r_full=$(nix eval --raw "github:nixos/nixpkgs/${branch}#R.version" 2>/dev/null || true)
    [ -n "$r_full" ] || continue
    r_minor="${r_full%.*}"
    printf '{"%s":"%s"}\n' "$r_minor" "$branch"
  done
} | jq -s 'add' >r-versions.json
