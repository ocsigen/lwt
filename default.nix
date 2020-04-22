{ pkgs ? import <nixpkgs> {} }:
  pkgs.ocamlPackages.callPackage ./pkg.nix {}
