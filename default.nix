{ sources ? import ./sources.nix }:

let
  fzf-pass-overlay = import ./overlay.nix;
  pkgs = import sources.nixpkgs { overlays = [ fzf-pass-overlay ]; };
in

pkgs.fzf-pass
