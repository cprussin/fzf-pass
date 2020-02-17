{ sources ? import ./sources.nix }:

let
  niv-overlay = self: _: {
    niv = self.symlinkJoin {
      name = "niv";
      paths = [ sources.niv ];
      buildInputs = [ self.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/niv \
          --add-flags "--sources-file ${toString ./sources.json}"
      '';
    };
  };
  fzf-pass-overlay = import ./overlay.nix;
  pkgs = import sources.nixpkgs {
    overlays = [ niv-overlay fzf-pass-overlay ];
  };
  runtimeDeps = pkgs.callPackage ./runtimeDeps.nix {};
  run = pkgs.writeShellScriptBin "run" "cabal new-run";
  build = pkgs.writeShellScriptBin "build" "cabal new-build";
  repl = pkgs.writeShellScriptBin "repl" "cabal new-repl";
  clean = pkgs.writeShellScriptBin "clean" "cabal new-clean";
in

pkgs.mkShell {
  inputsFrom = [
    pkgs.fzf-pass.env
  ];
  buildInputs = runtimeDeps ++ [
    pkgs.git
    pkgs.cabal-install
    pkgs.niv
    run
    build
    repl
    clean
  ];
}
