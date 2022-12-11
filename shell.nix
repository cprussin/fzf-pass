{
  sources ? import ./sources.nix,
  nixpkgs ? sources.nixpkgs,
  niv ? sources.niv,
  alejandra ? sources.alejandra,
}: let
  niv-overlay = self: _: {
    niv = self.symlinkJoin {
      name = "niv";
      paths = [niv];
      buildInputs = [self.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/niv \
          --add-flags "--sources-file ${toString ./sources.json}"
      '';
    };
  };

  alejandra-overlay = _: _: {
    alejandra = import alejandra {};
  };

  fzf-pass-overlay = import ./overlay.nix;

  pkgs = import nixpkgs {
    overlays = [niv-overlay alejandra-overlay fzf-pass-overlay];
  };

  runtimeDeps = pkgs.callPackage ./runtimeDeps.nix {};

  nix-files = "$(find . -name node_modules -prune -o -name '*.nix' -print)";

  scripts = pkgs.symlinkJoin {
    name = "scripts";
    paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin {
      build = "${pkgs.cabal-install}/bin/cabal new-build";
      check = "check-code-nix && check-format-nix";
      check-code-nix = "${pkgs.nix-linter}/bin/nix-linter ${nix-files}";
      check-format-nix = "${pkgs.alejandra}/bin/alejandra --check ${nix-files}";
      clean = "${pkgs.cabal-install}/bin/cabal new-clean";
      fix = "fix-format-nix";
      fix-format-nix = "${pkgs.alejandra}/bin/alejandra ${nix-files}";
      repl = "${pkgs.cabal-install}/bin/cabal new-repl";
      run = "${pkgs.cabal-install}/bin/cabal new-run";
    };
  };
in
  pkgs.mkShell {
    inputsFrom = [
      pkgs.fzf-pass.env
    ];
    buildInputs =
      runtimeDeps
      ++ [
        pkgs.git
        pkgs.cabal-install
        pkgs.niv
        scripts
      ];
  }
