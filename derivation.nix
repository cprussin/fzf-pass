{
  haskell,
  haskellPackages,
  makeWrapper,
  lib,
  callPackage,
}: let
  runtimeDeps = callPackage ./runtimeDeps.nix {};
  binPath = lib.makeBinPath runtimeDeps;
  cabalPkg = haskellPackages.callCabal2nix "fzf-pass" ./. {};
in
  haskell.lib.overrideCabal cabalPkg (drv: {
    buildDepends = (drv.buildDepends or []) ++ [makeWrapper];
    postInstall = ''
      ${drv.postInstall or ""}
      wrapProgram $out/bin/fzf-pass --prefix PATH ":" ${binPath}
    '';
  })
