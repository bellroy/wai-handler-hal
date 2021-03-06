{ sources ? import ./nix/sources.nix { }
, compiler-nix-name ? "ghc922"
, withHoogle ? false
}:
let
  project = import ./. { inherit sources compiler-nix-name; };
  pkgs = (import sources."haskell.nix" { }).pkgs-unstable;
in
project.shellFor {
  inherit withHoogle;

  packages = ps: with ps; [ wai-handler-hal wai-handler-hal-example ];
  tools.haskell-ci = "latest";

  buildInputs = with pkgs; [
    niv
    nixpkgs-fmt
    nodejs
    nodePackages.npm
    ormolu
  ] ++ (with pkgs.haskellPackages; [
    cabal-fmt
    hlint
  ]);
}
