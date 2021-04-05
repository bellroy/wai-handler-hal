{ sources ? import ./nix/sources.nix { }
, compiler-nix-name ? "ghc8104"
}:
let
  project = import ./. { inherit sources compiler-nix-name; };
  niv = (import sources.niv { }).niv;
  ormolu = (import sources.ormolu { }).ormolu;
  pkgs = (import sources."haskell.nix" { }).pkgs;
  nixpkgs-fmt = pkgs.nixpkgs-fmt;
  nodejs = pkgs.nodejs;
  npm = pkgs.nodePackages.npm;
in
project.shellFor {
  packages = ps: with ps; [ wai-handler-hal wai-handler-hal-example ];
  tools = {
    cabal-fmt = { };
    haskell-ci = {
      modules = [{ reinstallableLibGhc = true; }];
    };
    hlint = { };
  };
  buildInputs = [ niv nixpkgs-fmt nodejs npm ormolu ];
}
