{ sources ? import ./nix/sources.nix { }
, compiler-nix-name ? "ghc8107"
, withHoogle ? false
}:
let
  project = import ./. { inherit sources compiler-nix-name; };
  pkgs = (import sources."haskell.nix" { }).pkgs;
in
project.shellFor {
  inherit withHoogle;
  packages = ps: with ps; [ wai-handler-hal wai-handler-hal-example ];

  tools = {
    cabal-fmt = { };
    haskell-ci = {
      modules = [{ reinstallableLibGhc = true; }];
    };
    hlint = { };
    ormolu = { };
  };

  buildInputs = with pkgs; [
    niv
    nixpkgs-fmt
    nodejs
    nodePackages.npm
  ];
}
