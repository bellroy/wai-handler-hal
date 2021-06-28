{ sources ? import ./nix/sources.nix { }
, compiler-nix-name ? "ghc8105"
}:
let
  haskellNix = import sources."haskell.nix" { };
  pkgs = haskellNix.pkgs;
in
haskellNix.pkgs.haskell-nix.project {
  inherit compiler-nix-name;
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "wai-handler-hal";
    src = ./.;
  };
}
