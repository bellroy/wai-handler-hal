{
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs = inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      src = ./.;
      supportedCompilers = [
        "ghc8107"
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
      ];
      defaultCompiler = "ghc96";
    };
}
