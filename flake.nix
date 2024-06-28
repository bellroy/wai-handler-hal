{
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs = inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      supportedCompilers = [
        "ghc8107"
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
      ];
      defaultCompiler = "ghc96";
    };
}
