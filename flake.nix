{
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs = inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      cabalPackages = [
        {
          name = "wai-handler-hal";
          path = ./wai-handler-hal.nix;
        }
      ];
      supportedCompilers = [ "ghc8107" "ghc92" "ghc94" ];
      defaultCompiler = "ghc92";
      haskellPackagesOverride = { compilerName, haskellLib, final, prev }:
        if compilerName == "ghc94"
        then {
          # hal doesn't support newer hedgehog
          hedgehog = haskellLib.compose.dontCheck (prev.callHackage "hedgehog" "1.1.2" { });
        }
        else { };
    };
}
