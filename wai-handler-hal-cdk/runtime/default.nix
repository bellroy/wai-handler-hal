# Build a stripped, statically-linked version of our example Lambda
# for deployment. This expression produces a directory containing the
# `bootstrap` executable: CDK is smart enough to zip it up for deployment.
{ sources ? import ../../nix/sources.nix { }
, compiler-nix-name ? "ghc8104"
}:
let
  haskellNix = import sources."haskell.nix" { };
  pkgs = haskellNix.pkgs;
  pkgsMusl = haskellNix.pkgs.pkgsCross.musl64;
  project = pkgsMusl.haskell-nix.project {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "wai-handler-hal";
      src = ../..;
    };
    modules = [{
      # Set this only for packages providing the final binaries that
      # go to AWS, unless you want to rebuild the entire universe.
      packages.wai-handler-hal-example.dontStrip = false;
    }];
  };
  lambdaBinary = "${project.wai-handler-hal-example.components.exes.wai-handler-hal-example}/bin/wai-handler-hal-example";
  runtime = pkgs.runCommand "wai-handler-hal-example-runtime" { } ''
    mkdir $out
    ${pkgs.upx}/bin/upx -9 -o $out/bootstrap ${lambdaBinary}
  '';
in
runtime
