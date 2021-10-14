# Build a stripped, statically-linked version of our example Lambda
# for deployment. This expression produces a directory containing the
# `bootstrap` executable: CDK is smart enough to zip it up for deployment.
{ sources ? import ../../nix/sources.nix { }
, compiler-nix-name ? "ghc8107"
, architecture ? "x86_64"
}:
let
  # Use local nixpkgs for UPX compression and cleaning the source
  # repository, so we don't distribute more builds than necessary.
  pkgsLocal = (import sources."haskell.nix" { }).pkgs-unstable;

  # Be explicit about the architecture of the system performing this
  # build, so that we can distribute builds if necessary, and we don't
  # get broken cross-build configurations (e.g., trying to build linux
  # musl64 from Darwin breaks).
  pkgsMusl =
    {
      x86_64 = (import sources."haskell.nix" {
        system = "x86_64-linux";
      }).pkgs-unstable.pkgsCross.musl64;

      # Cross-compile from x86_64 to ARM. This seems to want to build
      # a cross-GHC, as well as do some magic with qemu, both of which
      # take a very long time to build.
      #
      # It also seems to be broken at the moment - after making it
      # nearly to the end of the build, it failed with
      # remote-iserv: internal error: mmapForLinkerMarkExecutable: mprotect: Out of memory
      # which seems to be https://github.com/input-output-hk/haskell.nix/issues/1227
      arm64 = (import sources."haskell.nix" {
        system = "x86_64-linux";
      }).pkgs-unstable.pkgsCross.aarch64-multiplatform-musl;
    }.${architecture};

  project = pkgsMusl.haskell-nix.project {
    inherit compiler-nix-name;
    src = pkgsLocal.haskell-nix.haskellLib.cleanGit {
      name = "wai-handler-hal";
      src = ../..;
    };
    # This is usually fine, but can "occasionally cause breakage":
    # https://input-output-hk.github.io/haskell.nix/troubleshooting/#why-does-my-executable-depend-on-ghcgcc
    modules = [{
      # Set this only for packages providing the final binaries that
      # go to AWS, unless you want to rebuild the entire universe.
      packages.wai-handler-hal-example.dontStrip = false;
    }];
  };
  lambdaBinary = "${project.wai-handler-hal-example.components.exes.wai-handler-hal-example-hal}/bin/wai-handler-hal-example-hal";
  runtime = pkgsLocal.runCommand "wai-handler-hal-example-runtime" { } ''
    mkdir $out
    ${pkgsLocal.upx}/bin/upx -9 -o $out/bootstrap ${lambdaBinary}
  '';
in
runtime
