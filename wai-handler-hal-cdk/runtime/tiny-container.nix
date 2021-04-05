# This file uses `streamLayeredImage` to build a minimal
# container. Instead of generating a tarball in the store,
# `streamLayeredImage` generates a script that dumps the container to
# stdout. After building, you'll want to do something like `./result |
# docker load`, and then tag and push.
{ sources ? import ../../nix/sources.nix { }
, compiler-nix-name ? "ghc8104"
}:
let
  haskellNix = import sources."haskell.nix" { };
  pkgs = haskellNix.pkgs;
in
pkgs.dockerTools.streamLayeredImage {
  name = "example-container";
  tag = "latest";

  contents =
    let
      bootstrap = import ./. { inherit sources compiler-nix-name; };

      # We run a tiny shell script to decide whether we need to
      # execute the runtime-interface-emulator. The simplest shell we
      # can get is busybox, statically linked against musl.
      busybox = pkgs.pkgsCross.musl64.busybox.override {
        enableStatic = true;
        enableMinimal = true;
      };

      # Grab the runtime interface emulator from a GitHub release.
      runtime-interface-emulator = builtins.fetchurl {
        url = "https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/download/v1.0/aws-lambda-rie";
        sha256 = "1x0200q4cnwwjiqsdqqm1ypz5hah9v7fzdc66kqw9sv3j1da11d4";
      };

      # This is basically the same as /lambda-entrypoint.sh in a real
      # Amazon container image.
      entrypoint = pkgs.writeScript "lambda-entrypoint.sh" ''
        #!${busybox}/bin/sh
        if [ -z "''${AWS_LAMBDA_RUNTIME_API}" ]; then
          exec /usr/local/bin/aws-lambda-rie /var/runtime/bootstrap
        else
          exec /var/runtime/bootstrap
        fi
      '';
    in
    # Assemble the filesystem layout for the container.
    pkgs.runCommandLocal "container-contents" { } ''
      mkdir -p $out/usr/local/bin $out/var/runtime $out/var/task
      cp ${entrypoint} $out/lambda-entrypoint.sh
      cp ${runtime-interface-emulator} $out/usr/local/bin/aws-lambda-rie
      chmod +x $out/usr/local/bin/aws-lambda-rie
      cp ${bootstrap}/bootstrap $out/var/runtime/bootstrap
    '';

  # Config is unchanged from `container.nix`; there is no
  # technical requirement for EntryPoint or WorkingDir to have these
  # values but they need to be set to something. We reuse the values
  # from Amazon's base image to minimise surprise.
  config = {
    Cmd = [ "UNUSED" ];
    EntryPoint = [ "/lambda-entrypoint.sh" ];
    WorkingDir = "/var/task";
  };
}
