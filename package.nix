{ mkDerivation, aeson, base, base64-bytestring, bytestring, case-insensitive, hal, http-types,
  network, pretty-simple, unordered-containers, vault, wai, lib, tasty, tasty-discover, tasty-golden, text
}:
mkDerivation {
  pname = "wai-handler-hal";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bytestring case-insensitive hal http-types network text unordered-containers vault wai
  ];
  testHaskellDepends = [
    aeson base pretty-simple tasty tasty-golden text
  ];
  testToolDepends = [ tasty-discover ];
  description = "Wrap WAI applications to run on AWS Lambda";
  license = lib.licenses.bsd3;
}
