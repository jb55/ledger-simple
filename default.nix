{ mkDerivation, base, formatting, semigroupoids, stdenv, text, time
}:
mkDerivation {
  pname = "ledger-simple";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base formatting semigroupoids text time
  ];
  testHaskellDepends = [ base time ];
  homepage = "https://github.com/jb55/ledger-simple";
  description = "Quickly render ledger-cli documents";
  license = stdenv.lib.licenses.mit;
}
