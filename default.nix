{ mkDerivation, base, pure-core, pure-default, containers, stdenv }:
mkDerivation {
  pname = "pure-cache";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-default containers ];
  homepage = "github.com/grumply/pure-cache";
  description = "Cache decorator";
  license = stdenv.lib.licenses.bsd3;
}
