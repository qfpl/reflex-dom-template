{ mkDerivation, base, containers, html-parse, lens, mtl
, reflex-dom-core, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-template";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers html-parse lens mtl reflex-dom-core text
  ];
  license = stdenv.lib.licenses.bsd3;
}
