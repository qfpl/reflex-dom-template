{ mkDerivation, base, containers, dependent-map, html-entities
, html-parse, lens, mtl, reflex, reflex-dom-core, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-template";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers dependent-map html-entities html-parse lens mtl
    reflex reflex-dom-core text
  ];
  license = stdenv.lib.licenses.bsd3;
}
