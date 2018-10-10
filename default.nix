{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};
  haskellPackages = ghc.override {
    overrides = self: super: {
      # working around this: https://github.com/nikita-volkov/html-entities/issues/8
      html-entities = pkgs.haskell.lib.dontCheck (self.callHackage "html-entities" "1.1.4.1" {});
    };
  };
  drv = haskellPackages.callPackage ./reflex-dom-template.nix {};
in
  drv
