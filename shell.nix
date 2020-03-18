{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  myClay = pkgs.fetchFromGitHub {
    owner = "sebastiaanvisser";
    repo = "clay";
    rev = "3808e460c044809f088ac8952ed3720cf801daf3";
    sha256 = "1jdl4422wp61gxclsz6k5gy0l80q9dachz6vmhfqpzarcrki8viw";
    };
  myRib = pkgs.fetchFromGitHub {
    owner = "srid";
    repo = "rib";
    rev = "729bcd69942675a9b84b532a0939dc47698fc9fc";
    sha256 = "0wr7r2200w5z10yy64jgm0j973yh814m8w05fxds8nzldhi05v8k";
  };
  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      clay = super.callCabal2nix "clay" myClay {};
      rib = super.callCabal2nix "rib" myRib {};
    };
  };
  ghc = myHaskellPackages.ghcWithPackages (ps: with ps; [
    rib
    clay
    PyF
    ghcid
    cabal-install
    with-utf8
  ]);
in
pkgs.mkShell {
  name = "my-haskell-env";
  buildInputs = [ ghc ];
}
