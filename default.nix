{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
, myHaskellPackages ? (import <nixpkgs> {}).myHaskellPackages
}:
let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_3
    hakyll silently lens;
  inherit (myHaskellPackages)
    BlogLiterately;
in cabal.mkDerivation (self: {
  pname = "dpwright.com";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    hakyll silently lens BlogLiterately
  ];
  buildTools = [ cabalInstall_1_18_0_3 ];
  enableSplitObjs = false;
})
