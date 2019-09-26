{ ... }:
let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  niv = import sources.niv { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elm-trend";
  buildInputs = [ niv.niv elmPackages.elm elmPackages.elm-test ];
}
