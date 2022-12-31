{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    packages =
         [
            (pkgs.haskellPackages.ghcWithPackages (hpkgs : [hpkgs.lens hpkgs.servant hpkgs.servant-server hpkgs.warp]))
             pkgs.haskellPackages.cabal-install
             pkgs.haskellPackages.haskell-language-server
         ];
}
