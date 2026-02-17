{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs-18_x
    nodePackages.pnpm
    nodePackages.npm-check-updates
  ];
}
