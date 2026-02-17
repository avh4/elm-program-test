{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs_24
    nodePackages.pnpm
    nodePackages.npm-check-updates
  ];
}
