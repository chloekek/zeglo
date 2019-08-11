let
    nixpkgs = import ./nix/nixpkgs.nix {};
in
{
    zegloc = nixpkgs.callPackage ./zegloc {};
    zeglor = nixpkgs.callPackage ./zeglor {};
}
