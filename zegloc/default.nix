{stdenv, haskellPackages}:
let
    ghc = haskellPackages.ghcWithPackages (p: [
        p.alex
        p.ansi-wl-pprint
        p.base
        p.dlist
        p.hashable
        p.lens
        p.text
        p.unordered-containers
        p.vector
    ]);
in
stdenv.mkDerivation {
    name = "zegloc";
    src = ./.;
    buildInputs = [ghc];
    phases = ["unpackPhase" "buildPhase"
              "installPhase" "fixupPhase"];
    unpackPhase = ''
        # GHC make mode is a convention over configuration mechanism, so we
        # have to write some configuration to put our files in the correct
        # place according to the convention.
        cp --no-preserve=mode --recursive $src Zegloc
    '';
    buildPhase = ''
        ghcFlags=(
            -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns
            -O
        )

        alex Zegloc/Lex.x

        echo 'import Zegloc.Main (main)' > Main.hs

        ghc "''${ghcFlags[@]}" Main.hs
    '';
    installPhase = ''
        mkdir --parents $out/bin
        mv Main $out/bin/zegloc
    '';
}
