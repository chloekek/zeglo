{stdenv, boost, clang}:
stdenv.mkDerivation {
    name = "zeglor";
    src = ./.;
    buildInputs = [boost clang];
    phases = ["unpackPhase" "buildPhase" "installPhase"];
    buildPhase = ''
        clangFlags=(
            -std=c++17
            -W{all,extra,pedantic,error}
            -O2 -flto=thin
            -pthread
        )

        clang++ "''${clangFlags[@]}" -c heap.cpp
        clang++ "''${clangFlags[@]}" -c main.cpp
        clang++ "''${clangFlags[@]}" -c stack.cpp
        clang++ "''${clangFlags[@]}" -c value.cpp
        clang++ "''${clangFlags[@]}" -c value/array.cpp
        clang++ "''${clangFlags[@]}" -c value/pod.cpp

        clang++ "''${clangFlags[@]}" *.o
    '';
    installPhase = ''
        mkdir --parents $out/bin
        mv a.out $out/bin
    '';
}
