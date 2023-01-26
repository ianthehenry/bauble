with import <nixpkgs> {};

mkShell {
  nativeBuildInputs = [
    emscripten
    yarn
    pandoc
    nodejs
    redo-apenwarr
  ];
}
