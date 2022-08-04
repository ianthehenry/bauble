with import <nixpkgs> {};

mkShell {
  nativeBuildInputs = [ emscripten yarn ];
}
