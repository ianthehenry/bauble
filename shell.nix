with import <nixpkgs> {};

let frameworks =
  with darwin.apple_sdk.frameworks; [
    AudioToolbox
    Carbon
    Cocoa
    CoreAudio
    Foundation
    IOKit
    Kernel
    OpenGL
  ];
in
mkShell {
  nativeBuildInputs = [
    darwin.libobjc
    emscripten
    yarn
    pandoc
    nodejs
    redo-apenwarr
    coreutils
  ] ++ frameworks;
}
