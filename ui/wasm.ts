export {};

export interface EvaluationResult {
  isError: boolean,
  shaderSource: string,
  isAnimated: boolean,
  error: string,
}

export interface Emscripten extends EmscriptenModule {
  evaluate_script: ((_: string) => EvaluationResult);
  outputTarget?: HTMLElement;
}

declare global {
  interface Window {
    Module: Partial<Emscripten>;
  }
}
