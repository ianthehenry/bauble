/// <reference types="emscripten" />

declare module 'bauble-runtime' {
  export interface EvaluationResult {
    isError: boolean,
    shaderSource: string,
    isAnimated: boolean,
    error: string,
  }

  export interface BaubleModule extends EmscriptenModule {
    evaluate_script: ((_: string) => EvaluationResult);
    // TODO, obviosly
    FS: WhyDoesTypescriptAllowGarbageHere;
  }

  const baubleFactory: EmscriptenModuleFactory<BaubleModule>;
  export default baubleFactory;
}
