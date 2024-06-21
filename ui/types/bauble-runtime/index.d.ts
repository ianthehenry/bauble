/// <reference types="emscripten" />

declare module 'bauble-runtime' {
  export interface EvaluationResult {
    isError: boolean,
    shaderSource: string,
    isAnimated: boolean,
    error: string,
  }

  export interface Definition {
    name: string,
    args: string,
    doc: string,
    type: number,
  }

  export interface DefinitionVector {
    get: (i: number) => Definition,
    size: () => number,
    delete: () => void,
  }

  export interface BaubleModule extends EmscriptenModule {
    evaluateScript: (_: string) => EvaluationResult;
    getDefinitions: () => DefinitionVector;
    // TODO, obviosly
    FS: WhyDoesTypescriptAllowGarbageHere;
  }

  const baubleFactory: EmscriptenModuleFactory<BaubleModule>;
  export default baubleFactory;
}
