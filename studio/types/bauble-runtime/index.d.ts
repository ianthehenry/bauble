/// <reference types="emscripten" />

declare module 'bauble-runtime' {
  export interface EvaluationResult {
    isError: boolean,
    shaderSource: string,
    dimension: number,
    isAnimated: boolean,
    hasCustomCamera: boolean,
    error: string,
    evalTimeMs: number,
    compileTimeMs: number,
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
    evaluateScript: (script: string, renderType: number) => EvaluationResult;
    getDefinitions: () => DefinitionVector;
    // TODO, obviosly
    FS: WhyDoesTypescriptAllowGarbageHere;
  }

  const baubleFactory: EmscriptenModuleFactory<BaubleModule>;
  export default baubleFactory;
}
