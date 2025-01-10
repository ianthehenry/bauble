/// <reference types="emscripten" />

declare module 'bauble-runtime' {
  export interface Value {
    vec2: number[2],
    vec3: number[3],
  }

  export interface Uniform {
    name: string,
    type: string,
    value: Value,
  }

  export interface UniformVector {
    get: (i: number) => Uniform,
    size: () => number,
    delete: () => void,
  }

  export interface EvaluationResult {
    isError: boolean,
    shaderSource: string,
    dimensions: number,
    isAnimated: boolean,
    hasCustomCamera: boolean,
    uniforms: UniformVector,
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
    evaluateScript: (script: string, renderType: number, crosshairs: boolean, dynamicCamera: boolean) => EvaluationResult;
    getDefinitions: () => DefinitionVector;
    // TODO, obviosly
    FS: WhyDoesTypescriptAllowGarbageHere;
  }

  const baubleFactory: EmscriptenModuleFactory<BaubleModule>;
  export default baubleFactory;
}
