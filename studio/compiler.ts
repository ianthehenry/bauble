import InitializeWasm from 'bauble-runtime';
import type {BaubleModule} from 'bauble-runtime';
import {RenderType} from 'types';
import convertCompilerOutput from 'embed-helpers';

export function init() {
  let outputs: Array<[string, boolean]>;
  return InitializeWasm({
    print: (x: string) => { outputs.push([x, false]); },
    printErr: (x: string) => { outputs.push([x, true]); },
  }).then((runtime: BaubleModule) => (source: string) => {
    outputs = [];
    const result = runtime.evaluateScript(source, RenderType.Normal, false, false) as any;
    const uniforms = [];
    const uniformCount = result.uniforms.size();
    for (let i = 0; i < uniformCount; i++) {
      const uniform = result.uniforms.get(i);
      const value: {[key: string]: any} = {};
      value[uniform.type] = uniform.value[uniform.type];
      uniform.value = value;
      uniforms.push(uniform);
    }
    result.uniforms.delete();
    result.uniforms = uniforms;
    result.outputs = outputs;

    if (result.isError) {
      throw new Error("compilation error", {cause: result.error});
    } else {
      const [options, uniforms] = convertCompilerOutput(result);
      options.uniformValues = uniforms;
      return options;
    }
  });
}
