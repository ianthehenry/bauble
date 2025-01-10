import InitializeWasm from 'bauble-runtime';
import type {BaubleModule} from 'bauble-runtime';

export function init() {
  let outputs: Array<[string, boolean]>;
  return InitializeWasm({
    print: (x: string) => { console.log(x); outputs.push([x, false]); },
    printErr: (x: string) => { console.error(x); outputs.push([x, true]); },
  }).then((runtime: BaubleModule) => {
    const getResponse = (request : any) => {
      // TODO: enum
      switch (request.tag) {
      case 'compile': {
        outputs = [];
        const result = runtime.evaluateScript(request.script, request.renderType, request.crosshairs, request.dynamicCamera) as any;
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
        return result;
      }
      case 'definitions': {
        const definitionVector = runtime.getDefinitions();
        const definitions = [];
        for (let i = 0; i < definitionVector.size(); i++) {
          definitions.push(definitionVector.get(i));
        }
        definitionVector.delete();
        return definitions;
      }
      case 'read-file': {
        return runtime.FS.readFile(request.path, {encoding: 'utf8'});
      }
      default: throw new Error("unknown request tag " + request.tag);
      }
    };

    self.addEventListener('message', (event) => {
      self.postMessage({
        id: event.data.id,
        response: getResponse(event.data.request),
      });
    });
  });
}
