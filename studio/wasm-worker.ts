import InitializeWasm from 'bauble-runtime';
import type {BaubleModule} from 'bauble-runtime';

export function init() {
  let outputs: Array<[string, boolean]>;
  InitializeWasm({
    print: (x: string) => { console.log(x); outputs.push([x, false]); },
    printErr: (x: string) => { console.error(x); outputs.push([x, true]); },
  }).then((runtime: BaubleModule) => {
    const getResponse = (request : any) => {
      // TODO: enum
      switch (request.tag) {
      case 'compile': {
        outputs = [];
        const {isError, error, isAnimated, shaderSource} = runtime.evaluateScript(request.script);
        return {
          outputs: outputs,
          isError: isError,
          error: error,
          isAnimated: isAnimated,
          shaderSource: shaderSource,
        };
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
      default: throw new Error("unknown request", request);
      }
    };

    self.addEventListener('message', (event) => {
      self.postMessage({
        id: event.data.id,
        response: getResponse(event.data.request),
      });
    });
    self.postMessage('ready');
  });
}
