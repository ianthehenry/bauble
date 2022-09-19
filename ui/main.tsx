import * as Storage from './storage';
import Bauble from './bauble';
import { render as renderSolid } from 'solid-js/web';
import InitializeWasm from 'bauble-runtime';
import type {BaubleModule} from 'bauble-runtime';
import OutputChannel from './output-channel';

document.addEventListener("DOMContentLoaded", (_) => {
  const outputChannel = new OutputChannel();
  const baubleOpts = {
    print: (x: string) => {
      outputChannel.print(x, false);
    },
    printErr: (x: string) => {
      outputChannel.print(x, true);
    },
  };

  switch (window.location.pathname) {
    case '/help/': {
      InitializeWasm(baubleOpts).then((runtime: BaubleModule) => {
        for (const placeholder of document.querySelectorAll('.bauble-placeholder')) {
          const initialScript = placeholder.textContent ?? '';
          placeholder.innerHTML = '';
          renderSolid(() =>
            <Bauble
              runtime={runtime}
              outputChannel={outputChannel}
              initialScript={initialScript}
              focusable={true}
              canSave={false}
              size={{width: 256, height: 256}}
            />, placeholder);
        }
      }).catch(console.error);
      break;
    }
    case '/': {
      InitializeWasm(baubleOpts).then((runtime: BaubleModule) => {
        const initialScript = Storage.getScript() ?? runtime.FS.readFile('intro.janet', {encoding: 'utf8'});
        renderSolid(() => <Bauble
          runtime={runtime}
          outputChannel={outputChannel}
          initialScript={initialScript}
          focusable={false}
          canSave={true}
          size={{width: 512, height: 512}}
        />, document.body);
      }).catch(console.error);
      break;
    }
  }
});
