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
              hijackScroll={false}
              canSave={false}
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
          hijackScroll={true}
          canSave={true}
        />, document.body);
      }).catch(console.error);
      break;
    }
  }
});
