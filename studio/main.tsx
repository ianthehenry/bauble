import * as Storage from './storage';
import Bauble from './bauble';
import { render as renderSolid } from 'solid-js/web';
import InitializeWasm from 'bauble-runtime';
import type {BaubleModule, Definition} from 'bauble-runtime';
import * as WasmWorker from './wasm-worker';
import Mailbox from './mailbox';

// @ts-ignore
const inWorker = typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope;

(async function() {

if (inWorker) {
  WasmWorker.init();
} else {
  await new Promise<void>((resolve) => {
    document.addEventListener("DOMContentLoaded", (_) => { resolve(); }, {once: true});
  });

  const worker = await new Promise<Worker>((resolve) => {
    const worker = new Worker(import.meta.url);
    worker.addEventListener('message', ((_) => { resolve(worker); }), {once: true});
  });
  const wasmMailbox = new Mailbox(worker);

  const definitions = await wasmMailbox.send({tag: 'definitions'}) as Array<Definition>;

  switch (window.location.pathname) {
  case '/help/': {
    const intersectionObserver = new IntersectionObserver((entries) => {
      for (const entry of entries) {
        if (!entry.isIntersecting) {
          continue;
        }
        const placeholder = entry.target;
        intersectionObserver.unobserve(entry.target);
        const initialScript = placeholder.textContent ?? '';
        placeholder.innerHTML = '';
        renderSolid(() =>
          <Bauble
            definitions={definitions}
            compiler={wasmMailbox}
            initialScript={initialScript}
            focusable={true}
            canSave={false}
            size={{width: 256, height: 256}}
          />, placeholder);
      }
    });
    for (const placeholder of document.querySelectorAll('.bauble-placeholder')) {
      intersectionObserver.observe(placeholder);
    }
    break;
  }
  case '/': {
    const initialScript = Storage.getScript() ?? await (wasmMailbox.send({tag: 'read-file', 'path': 'examples/intro.janet'}) as Promise<string>);
    renderSolid(() => <Bauble
      definitions={definitions}
      compiler={wasmMailbox}
      initialScript={initialScript}
      focusable={false}
      canSave={true}
      size={{width: 512, height: 512}}
    />, document.body);
  break;
  }
  }
}
})();
