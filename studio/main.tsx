import * as Storage from './storage';
import Bauble from './bauble';
import { render as renderSolid } from 'solid-js/web';
import InitializeWasm from 'bauble-runtime';
import type {BaubleModule, Definition} from 'bauble-runtime';
import * as WasmWorker from './wasm-worker';
import * as RenderWorker from './render-worker';
import Mailbox from './mailbox';

// @ts-ignore
const inWorker = typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope;

const getAck = (worker: Worker) => {
  return new Promise<void>((resolve) => {
    worker.addEventListener('message', ((_: any) => { resolve(); }), {once: true})
  });
};

const newMailbox = async (which: WhichWorker) => {
  const worker = new Worker(import.meta.url);
  await getAck(worker);
  const doneBecoming = getAck(worker);
  worker.postMessage(which);
  await doneBecoming;
  return new Mailbox(worker);
};

enum WhichWorker {
  Wasm,
  Render,
}

(async function() {

if (inWorker) {
  const which = new Promise((resolve) => {
    self.addEventListener('message', ((e) => { resolve(e.data as WhichWorker); }), {once: true});
  });
  self.postMessage('ready');
  switch (await which) {
  case WhichWorker.Wasm: await WasmWorker.init(); break;
  case WhichWorker.Render: await RenderWorker.init(); break;
  }
  self.postMessage('ready');
} else {
  await new Promise<void>((resolve) => {
    document.addEventListener("DOMContentLoaded", (_: any) => { resolve(); }, {once: true});
  });

  const wasmBox = await newMailbox(WhichWorker.Wasm);
  const renderBox = await newMailbox(WhichWorker.Render);

  const definitions = await wasmBox.send({tag: 'definitions'}) as Array<Definition>;

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
            wasmBox={wasmBox}
            renderBox={renderBox}
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
    const initialScript = Storage.getScript() ?? await (wasmBox.send({tag: 'read-file', 'path': 'examples/intro.janet'}) as Promise<string>);
    renderSolid(() => <Bauble
      definitions={definitions}
      wasmBox={wasmBox}
      renderBox={renderBox}
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
