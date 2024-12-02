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
    if (document.readyState === 'loading') {
      document.addEventListener("DOMContentLoaded", (_: any) => { resolve(); }, {once: true});
    } else {
      resolve();
    }
  });

  const wasmBox = await newMailbox(WhichWorker.Wasm);
  const definitions = await wasmBox.send({tag: 'definitions'}) as Array<Definition>;

  const intersectionObserver = new IntersectionObserver(async (entries) => {
    for (const entry of entries) {
      if (!entry.isIntersecting) {
        continue;
      }
      const placeholder = entry.target;
      intersectionObserver.unobserve(entry.target);
      const renderBox = await newMailbox(WhichWorker.Render);
      const initialScript = placeholder.textContent ?? '';
      const container = document.createElement('div');
      container.classList.add('bauble-root');
      placeholder.replaceWith(container);
      renderSolid(() =>
        <Bauble
          definitions={definitions}
          wasmBox={wasmBox}
          renderBox={renderBox}
          initialScript={initialScript}
          focusable={true}
          canSave={false}
          canSearch={false}
          canExport={false}
          showLineGutter={false}
          size={{width: 384, height: 384}}
        />, container);
    }
  });
  for (const placeholder of document.querySelectorAll('pre:has(code.language-bauble)')) {
    intersectionObserver.observe(placeholder);
  }
}})();
