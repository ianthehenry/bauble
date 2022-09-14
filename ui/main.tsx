import * as Storage from './storage';
import installCodeMirror from './editor';
import Renderer from './renderer';
// import {Timer, LoopMode, TimerState} from './timer'
import {mod, clamp, TAU} from './util'
import {Emscripten} from './wasm'
import Bauble from './bauble';
import { render as renderSolid } from "solid-js/web";

function print(text: string, isErr=false) {
  if (window.Module.outputTarget == null) {
    if (isErr) {
      console.error(text);
    } else {
      console.log(text);
    }
  } else {
    const span = document.createElement('span');
    span.classList.toggle('err', isErr);
    span.appendChild(document.createTextNode(text));
    span.appendChild(document.createTextNode('\n'));
    window.Module.outputTarget.appendChild(span);
  }
}

let resolveReady: (_: undefined) => void;
const wasmReady = new Promise((x) => { resolveReady = x; });

window.Module = {
  preRun: [],
  print: function(x: string) {
    print(x, false);
  },
  printErr: function(x: string) {
    print(x, true);
  },
  postRun: [function() {
    resolveReady(void(0));
  }],
  locateFile: function(path, prefix) {
    if (prefix === '') {
      return '/js/' + path;
    } else {
      return prefix + path;
    }
  },
};

document.addEventListener("DOMContentLoaded", (_) => {
  switch (window.location.pathname) {
    case '/help/': {
      wasmReady.then(() => {
        for (const placeholder of document.querySelectorAll('.bauble-placeholder')) {
          const initialScript = placeholder.textContent ?? '';
          placeholder.innerHTML = '';
          renderSolid(() => <Bauble initialScript={initialScript} hijackScroll={false} canSave={false} />, placeholder);
        }
      }).catch(console.error);
      break;
    }
    case '/': {
      wasmReady.then(() => {
        const initialScript = Storage.getScript() ?? FS.readFile('intro.janet', {encoding: 'utf8'});
        renderSolid(() => <Bauble initialScript={initialScript} hijackScroll={true} canSave={true} />, document.body);
      }).catch(console.error);
      break;
    }
  }
});
