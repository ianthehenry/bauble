import {basicSetup} from "codemirror"
import {EditorView, keymap, ViewUpdate} from "@codemirror/view"
import {indentWithTab} from "@codemirror/commands"
import {janet} from "codemirror-lang-janet"

function clear() {
  const output = document.getElementById('output')!;
  output.innerHTML = "";
}

function print(text: string, isErr=false) {
  const output = document.getElementById('output')!;
  const span = document.createElement('span');
  span.classList.toggle('err', isErr);
  span.appendChild(document.createTextNode(text));
  span.appendChild(document.createTextNode('\n'));
  output.appendChild(span);
  output.scrollTop = output.scrollHeight;
}

let evaluateJanet: ((code:string) => number) | null = null;
let ready = function() {};

function onReady(f: (() => void)) {
  if (ready == null) {
    f();
  } else {
    const old = ready;
    ready = function() {
      old();
      f();
    };  
  }
}

const preamble = '(use ./shapes)\n';

function executeJanet(code: string) {
  if (evaluateJanet === null) {
    console.error('not ready yet');
    return;
  }
  const result = evaluateJanet(preamble + code);
  if (result !== 0) {
    print('ERREXIT: ' + result, true);
  }
}

export interface MyEmscripten extends EmscriptenModule {
  cwrap: typeof cwrap;
}

const Module: Partial<MyEmscripten> = {
  preRun: [],
  print: function(x: string) {
    print(x, false);
  },
  printErr: function(x: string) {
    print(x, true);
  },
  postRun: [function() {
    evaluateJanet = Module.cwrap!("run_janet", 'number', ['string']);
    ready();
  }],
};

document.addEventListener("DOMContentLoaded", function (e) {
  function runCode() {
    setTimeout(function() {
      clear();
      executeJanet(editor.state.doc.toString());
    }, 0);
  }
  // console.log(janet());
  const editor = new EditorView({
    extensions: [
      basicSetup,
      janet(),
      keymap.of([indentWithTab]),
      EditorView.updateListener.of(function(viewUpdate: ViewUpdate) {
        if (viewUpdate.docChanged) {
          runCode();
        }
      }),
    ],
    parent: document.getElementById('editor-container')!,
    doc: `(def monolith
  (->> (box [20 20 50])
    (rotate-y (tau 0.125))
    (rotate-z (tau 0.125))
    (translate [20 0 50])))

(def orb (->> (sphere 50) (translate [20 0 0])))

(union monolith orb)
`
  });

  onReady(runCode);
  editor.focus();
  // editor.navigateFileEnd();
});

(<any> window).Module = Module;
