import {basicSetup} from "codemirror"
import {EditorView, keymap, ViewUpdate} from "@codemirror/view"
import {indentWithTab} from "@codemirror/commands"
import {syntaxTree} from "@codemirror/language"
import {SyntaxNode} from "@lezer/common"
import {janet} from "codemirror-lang-janet"
import {EditorState, StateCommand, EditorSelection, SelectionRange, findClusterBreak} from "@codemirror/state"

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

interface MyEmscripten extends EmscriptenModule {
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

function isNumberNode(node: SyntaxNode) {
  return node.type.name === 'Number';
}

const incrementNumber: StateCommand = ({state, dispatch}) => {
  const range = state.selection.ranges[state.selection.mainIndex];
  const tree = syntaxTree(state);

  let node = tree.resolveInner(range.head, -1);
  if (!isNumberNode(node)) {
    node = tree.resolveInner(range.head, 1);
  }
  if (!isNumberNode(node)) {
    return false;
  }

  // TODO: we shouldn't be doing any floating point math; we should
  // parse this as a decimal number and increment it as a decimal number
  const numberText = state.sliceDoc(node.from, node.to);
  const number = Number(numberText);
  if (isNaN(number)) {
    console.error('unable to parse number: ', numberText);
    return false;
  }

  const newNumber = number + 1;
  const newNumberText = newNumber.toString();

  const lengthDifference = newNumberText.length - numberText.length;

  dispatch(state.update({
    changes: {
      from: node.from,
      to: node.to,
      insert: newNumberText,
    },
    selection: EditorSelection.single(node.from, node.to + lengthDifference),
    scrollIntoView: true,
    userEvent: "increment"
  }));
  return true;
}

document.addEventListener("DOMContentLoaded", function (e) {
  function runCode() {
    setTimeout(function() {
      clear();
      executeJanet(editor.state.doc.toString());
    }, 0);
  }
  const editor = new EditorView({
    extensions: [
      basicSetup,
      janet(),
      keymap.of([
        indentWithTab,
        { key: "Alt-h", run: incrementNumber },
      ]),
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
