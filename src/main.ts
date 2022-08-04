import {basicSetup} from 'codemirror';
import {EditorView, keymap, ViewUpdate} from '@codemirror/view';
import {indentWithTab, cursorDocEnd} from '@codemirror/commands';
import {syntaxTree} from '@codemirror/language';
import {SyntaxNode} from '@lezer/common';
import {janet} from 'codemirror-lang-janet';
import {
  EditorState, EditorSelection, Transaction,
} from '@codemirror/state';
import Big from 'big.js';

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

let evaluateJanet: ((_: string) => number) | null = null;
let ready: (() => void) | null = function() { ready = null; };

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

const initialScript = `
(def top (cylinder :z 12 10))

(def body 
  (->> (sphere 100)
    (morph 0.9 (sphere 90 [0 0 -20]))
    (smooth-union 92 (sphere -17 [0 0 -130]))
    (smooth-union 10 (translate [0 0 100] top))))

(def hat
  (smooth-union 10
    (smooth-subtract 2
      (onion 1 top)
      (half-space :-z))
    (cylinder :z 1 30)))

(union
  body
  (translate [0 0 107] hat))
`.trimLeft();

const preamble = '(use ./shapes)\n';

function executeJanet(code: string) {
  if (evaluateJanet === null) {
    console.error('not ready yet');
    return;
  }
  const result = evaluateJanet(preamble + code);
  if (result !== 0) {
    print('ERREXIT: ' + result.toString(), true);
  }
}

interface MyEmscripten extends EmscriptenModule {
  cwrap: typeof cwrap;
}

declare global {
  interface Window { Module: Partial<MyEmscripten>; }
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

interface StateCommandInput {state: EditorState, dispatch: (_: Transaction) => void}

function alterNumber({state, dispatch}: StateCommandInput, amount: Big) {
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
  let number;
  try {
    number = Big(numberText);
  } catch (e) {
    console.error('unable to parse number: ', numberText);
    return false;
  }
  const decimalPointIndex = numberText.indexOf('.');
  const digitsAfterDecimalPoint = decimalPointIndex < 0 ? 0 : numberText.length - decimalPointIndex - 1;
  const increment = Big('10').pow(-digitsAfterDecimalPoint);

  const newNumber = number.add(amount.times(increment));
  const newNumberText = newNumber.toFixed(digitsAfterDecimalPoint);

  const lengthDifference = newNumberText.length - numberText.length;

  dispatch(state.update({
    changes: {
      from: node.from,
      to: node.to,
      insert: newNumberText,
    },
    selection: EditorSelection.single(node.from, node.to + lengthDifference),
    scrollIntoView: true,
    userEvent: 'increment',
  }));
  return true;
}

document.addEventListener("DOMContentLoaded", (_) => {
  function runCode() {
    setTimeout(function() {
      clear();
      executeJanet(editor.state.doc.toString());
    }, 0);
  }

  const incrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('1'));
  const decrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('-1'));

  const script = localStorage.getItem('script') ?? initialScript;

  const editor = new EditorView({
    extensions: [
      basicSetup,
      janet(),
      keymap.of([
        indentWithTab,
        { key: "Alt-h", run: incrementNumber, shift: decrementNumber },
      ]),
      EditorView.updateListener.of(function(viewUpdate: ViewUpdate) {
        if (viewUpdate.docChanged) {
          runCode();
        }
      }),
    ],
    parent: document.getElementById('editor-container')!,
    doc: script,
  });

  // honestly this is so annoying on firefox that
  // i'm not even gonna bother
  const usePointerLock = false;

  if (usePointerLock) {
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Control') {
        document.body.requestPointerLock();
      }
    });
    document.addEventListener('keyup', (e) => {
      if (e.key === 'Control') {
        document.exitPointerLock();
      }
    });
  }

  document.body.addEventListener('pointermove', (e) => {
    if (e.ctrlKey) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  window.addEventListener('beforeunload', (_e) => {
    const script = editor.state.doc.toString();
    if (script.trim().length > 0) {
      localStorage.setItem('script', script);
    } else {
      localStorage.removeItem('script');
    }
  });

  onReady(runCode);
  editor.focus();
  cursorDocEnd(editor);
});

window.Module = Module;
