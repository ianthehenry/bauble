import {basicSetup} from 'codemirror';
import {EditorView, keymap, ViewUpdate} from '@codemirror/view';
import {indentWithTab} from '@codemirror/commands';
import {syntaxTree} from '@codemirror/language';
import {SyntaxNode} from '@lezer/common';
import {janet} from 'codemirror-lang-janet';
import {EditorState, EditorSelection, Transaction} from '@codemirror/state';
import Big from 'big.js';
import * as Storage from './storage';

function save({state}: StateCommandInput) {
  const script = state.doc.toString();
  if (script.trim().length > 0) {
    Storage.saveScript(script);
  } else {
    Storage.deleteScript();
  }
  return true;
}

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

const incrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('1'));
const decrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('-1'));

export default function installCodeMirror(script: string, parent: HTMLElement, onChange: (() => void)): EditorView {
  const editor = new EditorView({
    extensions: [
      basicSetup,
      janet(),
      keymap.of([
        indentWithTab,
        { key: "Alt-h", run: incrementNumber, shift: decrementNumber },
        { key: "Mod-s", run: save },
      ]),
      EditorView.updateListener.of(function(viewUpdate: ViewUpdate) {
        if (viewUpdate.docChanged) {
          onChange();
        }
      }),
    ],
    parent: parent,
    doc: script,
  });

  let ctrlClickedAt = 0;
  const isTryingToEngageNumberDrag = () => {
    return performance.now() - ctrlClickedAt < 100;
  };

  parent.addEventListener('pointerdown', (e) => {
    if ((e.buttons === 1 || e.buttons === 2) && e.ctrlKey) {
      ctrlClickedAt = performance.now();
      parent.setPointerCapture(e.pointerId);
      e.preventDefault();
    }
  });
  parent.addEventListener('contextmenu', (e) => {
    if (isTryingToEngageNumberDrag()) {
      e.preventDefault();
    }
  });
  parent.addEventListener('pointermove', (e) => {
    if (parent.hasPointerCapture(e.pointerId)) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  // There is a bug in Firefox where ctrl-click fires as
  // a pointermove event instead of a pointerdown event,
  // and then will not respect setPointerCapture() when
  // called from the pointermove event.
  //
  // https://bugzilla.mozilla.org/show_bug.cgi?id=1504210
  //
  // So on Firefox you have to use an actual right-click.
  // It's very annoying. This is an *okay* workaround.

  // TODO: What if we have multiple codemirror instances?
  // should check if things are focused
  document.addEventListener('pointermove', (e) => {
    if (e.shiftKey && e.metaKey) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  document.addEventListener('pagehide', (_e) => {
    save(editor);
  });
  let savedBefore = false;
  // iOS Safari doesn't support beforeunload,
  // but it does support unload.
  window.addEventListener('beforeunload', (_e) => {
    savedBefore = true;
    save(editor);
  });
  window.addEventListener('unload', (_e) => {
    if (!savedBefore) {
      save(editor);
    }
  });

  return editor;
}
