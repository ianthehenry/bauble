import {basicSetup} from 'codemirror';
import {EditorView, keymap, ViewUpdate} from '@codemirror/view';
import {indentWithTab} from '@codemirror/commands';
import {syntaxTree, syntaxHighlighting, HighlightStyle} from '@codemirror/language';
import {SyntaxNode} from '@lezer/common';
import {tags} from '@lezer/highlight';
import {janet} from 'codemirror-lang-janet';
import {EditorState, EditorSelection, Transaction} from '@codemirror/state';
import Big from 'big.js';
import * as Storage from './storage';

function save({state}: StateCommandInput) {
  console.log('saving...');
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
    userEvent: 'alterNumber',
  }));
  return true;
}

interface EditorOptions {
  initialScript: string,
  parent: HTMLElement,
  canSave: boolean,
  onChange: (() => void),
}

const highlightStyle = HighlightStyle.define([
  {tag: tags.keyword, color: 'var(--purple)'},
  {tag: tags.atom, color: 'var(--foreground)'},
  {tag: tags.number, color: 'var(--blue)'},
  {tag: tags.comment, color: 'var(--comment)'},
  {tag: tags.null, color: 'var(--purple)'},
  {tag: tags.bool, color: 'var(--purple)'},
  {tag: tags.string, color: 'var(--green)'},
]);

const theme = EditorView.theme({
  "&": {
    color: 'var(--foreground)',
    backgroundColor: 'var(--background)',
  },
  ".cm-content": {
    padding: '0',
    caretColor: 'var(--foreground)',
  },
  ".cm-cursor": {
    borderLeftColor: 'var(--foreground)',
  },
  ".cm-activeLine": {
    backgroundColor: 'var(--line)',
  },
  ".cm-activeLineGutter": {
    backgroundColor: 'var(--background)',
  },
  ".cm-selectionMatch": {
    outline: 'solid 1px var(--comment)',
    borderRadius: '2px',
    backgroundColor: 'initial',
  },
  "&.cm-focused .cm-matchingBracket": {
    backgroundColor: 'var(--green)',
    color: 'var(--background)',
  },
  // slightly subtler as you type; looks a little weird though
  // "&.cm-focused .cm-activeLine .cm-matchingBracket" {
  //   backgroundColor: 'initial',
  //   color: 'var(--green)',
  // },
  ".cm-foldPlaceholder": {
    outline: 'solid 1px var(--comment)',
    border: 'none',
    width: '2ch',
    display: 'inline-block',
    margin: '0',
    padding: '0',
    textAlign: 'center',
    borderRadius: '2px',
    backgroundColor: 'var(--background)',
    color: 'var(--comment)',
  },
  "&.cm-focused .cm-selectionBackground, ::selection": {
    backgroundColor: 'var(--selection)',
  },
  ".cm-gutters": {
    backgroundColor: 'var(--line)',
    color: 'var(--comment)',
    border: "none",
  },
  // TODO: style the "find/replace" box
});

export default function installCodeMirror({initialScript, parent, canSave, onChange}: EditorOptions): EditorView {
  const keyBindings = [indentWithTab];
  if (canSave) {
    keyBindings.push({ key: "Mod-s", run: save });
  }

  const editor = new EditorView({
    extensions: [
      basicSetup,
      janet(),
      keymap.of(keyBindings),
      EditorView.updateListener.of(function(viewUpdate: ViewUpdate) {
        if (viewUpdate.docChanged) {
          onChange();
        }
      }),
      theme,
      syntaxHighlighting(highlightStyle),
    ],
    parent: parent,
    doc: initialScript,
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
  document.addEventListener('pointermove', (e) => {
    if (!editor.hasFocus) {
      return;
    }
    if (e.shiftKey && e.metaKey) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  if (canSave) {
    setInterval(function() {
      save(editor);
    }, 30 * 1000);
    document.addEventListener('pagehide', () => {
      save(editor);
    });
    let savedBefore = false;
    // iOS Safari doesn't support beforeunload,
    // but it does support unload.
    window.addEventListener('beforeunload', () => {
      savedBefore = true;
      save(editor);
    });
    window.addEventListener('unload', () => {
      if (!savedBefore) {
        save(editor);
      }
    });
  }

  return editor;
}
