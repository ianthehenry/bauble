import {EditorView, ViewUpdate} from '@codemirror/view';
import {indentWithTab} from '@codemirror/commands';
import {syntaxTree, HighlightStyle} from '@codemirror/language';
import {SyntaxNode} from '@lezer/common';
import {tags} from '@lezer/highlight';
import {janet} from 'codemirror-lang-janet';
import {EditorSelection, Transaction} from '@codemirror/state';
import type {Definition} from 'bauble-runtime';
import Big from 'big.js';
import * as Storage from './storage';
import janetAutocomplete from "./autocomplete";

/////////////////////////////////////////////////////////////////////////////////////
// https://github.com/codemirror/basic-setup/blob/main/src/codemirror.ts
// copied here so that we can optionally exclude searchKeyMap
import {keymap, highlightSpecialChars, drawSelection, highlightActiveLine, dropCursor,
        rectangularSelection, crosshairCursor,
        lineNumbers, highlightActiveLineGutter} from "@codemirror/view"
import {Extension, EditorState} from "@codemirror/state"
import {defaultHighlightStyle, syntaxHighlighting, indentOnInput, bracketMatching,
        foldGutter, foldKeymap} from "@codemirror/language"
import {defaultKeymap, history, historyKeymap} from "@codemirror/commands"
import {searchKeymap, highlightSelectionMatches} from "@codemirror/search"
import {autocompletion, completionKeymap, closeBrackets, closeBracketsKeymap} from "@codemirror/autocomplete"
import {lintKeymap} from "@codemirror/lint"
const basicSetup = (enableSearch: boolean) => [
  lineNumbers(),
  highlightActiveLineGutter(),
  highlightSpecialChars(),
  history(),
  foldGutter(),
  drawSelection(),
  dropCursor(),
  EditorState.allowMultipleSelections.of(true),
  indentOnInput(),
  syntaxHighlighting(defaultHighlightStyle, {fallback: true}),
  bracketMatching(),
  closeBrackets(),
  autocompletion(),
  rectangularSelection(),
  crosshairCursor(),
  highlightActiveLine(),
  highlightSelectionMatches(),
  keymap.of([
    ...closeBracketsKeymap,
    ...defaultKeymap,
    ...(enableSearch ? searchKeymap : []),
    ...historyKeymap,
    ...foldKeymap,
    ...completionKeymap,
    ...lintKeymap
  ])
];
/////////////////////////////////////////////////////////////////////////////////////

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
  }));
  return true;
}

interface EditorOptions {
  initialScript: string,
  parent: HTMLElement,
  canSave: boolean,
  canSearch: boolean,
  onChange: (() => void),
  definitions: Array<Definition>,
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
    backgroundColor: 'initial',
  },
  "&.cm-focused .cm-activeLine": {
    // TODO: this breaks selection highlighting, which is crazy
    // backgroundColor: 'var(--line)',
  },
  ".cm-activeLineGutter": {
    backgroundColor: 'initial',
  },
  "&.cm-focused .cm-activeLineGutter": {
    backgroundColor: 'var(--selection)',
  },
  ".cm-selectionMatch": {
    outline: 'solid 1px var(--comment)',
    borderRadius: '2px',
    backgroundColor: 'initial',
  },
  "&.cm-focused .cm-matchingBracket": {
    outline: 'solid 1px var(--green)',
    borderRadius: '2px',
    color: 'var(--green)',
    backgroundColor: 'initial',
  },
  "&.cm-focused .cm-nonmatchingBracket": {
    outline: 'solid 1px var(--red)',
    borderRadius: '2px',
    color: 'var(--red)',
    backgroundColor: 'initial',
  },
  // slightly subtler as you type; i dunno
  // "&.cm-focused .cm-activeLine .cm-matchingBracket": {
  //   outline: 'none',
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
  ".cm-selectionBackground, ::selection": {
    backgroundColor: 'var(--selection) !important',
  },
  ".cm-gutters": {
    backgroundColor: 'var(--line)',
    color: 'var(--comment)',
    border: "none",
  },
  ".cm-tooltip": {
    backgroundColor: 'var(--popover-background)',
    backdropFilter: 'var(--popover-backdrop)',
    color: 'var(--foreground)',
    borderRadius: '2px',
  },
  ".cm-tooltip-autocomplete ul li[aria-selected]": {
    backgroundColor: 'var(--selection)',
    color: 'var(--foreground)',
  },
  ".cm-tooltip-autocomplete ul li[aria-selected]:first-child": {
    borderTopLeftRadius: '2px',
    borderTopRightRadius: '2px',
  },
  ".cm-tooltip-autocomplete ul li[aria-selected]:last-child": {
    borderBottomLeftRadius: '2px',
    borderBottomRightRadius: '2px',
  },
  ".cm-tooltip.cm-completionInfo": {
    maxHeight: '10.5em',
    overflowY: 'auto',
    backgroundColor: 'var(--popover-background)',
    backdropFilter: 'var(--popover-backdrop)',
    borderRadius: '2px',
  },
  ".cm-tooltip.cm-completionInfo > div": {
    display: 'flex',
    flexDirection: 'column',
    gap: '0.5em',
  },
  // TODO: style the "find/replace" box
});

export default function installCodeMirror({definitions, initialScript, parent, canSave, canSearch, onChange}: EditorOptions): EditorView {
  const keyBindings = [indentWithTab];
  if (canSave) {
    keyBindings.push({ key: "Mod-s", run: save });
  }

  const editor = new EditorView({
    extensions: [
      basicSetup(canSearch),
      janet(),
      keymap.of(keyBindings),
      EditorView.updateListener.of(function(viewUpdate: ViewUpdate) {
        if (viewUpdate.docChanged) {
          onChange();
        }
      }),
      janetAutocomplete(definitions),
      autocompletion(),
      // tooltips({
      //   parent: document.body,
      //   tooltipSpace: (view: EditorView) => {
      //     return {
      //       left: 0,
      //       right: window.innerWidth,
      //       top: 100,
      //       bottom: window.innerHeight - 20,
      //     };
      //   }
      // }),
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
